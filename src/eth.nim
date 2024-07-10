# The whole compiler
import myColoredLogger
import std/[logging, os, parseopt, sets, tables]
{.push used.}
type
  UId = uint64
  ObjectKind = enum
    okI1, okI2, okI4, okI8, okU1, okU2, okU4, okU8, okInt, okUInt,
      okFloat, okF4, okF8, okIdn
  Object = ref object
    case kind: ObjectKind
    of okI1, okI2, okI4, okI8, okU1, okU2, okU4, okU8, okInt, okUInt:
      ival: uint64
    of okFloat, okF4, okF8:
      fval: float64
      lastE: float=10.0
    of okIdn:
      name: string
      uid: UId
  State = enum
    stTop, stIPref0, stIPrefK, stI, stIBin, stIOct, stIHex, stIPsf, stIdn, stF, stFPsf
  CpEnv = ref object
    stk: seq[State]
    outStk: seq[Object]
    symTable:Table[string,Table[int,UId]]
    depth:int
    count:int
proc `$`(x:Object):string=
  case x.kind
  of okI1, okI2, okI4, okI8, okU1, okU2, okU4, okU8, okInt, okUInt:
    ($x.ival)&($x.kind)[2..^1]
  of okF4,okF8,okFloat:
    ($x.fval)&($x.kind)[2..^1]
  of okIdn:
    x.name&"@"&($x.uid)
proc newInt(i: SomeInteger): Object =
  new result
  result = Object(kind: okInt, ival: cast[uint64](i))
proc newFloat(i: SomeFloat): Object =
  new result
  result = Object(kind: okFloat, fval: float64(i))
proc newIdn(s:string):Object=
  new result
  result = Object(kind:okIdn,name:s)
var
  defines: HashSet[string]
  options: HashSet[string]
  keyOptions: Table[string, string]
  files: seq[string]
  consoleLogger: ConsoleLogger
  fileLogger: FileLogger
when isMainModule:
  consoleLogger = newColoredLogger(fmtStr = "$levelid: ",useStdErr=true)
  fileLogger = newFileLogger("log.log", fmAppend,
      fmtStr = "[$datetime] $levelid: ")
  addHandler(consoleLogger)
  addHandler(fileLogger)
  var cmdParser = initOptParser(os.commandLineParams())
  while true:
    cmdParser.next()
    case cmdParser.kind
    of cmdEnd:
      break
    of cmdShortOption, cmdLongOption:
      if cmdParser.kind == cmdShortOption:
        case cmdParser.key
        of "d": cmdParser.key = "define"
        of "r": cmdParser.key = "run"
        of "l": cmdParser.key = "log"
      if cmdParser.val == "":
        if options.containsOrIncl cmdParser.key:
          notice "Option already exists; ignoring."
      elif cmdParser.key == "define":
        if defines.containsOrIncl cmdParser.val:
          notice "Define already exists; ignoring."
      elif cmdParser.key == "log":
        fileLogger.file.close()
        fileLogger.file = open(cmdParser.val, fmAppend)
      else:
        if cmdParser.key in keyOptions:
          notice "Option already exists; covering."
        keyOptions[cmdParser.key] = cmdParser.val
    of cmdArgument:
      if files.len > 0:
        notice "More than one input; combining in order."
      files.add cmdParser.key
  var output{.used.} = ""
  for file in files:
    var f:File
    info "Trying to open file "&file
    try:
      f = open(file, fmRead)
    except IOError:
      error "Could not open file "&file&"; exiting"
      quit(QuitFailure)
    var cpEnv: CpEnv
    new cpEnv
    cpEnv.stk = newSeqOfCap[State](32)
    cpEnv.stk.add stTop
    template tops: State =
      cpEnv.stk[^1]
    template top: var Object =
      cpEnv.outStk[^1]
    proc push(o: Object) {.inline.} =
      cpEnv.outStk.add o
    proc pushs(s: State) {.inline.} =
      cpEnv.stk.add s
    proc pop(): Object {.inline, discardable.} =
      cpEnv.outStk.pop()
    proc pops(): State {.inline, discardable.} =
      cpEnv.stk.pop()
    template d2i(a: char): uint64 =
      uint64(a.int8-'0'.int8)
    template h2i(a: char): uint64 =
      uint64(if a in '0'..'9':
        a.int8-'0'.int8
      elif a in 'a'..'f':
        a.int8-'a'.int8+10
      elif a in 'A'..'F':
        a.int8-'A'.int8+10
      else:
        -1'i8)
    proc process(c: char) =
      proc invalidChar() =
        error "Unexpected char '" & ($c.int8) & "'; exiting."
        quit(QuitFailure)
      case tops
      of stTop:
        case c
        of ' ', '\n', '\r', '\t':
          return
        of '0':
          pushs stIPref0
        of '1'..'9':
          pushs stI
          push newInt(c.d2i)
        of 'A'..'Z', 'a'..'z':
          pushs stIdn
          push newIdn($c)
        else:
          invalidChar()
      of stIPref0:
        pops()
        case c
        of '0'..'9':
          pushs stI
          push newInt(c.d2i)
        of 'x', 'X', 'o', 'O', 'b', 'B':
          case c
          of 'x', 'X': pushs stIHex
          of 'o', 'O': pushs stIOct
          of 'b', 'B': pushs stIBin
          else:
            fatal "Impossible case; exiting"
            quit(QuitFailure)
          push newInt(0)
        of '.':
          pushs stF
          push newFloat(0.0)
        of 'f', 'F':
          pushs stFPsf
          push newFloat(0.0)
        else:
          push newInt(0)
          process c
      of stI:
        case c
        of '0'..'9':
          top.ival = top.ival*10+c.d2i
        of 'i', 'I':
          pops()
          pushs stIPsf
        of 'u', 'U':
          pops()
          top = Object(kind: okUInt, ival: top.ival)
          pushs stIPsf
        of '.':
          pops()
          pushs stF
          push newFloat(float64(pop().ival))
        of 'f', 'F':
          pops()
          pushs stFPsf
          push newFloat(float64(pop().ival))
        else:
          pops()
          process c
      of stIHex:
        case c
        of '0'..'9', 'a'..'f', 'A'..'F':
          top.ival = (top.ival shl 4)+c.h2i
        of 'i', 'I':
          pops()
          pushs stIPsf
        of 'u', 'U':
          pops()
          top = Object(kind: okUInt, ival: top.ival)
          pushs stIPsf
        else:
          process c
      of stIOct:
        case c
        of '0'..'7':
          top.ival = (top.ival shl 3)+c.d2i
        of 'i', 'I':
          pops()
          pushs stIPsf
        of 'u', 'U':
          pops()
          top = Object(kind: okUInt, ival: top.ival)
          pushs stIPsf
        else:
          process c
      of stIBin:
        case c
        of '0'..'1':
          top.ival = (top.ival shl 1)+c.d2i
        of 'i', 'I':
          pops()
          pushs stIPsf
        of 'u', 'U':
          pops()
          top = Object(kind: okUInt, ival: top.ival)
          pushs stIPsf
        else:
          process c
      of stIPsf:
        pops()
        case c
        of '1':
          if top.kind == okUInt:
            top = Object(kind: okU1, ival: top.ival)
          else:
            top = Object(kind: okI1, ival: top.ival)
        of '2':
          if top.kind == okUInt:
            top = Object(kind: okU2, ival: top.ival)
          else:
            top = Object(kind: okI2, ival: top.ival)
        of '4':
          if top.kind == okUInt:
            top = Object(kind: okU4, ival: top.ival)
          else:
            top = Object(kind: okI4, ival: top.ival)
        of '8':
          if top.kind == okUInt:
            top = Object(kind: okU8, ival: top.ival)
          else:
            top = Object(kind: okI8, ival: top.ival)
        else:
          process c
      of stF:
        case c
        of '0'..'9':
          top.fval = top.fval+c.d2i.float64/top.lastE
          top.lastE*=10.0
        of 'f', 'F':
          pops()
          pushs stFPsf
        else:
          pops()
          process c
      of stFPsf:
        pops()
        case c
        of '4':
          top = Object(kind: okF4, fval: top.fval)
        of '8':
          top = Object(kind: okF8, fval: top.fval)
        else:
          process c
      of stIdn:
        case c
        of 'a'..'z','A'..'Z','0'..'9':
          top.name.add c
        of '_':
          if top.name[^1]=='_':
            warn "Two underlines in a identifier; ignoring."
          else:
            top.name.add c
        else:
          pops()
          if top.name notin cpEnv.symTable:
            cpEnv.symTable[top.name]=Table[int,UId]()
          if cpEnv.depth notin cpEnv.symTable[top.name]:
            inc cpEnv.count
            cpEnv.symTable[top.name][cpEnv.depth]=cpEnv.count.UId
          top.uid=cpEnv.symTable[top.name][cpEnv.depth]
          process c
      else:
        fatal "Not a valid state! exiting."
        quit(QuitFailure)
    while cpEnv.stk.len > 0:
      if f.endOfFile:
        break
      var c = f.readChar()
      process c
    echo cpEnv.outStk


