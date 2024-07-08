# The whole compiler
import std/[logging, os, parseopt, sets, tables]
{.push used.}
type
  UId = uint64
  ObjectKind = enum
    okI8, okI16, okI32, okI64, okU8, okU16, okU32, okU64, okInt, okUInt,
      okFloat, okF32, okF64, okIdn
  Object = ref object
    case kind: ObjectKind
    of okI8, okI16, okI32, okI64, okU8, okU16, okU32, okU64, okInt, okUInt:
      ival: uint64
    of okFloat, okF32, okF64:
      fval: float64
    of okIdn:
      name: string
      uid: UId
  State = enum
    stTop, stIPref0, stIPrefK, stI, stIBin, stIOct, stIHex, stIPsfK, stIPsfS1,
      stIPsfS2, stIdn
  CpEnv = ref object
    stk: seq[State]
    outStk: seq[Object]
    symTable:Table[string,Table[int,UId]]
    depth:int
    count:int
proc `$`(x:Object):string=
  case x.kind
  of okI8, okI16, okI32, okI64, okU8, okU16, okU32, okU64, okInt, okUInt:
    ($x.ival)&($x.kind)[2..^1]
  of okF32,okF64,okFloat:
    ($x.fval)&($x.kind)[2..^1]
  of okIdn:
    x.name&"@"&($x.uid)
proc newInt(i: SomeInteger): Object =
  new result
  result = Object(kind: okInt, ival: cast[uint64](i))
proc newIdn():Object=
  new result
  result = Object(kind:okIdn)
var
  defines: HashSet[string]
  options: HashSet[string]
  keyOptions: Table[string, string]
  files: seq[string]
  consoleLogger: ConsoleLogger
  fileLogger: FileLogger
when isMainModule:
  consoleLogger = newConsoleLogger(fmtStr = "$levelname: ")
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
          warn "Option already exists; ignoring."
      elif cmdParser.key == "define":
        if defines.containsOrIncl cmdParser.val:
          warn "Define already exists; ignoring."
      elif cmdParser.key == "log":
        fileLogger.file.close()
        fileLogger.file = open(cmdParser.val, fmAppend)
      else:
        if cmdParser.key in keyOptions:
          warn "Option already exists; covering."
        keyOptions[cmdParser.key] = cmdParser.val
    of cmdArgument:
      if files.len > 0:
        info "More than one input; combining in order."
      files.add cmdParser.key
  var output{.used.} = ""
  for file in files:
    var f = open(file, fmRead)
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
    template invalidChar =
      error "Unexpected char '", $c, "'; exiting."
      quit(QuitFailure)
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
      case tops
      of stTop:
        case c
        of ' ', '\n', '\t':
          return
        of '0':
          pushs stIPref0
        of '1'..'9':
          pushs stI
          push newInt(c.d2i)
        of 'A'..'Z', 'a'..'z':
          pushs stIdn
          push newIdn()
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
        else:
          push newInt(0)
          process c
      of stI:
        case c
        of '0'..'9':
          top.ival = top.ival*10+c.d2i
        else:
          pops()
          pushs stIPsfK
      of stIHex:
        case c
        of '0'..'9', 'a'..'f', 'A'..'F':
          top.ival = (top.ival shl 4)+c.h2i
        else:
          pops()
          pushs stIPsfK
      of stIOct:
        case c
        of '0'..'7':
          top.ival = (top.ival shl 3)+c.d2i
        else:
          pops()
          pushs stIPsfK
      of stIBin:
        case c
        of '0'..'1':
          top.ival = (top.ival shl 1)+c.d2i
        else:
          pops()
          pushs stIPsfK
      of stIPsfK:
        pops()
        case c
        of 'i', 'I':
          pushs stIPsfS1
        of 'u', 'U':
          top = Object(kind: okUInt, ival: top.ival)
          pushs stIPsfS1
        else:
          process c
      of stIPsfS1:
        pops()
        case c
        of '8':
          if top.kind == okUInt:
            top = Object(kind: okU8, ival: top.ival)
          else:
            top = Object(kind: okI8, ival: top.ival)
        of '1', '3', '6':
          pushs stIPsfS2
        else:
          process c
      of stIPsfS2:
        pops()
        case c
        of '6':
          if top.kind == okUInt:
            top = Object(kind: okU16, ival: top.ival)
          else:
            top = Object(kind: okI16, ival: top.ival)
        of '2':
          if top.kind == okUInt:
            top = Object(kind: okU32, ival: top.ival)
          else:
            top = Object(kind: okI32, ival: top.ival)
        of '4':
          if top.kind == okUInt:
            top = Object(kind: okU64, ival: top.ival)
          else:
            top = Object(kind: okI64, ival: top.ival)
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


