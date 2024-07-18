import std/[math,parseutils,pegs,strutils,tables]
import errors,logger
converter u64tof64(u:uint64):float64=
  var i = $u
  for c in i:
    result*=10.0
    result+=float64(c.int8-'0'.int8)
type
  Tokenizer* = ref object
    line*,column*:int
    file*:string
    f:File
    buffer:string
    idx:int
    output*:seq[Token]
  TokenKind* = enum
    tkIdn,tkILit,tkFLit,tkCLit,tkSLit,tkImport,tkFrom,tkModule,
    tkDef,tkPriv,tkPub,tkExt,tkVar,tkLet,tkConst,tkFn,tkTempl,tkType,
    tkOPar,tkCPar,tkOBracket,tkCBracket,tkOBrace,tkCBrace,tkDot
    tkAdd,tkSub,tkMul,tkDiv,tkIDiv,tkMod,tkPow,tkArrow,tkPipe
    tkAnd,tkOr,tkXor,tkNot,tkNand,tkNor,tkNxor,tkShl,tkShr,tkAssi
    tkGt,tkLt,tkGe,tkLe,tkEq,tkNeq,tkAbsEq,tkAbsNeq,tkAt,tkDollar
    tkColon,tkSemiColon,tkComma,tkQuestion,tkComment,tkReverse,tkEOF
  Token* = ref object
    line*,column*:int
    file*:string
    case kind*:TokenKind
    of tkIdn:
      name*:string
      uid*:uint64
    of tkILit:
      ival*:uint64
      iType*:IType
    of tkFLit:
      fval*:float64
      fType*:FType
    of tkCLit:
      cval*:char
    of tkSLit:
      sval*:string
    else:
      discard
  IType* = enum
    itI8,itI16,itI32,itI64,
    itU8,itU16,itU32,itU64
  FType* = enum
    ftF32,ftF64
proc `$`*(t:Token):string=
  case t.kind
  of tkILit:
    $t.ival & ($t.iType)[2..^1].toLowerAscii()
  of tkFLit:
    $t.fval & ($t.fType)[2..^1].toLowerAscii()
  of tkCLit:
    t.cval.repr
  of tkEOF:
    "<EOF>"
  else:
    ""
proc newTokenizer*(file:string):Tokenizer=
  new result
  result.line=1
  result.column=1
  result.file=file
  try:
    result.f=open(file,fmRead)
    result.buffer=result.f.readAll().replace("\r\n","\n")
  except IOError as err:
    error "IOError: ",err.msg
    quit QuitFailure
proc step(tokenizer:Tokenizer,options:Table[string,string])
proc tokenize*(tokenizer:Tokenizer,options:Table[string,string])=
  catchTokenizeError tokenizer.line,tokenizer.column,tokenizer.file,tokenizer.buffer:
    while tokenizer.output.len==0 or tokenizer.output[^1].kind!=tkEOF:
      tokenizer.step(options)
let
  pInt=peg"{(i'0x'[0-9a-fA-F]+)/(i'0o'[0-7]+)/(i'0b'('0'/'1')+)/([0-9]+)} {(('i'/'u') ('8'/'16'/'32'/'64')?)?}"
  pFloat=peg"""
  ({[0-9]+'.'[0-9]+}{(i'e' ('+'/'-')? [0-9]+)?}{('f' ('32'/'64'))?})/
  ({[0-9]}{(i'e' ('+'/'-')? [0-9]+)}{('f' ('32'/'64'))?})
  """
  pChar=peg"\'{('\\' ('0'/'n'/'r'/'t'/'b'/'a'/('x'[0-9a-fA-F][0-9a-fA-F])))/.}\'"
proc step(tokenizer:Tokenizer,options:Table[string,string])=
  while tokenizer.idx<tokenizer.buffer.len and tokenizer.buffer[tokenizer.idx].isSpaceAscii:
    case tokenizer.buffer[tokenizer.idx]
    of '\n':
      inc tokenizer.line
      tokenizer.column=1
    else:
      inc tokenizer.column
    inc tokenizer.idx
  if tokenizer.idx>=tokenizer.buffer.len:
    var x:Token
    new x
    x=Token(kind:tkEOF,line:tokenizer.line,column:tokenizer.column,file:tokenizer.file)
    tokenizer.output.add x
    return
  var matched:array[MaxSubpatterns,string]
  var l:int
  template match(p:Peg,b:untyped)=
    bind l
    l=matchLen(tokenizer.buffer,p,matched,tokenizer.idx)
    if l != -1:
      tokenizer.idx+=l
      var x{.inject.}:Token
      new x
      b
      x.line=tokenizer.line
      x.column=tokenizer.column
      x.file=tokenizer.file
      for i in tokenizer.idx-l..<tokenizer.idx:
        case tokenizer.buffer[i]
        of '\n':
          inc tokenizer.line
          tokenizer.column=1
        else:
          inc tokenizer.column
      tokenizer.output.add x
      return
  match pChar:
    x=Token(kind:tkCLit,cval:(
      if matched[0].len==1:matched[0][0]
      else:
        case matched[0][1]
        of 'n':'\n'
        of 'r':'\r'
        of 'b':'\b'
        of 'a':'\a'
        of 't':'\t'
        of 'x':
          var i:int8
          assert parseHex(matched[0],i,2,2)!=0
          if i<0:
            raise TokenizeError(msg:"Invalid ascii character")
          i.char
        else:'\0'
    ))
    
  match pFloat:
    var f:BiggestFloat
    assert parseBiggestFloat(matched[0],f)!=0
    if matched[1].len>0:
      var e:BiggestInt
      assert parseBiggestInt(matched[1],e,1)!=0
      f*=pow(10.0,e.toBiggestFloat)
    x=Token(kind:tkFLit,fval:f,fType:(
      case matched[1]
      of "f32","F32":ftF32
      of "f64","F64":ftF64
      of "f","F","":
        if "bits" notin options or options["bits"]=="64":ftF64
        elif options["bits"]=="32":ftF32
        else:ftF64
      else:ftF64
    ))
  match pInt:
    var i:BiggestUInt
    if matched[0][0]=='0' and matched[0].len>2 and matched[0][1] in {'x','X','o','O','b','B'}:
      case matched[0][1]
      of 'x','X':
        assert parseHex(matched[0],i,2)!=0
      of 'o','O':
        assert parseOct(matched[0],i,2)!=0
      of 'b','B':
        assert parseBin(matched[0],i,2)!=0
      else:
        assert false
    else:
      try:
        assert parseBiggestUInt(matched[0],i)!=0
      except ValueError:
        raise TokenizeError(msg:"Integer extremely large:"&matched[0])
    if matched[1].len>1:
      var s:int
      assert parseInt(matched[1],s,1)!=0
      if i>pow(2.0,s.toFloat)-1:
        raise TokenizeError(msg:"Integer too large:"&($i)&" as a "&($s)&"-bit integer")
    x=Token(kind:tkILit,ival:i,iType:(
      case matched[1]
      of "i8","I8":itI8
      of "i16","I16":itI16
      of "i32","I32":itI32
      of "i64","I64":itI64
      of "u8","U8":itU8
      of "u16","U16":itU16
      of "u32","U32":itU32
      of "u64","U64":itU64
      of "i","I","":
        if "bits" notin options or options["bits"]=="64":itI64
        elif options["bits"]=="32":itI32
        else:itI64
      of "u","U":
        if "bits" notin options or options["bits"]=="64":itU64
        elif options["bits"]=="32":itU32
        else:itU64
      else:itI64
    ))

  raise TokenizeError(msg:"Invalid Syntax")


