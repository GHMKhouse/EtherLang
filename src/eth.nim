import std/[os,parseopt,tables]
import logger,tokenizer
when isMainModule:
  var optParser=initOptParser(os.commandLineParams())
  var
    options:Table[string,string]
    files:seq[string]
  while true:
    optParser.next()
    case optParser.kind
    of cmdEnd:
      break
    of cmdShortOption,cmdLongOption:
      if optParser.kind==cmdShortOption:
        const short2long={"d":"define","l":"log"}.toTable
        optParser.key=short2long[optParser.key]
      options[optParser.key]=optParser.val
    of cmdArgument:
      files.add optParser.key
  if "log" in options:
    initLogger(options["log"])
  else:
    initLogger()
  for file in files:
    var tokenizer=newTokenizer(file)
    tokenizer.tokenize(options)
    echo tokenizer.output