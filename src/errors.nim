import std/[strutils]
import logger
type
  TokenizeError* = ref object of CatchableError

template catchTokenizeError*(line,column:int;file,buffer:string;body)=
  try:
    body
  except TokenizeError as err:
    error("file: '",file,"' line: ",line," column: ",column,"\n",buffer.splitLines(true)[line-1]," ".repeat(column-1),"^\nTokenizeError: ",err.msg)
    quit QuitFailure