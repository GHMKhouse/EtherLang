import std/[logging, strutils, terminal, colors]

type ColoredConsoleLogger = ref object of ConsoleLogger
const coolerFmtStr* = "$datetime $levelname [$appname] "

proc newColoredLogger*(levelThreshold = lvlAll, fmtStr = defaultFmtStr, useStderr = false): ColoredConsoleLogger =
  ColoredConsoleLogger(levelThreshold: levelThreshold, fmtStr: fmtStr, useStderr: useStderr)

#                                          DEBUG     DEBUG     INFO     NOTICE    WARN       ERROR   FATAL       NONE
const LevelColours: array[Level, Color] = [colWhite, colWhite, colCyan, colGreen, colOrange, colRed, colDarkRed, colGray]

method log*(logger: ColoredConsoleLogger, level: Level, args: varargs[string, `$`]) =
  let
    color = ansiForegroundColorCode(LevelColours[level])
    cdef = ansiForegroundColorCode(fgDefault)
    fmt = logger.fmtStr.multiReplace(("$levelname", color & "$levelname" & cdef), ("$levelid", color & "$levelid" & cdef))
    line = substituteLog(fmt, level, args)
  if logger.useStderr: stderr.writeLine line
  else: stdout.writeLine line

var
  cLogger*:ColoredConsoleLogger
  fLogger*:FileLogger
proc initLogger*(file:string="log/log.log")=
  cLogger = newColoredLogger(fmtStr="$levelid:  ",useStderr=true)
  fLogger = newFileLogger(file,fmAppend,fmtStr="[$datetime] $levelid: ")
  addHandler cLogger
  addHandler fLogger
export logging.error,logging.fatal,logging.warn,logging.notice,logging.info,logging.debug