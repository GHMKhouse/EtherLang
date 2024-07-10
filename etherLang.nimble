# Package

version       = "0.1.0"
author        = "InkOfSilicon"
description   = "The compiler of a new flat language:Ether"
license       = "MIT"
srcDir        = "src"
bin           = @["eth"]
binDir        = "bin"

# Dependencies

requires "nim >= 2.0.0"

# Tasks

task sample,"running samples":
  exec "nimble build"
  exec "bin/eth sample/tokens.eth"