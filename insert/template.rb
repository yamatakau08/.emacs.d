require 'pp'

## put out the source line without buffering when use "ruby -rdebug" since ruby2.6.3p62 on mingw64
$stdout.sync = true
## change the directory in which script is and execute
Dir.chdir(__dir__)
