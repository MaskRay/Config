# Options {{{1
set debug-file-directory /tmp/debug
set $ASM = 0

set confirm off
set verbose off

set history save on
set history size 100000
set history filename ~/.history/gdb
set disassembly-flavor intel
#set output-radix 16

# prevent "Type <return> to continue"
set height 0
# prevent line wrap
set width 0

# Bindings {{{1

alias var=info variables

define li
  x/10i $pc

end
document li
  list machine instructions
end

alias dd=disassemble

# Plugins {{{1

#source ~/.gdb/pygdb-fork.py

#define nub
#  python nextUntilBreakpoint()
#end

define fs
  finish
  step
end

#
# C++ related beautifiers (optional)
#

#set print pretty on
#set print object on
#set print static-members on
#set print vtbl on
#set demangle-style gnu-v3

python
import sys
sys.path.insert(0, '/usr/share/gdb/python/gdb/command')
import pretty_printers
pretty_printers.register_pretty_printer_commands()
end

set python print-stack full

# -*- vim: set sts=2 sw=2 et fdm=marker: -*-
