# Options {{{1
set $ASM = 0

set confirm off
set verbose off

set history save on
set history size 10000
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

source ~/.gdb/pygdb-fork.py

define nub
  python nextUntilBreakpoint()
end

# -*- vim: set sts=2 sw=2 et fdm=marker: -*-
