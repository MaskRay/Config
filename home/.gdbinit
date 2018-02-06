# Options {{{1
set debug-file-directory /usr/lib/debug
set $ASM = 0

set confirm off
set verbose off

set history save on
set history size 100000
set history filename ~/.history/gdb
set history expansion on
set disassembly-flavor intel
#set output-radix 16

# prevent "Type <return> to continue"
set height 0
# prevent line wrap
set width 0

handle SIGALRM nostop print nopass
handle SIGBUS stop print nopass
handle SIGPIPE nostop print nopass
handle SIGSEGV stop print nopass

set python print-stack full

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

define b_a
  #b __asan_report_load1
  #b __asan_report_load2
  #b __asan_report_load4
  #b __asan_report_load8
  #b __asan_report_load16
  #b __asan_report_store1
  #b __asan_report_store2
  #b __asan_report_store4
  #b __asan_report_store8
  #b __asan_report_store16
  #b __asan_report_error
  b __asan::ReportGenericError
end

define b_u
  b __ubsan_handle_add_overflow
  b __ubsan_handle_mul_overflow
  b __ubsan_handle_negate_overflow

  b __ubsan_handle_builtin_unreachable
  b __ubsan_handle_divrem_overflow
  b __ubsan_handle_out_of_bounds

  b __ubsan_handle_float_cast_overflow
  b __ubsan_handle_shift_out_of_bounds

  b __ubsan_handle_function_type_mismatch
  b __ubsan_handle_sub_overflow

  b __ubsan_handle_load_invalid_value
  b __ubsan_handle_type_mismatch

  b __ubsan_handle_missing_return
  b __ubsan_handle_vla_bound_not_positive
end

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

#python
#import sys
#sys.path.insert(0, '/usr/share/gdb/python/gdb/command')
#import pretty_printers
#pretty_printers.register_pretty_printer_commands()
#end

# -*- vim: set sts=2 sw=2 et fdm=marker: -*-
