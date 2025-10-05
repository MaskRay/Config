# Options {{{1
set debug-file-directory /usr/lib/debug
set $ASM = 0

set confirm off
set verbose off

set history save on
set history size 100000
set history filename ~/.history/gdb
set history expansion on
#set disassembly-flavor intel
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

alias dd=disassemble
alias fi=finish
alias var=info variables

define li
  x/10i $pc

end
document li
  list machine instructions
end

# Plugins {{{1

#source ~/.gdb/pygdb-fork.py

define b_a
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

python
class SkipFrames(gdb.Command):
    """Move through stack, skipping frames matching regex pattern.
    Usage: usk [pattern]  - up-skip
           dsk [pattern]  - down-skip
           fsk [pattern]  - finish-skip
    Moves up/down/finish through frames, skipping any where the filename matches the pattern.
    """
    def __init__(self, cmd_name, gdb_command, cmd_class):
        self.gdb_command = gdb_command
        super(SkipFrames, self).__init__(cmd_name, cmd_class)

    def invoke(self, arg, from_tty):
        pattern = re.compile(arg.strip() if arg.strip() else r'\.cargo|/rustc')

        # Save current pagination setting and disable it
        pagination = gdb.parameter("pagination")
        gdb.execute("set pagination off", to_string=True)

        try:
            while True:
                # Execute the command (up/down/finish)
                gdb.execute(self.gdb_command, to_string=True)

                # Get current frame and source info
                frame = gdb.selected_frame()
                sal = frame.find_sal()

                # Check filename
                if sal.symtab and sal.symtab.filename:
                    filename = sal.symtab.filename
                    # If doesn't match unwanted pattern, stop here
                    if not pattern.search(filename):
                        gdb.execute("frame")  # Display frame info
                        break
                    else:
                        print(f"Skipping: {filename}")
                else:
                    # No debug info - stop here to be safe
                    gdb.execute("frame")
                    break
        except gdb.error as e:
            print(f"Cannot go further: {e}")
        finally:
            # Restore pagination setting
            gdb.execute(f"set pagination {'on' if pagination else 'off'}", to_string=True)

SkipFrames("usk", "up", gdb.COMMAND_STACK)
SkipFrames("dsk", "down", gdb.COMMAND_STACK)
SkipFrames("fsk", "finish", gdb.COMMAND_RUNNING)
end

#
# C++ related beautifiers (optional)
#

#set print pretty on
#set print object on
set print static-members off

python
import os
prettyprinter_path = os.path.expanduser("~/llvm/llvm/utils/gdb-scripts/prettyprinters.py")
if os.path.exists(prettyprinter_path):
    gdb.execute(f"source {prettyprinter_path}")
end

#python
#import sys
#sys.path.insert(0, '/usr/share/gdb/python/gdb/command')
#import pretty_printers
#pretty_printers.register_pretty_printer_commands()
#end

# -*- vim: set sts=2 sw=2 et fdm=marker: -*-
