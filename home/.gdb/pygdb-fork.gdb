# gdb script: pygdb-fork.gdb
# easier interface for pygdb-fork.py stuff
# from within gdb: (gdb) source -v pygdb-fork.gdb
# from cdmline: gdb -x pygdb-fork.gdb -se test.exe


# first, "include" the python file:

# define shorthand for nextUntilBreakpoint():

# set up breakpoints for test.exe:
b main
b doFunction

# go to main breakpoint
run

# python RET=forkExecCapture("n")
# python time.sleep(0.5)
# python print RET
