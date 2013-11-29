
# gdb will 'recognize' this as python
#  upon 'source pygdb-fork.py'
# however, from gdb functions still have
#  to be called like:
#  (gdb) python print forkExecCapture("bt")   # with debug enabled,
                                              #   for return "..." prints
#  (gdb) python VAR=forkExecCapture("bt")     # 'release'
#
# note: even in 'release' (forkExecCapture), two (gdb) prompts may appear after a run!
# note: below also outputs word "None" in shell:
# python VAR=forkExecCapture("bt");print VAR
# ... but not if those two statements are ran on separate lines!

# note: bt doesn't really fail - but n indeed does (forkExecCapture)!
# only logExecCapture seems to work with n ...

import sys
import gdb
import os
import pty
import time
import signal

# 0 or 1 - debug print
FDBG=0

def forkExecCapture(instr):
  childpid=0;
  sys.stdout.flush()
  sys.stdin.flush()
  sys.stderr.flush()
  prev=signal.signal(signal.SIGTTOU, signal.SIG_IGN)
  os.tcsetpgrp(sys.stdin.fileno(),os.getpid())
  try:
    ( child_pid, fd ) = pty.fork()    # OK
    #~ child_pid, fd = os.forkpty()      # OK
  except OSError as e:
    print str(e)

  if child_pid == 0:
    #~ os.tcsetpgrp(fd, tcgrp) # note: fd=-1 for child!
    # note: in child process, every print is redirected to parent!
    #
    #childpid=os.getpid()
    if FDBG: print "In Child Process: PID# %s" % os.getpid()
    # no need for REP=gdb.execute("n",True,to_string=True)
    # here we capture everything anyways; just run
    gdb.execute(instr)
    time.sleep(0.5)
    print RET
    if FDBG: return "child returns"
    #else: return 0
  else:
    #~ os.tcsetpgrp(fd, tcgrp) # Inappropriate ioctl for device
    os.tcsetpgrp(sys.stdin.fileno(),os.getpid())
    if FDBG: print "In Parent Process: PID# %s" % os.getpid()
    # now must 'drain' the printout from child process
    #~ os.waitpid(os.getpid(), 0) # No child processes
    time.sleep(0.5)
    CHILDREPLY=os.read(fd, 10000)
    time.sleep(0.5)
    # wait for child to exit
    if FDBG: print CHILDREPLY
    if FDBG: return "parent returns"
    else: return CHILDREPLY


def logExecCapture(instr):
  # http://www.howtoforge.com/storing-files-directories-in-memory-with-tmpfs
  # /dev/shm - save file in RAM
  ltxname="/dev/shm/c.log"

  # unfortunately, locks with fifo
  #~ lpfname="logpipe"
  #~ if not os.path.exists(lpfname):
    #~ open('file', 'w').close() #  "touch",  http://www.gossamer-threads.com/lists/python/python/650546
    #~ os.mkfifo(lpfname)
  #~ fifo = open(lpfname, 'rw')

  gdb.execute("set logging file "+ltxname) # lpfname
  gdb.execute("set logging redirect on")
  gdb.execute("set logging overwrite on")
  gdb.execute("set logging on")
  gdb.execute(instr)
  gdb.execute("set logging off")

  #~ replyContents = os.read(fifo, 10000) #fifo.read()
  #~ fifo.close()

  replyContents = open(ltxname, 'r').read() # read entire file
  return replyContents


# next until breakpoint
# note: adjusted to work with FDBG=0 !!
# doesn't really work with forkExecCapture
def nextUntilBreakpoint():
  #~ tcgrp=os.tcgetpgrp(sys.stdin.fileno()); # get "process group associated with" stdout
  # as long as we don't find "Breakpoint" in report:
  isInBreakpoint = -1;
  while isInBreakpoint == -1:
    #~ REP=forkExecCapture("n") #,tcgrp)
    REP=logExecCapture("n") #,tcgrp)
    isInBreakpoint = REP.find("Breakpoint")
    print "LOOP:: ", isInBreakpoint, "\n", REP # forkExecCapture: [tcsetpgrp failed in terminal_inferior: Operation not permitted]
    #~ isInBreakpoint=0


