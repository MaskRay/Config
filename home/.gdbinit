# Options {{{1
set $ASM = 0
set $SHOW_CONTEXT = 1
set $CONTEXTSIZE_STACK = 6
set $CONTEXTSIZE_DATA  = 8
set $CONTEXTSIZE_CODE  = 8
# set to 1 to have ARM target debugging as default, use the "arm" command to switch inside gdb
set $ARM = 0
# set to 0 if you have problems with the colorized prompt - reported by Plouj with Ubuntu gdb 7.2
set $COLOUREDPROMPT = 1
# Colour the first line of the disassembly - default is green, if you want to change it search for
# SETCOLOUR1STLINE and modify it :-)
set $SETCOLOUR1STLINE = 0
# set to 0 to remove display of objectivec messages (default is 1)
set $SHOWOBJECTIVEC = 1
# set to 0 to remove display of cpu registers (default is 1)
set $SHOWREGS = 1
# set to 1 to enable display of stack (default is 0)
set $SHOWSTACK = 0
# set to 1 to enable display of data window (default is 0)
set $SHOWDATAWIN = 0
# set to 0 to disable coloured display of changed registers
set $SHOWREGCHANGES = 1
# set to 1 so skip command to execute the instruction at the new location
# by default it EIP/RIP will be modified and update the new context but not execute the instruction
set $SKIPEXECUTE = 0
# if $SKIPEXECUTE is 1 configure the type of execution
# 1 = use stepo (do not get into calls), 0 = use stepi (step into calls)
set $SKIPSTEP = 1
# show the ARM opcodes - change to 0 if you don't want such thing (in x/i command)
set $ARMOPCODES = 1

set confirm off
set verbose off

set history save on
set history size 10000
set history filename ~/.history/gdb
set disassembly-flavor intel
#set output-radix 16

# Toggles {{{2
define enablestack
  set $SHOWSTACK = 1
end
define disablestack
  set $SHOWSTACK = 0
end

define enableregs
  set $SHOWREGS = 1
end
define disableregs
  set $SHOWREGS = 0
end

define asm
  set $ASM = ! $ASM
end

define arm
  set $ARM = 1
  set arm show-opcode-bytes 1
end

# Colors {{{1

set $BLACK = 0
set $RED = 1
set $GREEN = 2
set $YELLOW = 3
set $BLUE = 4
set $MAGENTA = 5
set $CYAN = 6
set $WHITE = 7

set $COLOR_REGNAME = $GREEN
set $COLOR_REGVAL = $BLACK
set $COLOR_REGVAL_MODIFIED  = $RED
set $COLOR_SEPARATOR = $BLUE
set $COLOR_CPUFLAGS = $RED
set $COLOR_REGNAME = $GREEN
set $COLOR_REGVAL = $BLACK
set $COLOR_REGVAL_MODIFIED  = $RED
set $COLOR_SEPARATOR = $BLUE
set $COLOR_CPUFLAGS = $RED

define color_reset
  echo \033[0m
end

define color_bold
  echo \033[1m
end

define color_underline
  echo \033[4m
end

# this is ugly but there's no else if available :-(
define color
  # BLACK
  if $arg0 == 0
    echo \033[30m
  else
    # RED
    if $arg0 == 1
      echo \033[31m
    else
      # GREEN
      if $arg0 == 2
        echo \033[32m
      else
        # YELLOW
        if $arg0 == 3
          echo \033[33m
        else
          # BLUE
          if $arg0 == 4
            echo \033[34m
          else
            # MAGENTA
            if $arg0 == 5
              echo \033[35m
            else
              # CYAN
              if $arg0 == 6
                echo \033[36m
              else
                # WHITE
                if $arg0 == 7
                  echo \033[37m
                end
              end
            end
          end
        end
      end
    end
  end
end

# prevent "Type <return> to continue"
set height 0
# prevent line wrap
set width 0

# Bindings {{{1

define n
  if $ASM > 0
    if $argc == 0
      nexti
    end
    if $argc == 1
      nexti $arg0
    end
  else
    if $argc == 0
      next
    end
    if $argc == 1
      next $arg0
    end
  end
end

define s
  if $ASM > 0
    if $argc == 0
        stepi
    end
    if $argc == 1
        stepi $arg0
    end
    if $argc > 1
        help go
    end
  else
    if $argc == 0
        step
    end
    if $argc == 1
        step $arg0
    end
  end
end

alias var=info variables

define li
  x/10i $pc
end
document li
  list machine instructions
end

alias dd=disassemble

# __________hex/ascii dump an address_________
define ascii_char
    if $argc != 1
        help ascii_char
    else
        # thanks elaine :)
        set $_c = *(unsigned char *)($arg0)
        if ($_c < 0x20 || $_c > 0x7E)
            printf "."
        else
            printf "%c", $_c
        end
    end
end
document ascii_char
Syntax: ascii_char ADDR
| Print ASCII value of byte at address ADDR.
| Print "." if the value is unprintable.
end

define hexdump
  if $argc == 1
    hexdump $arg0, 1
  else
    if $argc == 2
      set $_line = 0
      while ($_line < $arg1)
        set $_i = $arg0 + $_line * 16

        color_bold
        if (sizeof(void *) == 8)
          printf "0x%016lX  ", $_i
        else
          printf "0x%08X  ", $_i
        end
        color_reset

        set $_j = 0
        while ($_j < 16)
          printf "%02x%02x ", *(unsigned char*)($_i+$_j), *(unsigned char*)($_i+$_j)
          set $_j += 2
        end

        printf " "
        color_bold
        set $_j = 0
        while ($_j < 16)
          ascii_char $_i+$_j
          set $_j++
        end
        color_reset

        printf "\n"
        set $_line++
      end
    else
      help hexdump
    end
  end
end
document hexdump
Syntax: hexdump ADDR <NR_LINES>
| Display a 16-byte hex/ASCII dump of memory starting at address ADDR.
| Optional parameter is the number of lines to display if you want more than one.
end

# Context {{{1
define context
  color $COLOR_SEPARATOR
  if $SHOWREGS == 1
    printf "----------------------------------------"
    printf "----------------------------------"
    if (sizeof(void *) == 8)
      printf "---------------------------------------------"
    end
    color $COLOR_SEPARATOR
    color_bold
    printf "[regs]\n"
    color_reset
    reg
    color $CYAN
  end
  if $SHOWSTACK == 1
    color $COLOR_SEPARATOR
  if (sizeof(void *) == 8)
    printf "[0x%04X:0x%016lX]", $ss, $rsp
  else
    printf "[0x%04X:0x%08X]", $ss, $esp
  end
  color $COLOR_SEPARATOR
  printf "-------------------------"
    printf "-----------------------------"
    if (sizeof(void *) == 8)
      printf "-------------------------------------"
    end
    color $COLOR_SEPARATOR
    color_bold
    printf "[stack]\n"
    color_reset
    set $context_i = $CONTEXTSIZE_STACK
    while ($context_i > 0)
          set $context_t = $sp + 0x10 * ($context_i - 1)
          hexdump $context_t
          set $context_i--
    end
  end
  color_reset
# and this is the end of this little crap

    color $COLOR_SEPARATOR
    printf "--------------------------------------------------------------------------"
    if (sizeof(void *) == 8)
      printf "---------------------------------------------"
  end
  color $COLOR_SEPARATOR
  color_bold
  printf "[code]\n"
  color_reset
  set $context_i = $CONTEXTSIZE_CODE
  if ($context_i > 0)
    if ($SETCOLOUR1STLINE == 1) 
      color $GREEN
      x /i $pc
      color_reset
    else
      x /i $pc
    end
    set $context_i--
  end
  while ($context_i > 0)
    x /i
    set $context_i--
  end
  color $COLOR_SEPARATOR
  printf "----------------------------------------"
  printf "----------------------------------------"
  if (sizeof(void *) == 8)
    printf "---------------------------------------------\n"
  else
    printf "\n"
  end
  color_reset
end
document context
Syntax: context
| Print context window, i.e. regs, stack, ds:esi and disassemble cs:eip.
end

define hook-stop
# Display instructions formats
  if $ARM == 1
    if $ARMOPCODES == 1
      set arm show-opcode-bytes 1
    else
      set arm show-opcode-bytes 1
    end
  end

  # this makes 'context' be called at every BP/step
  if ($SHOW_CONTEXT > 0)
    context
  end
end

define context-on
  set $SHOW_CONTEXT = 1
  printf "Displaying of context is now ON\n"
end

define context-off
  set $SHOW_CONTEXT = 0
  printf "Displaying of context is now OFF\n"
end

# Registers {{{2

# Init {{{3
# Initialize these variables else comparisons will fail for colouring
# we must initialize all of them at once, 32 and 64 bits, and ARM.
set $oldrax = 0
set $oldrbx = 0
set $oldrcx = 0
set $oldrdx = 0
set $oldrsi = 0
set $oldrdi = 0
set $oldrbp = 0
set $oldrsp = 0
set $oldr8  = 0
set $oldr9  = 0
set $oldr10 = 0
set $oldr11 = 0
set $oldr12 = 0
set $oldr13 = 0
set $oldr14 = 0
set $oldr15 = 0
set $oldeax = 0
set $oldebx = 0
set $oldecx = 0
set $oldedx = 0
set $oldesi = 0
set $oldedi = 0
set $oldebp = 0
set $oldesp = 0
set $oldr0  = 0
set $oldr1  = 0
set $oldr2  = 0
set $oldr3  = 0
set $oldr4  = 0
set $oldr5  = 0
set $oldr6  = 0
set $oldr7  = 0
set $oldsp  = 0
set $oldlr  = 0

# Workhorse {{{3
define regarm
    printf "  "
    # R0
    color $COLOR_REGNAME
    printf "R0:"
    if ($r0 != $oldr0 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X  ", $r0
    # R1
    color $COLOR_REGNAME
    printf "R1:"
    if ($r1 != $oldr1 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $r1
  # R2
    color $COLOR_REGNAME
    printf "R2:"
    if ($r2 != $oldr2 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X  ", $r2
  # R3
    color $COLOR_REGNAME
    printf "R3:"
    if ($r3 != $oldr3 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X\n", $r3
    printf "  "
  # R4
    color $COLOR_REGNAME
    printf "R4:"
    if ($r4 != $oldr4 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X  ", $r4
    # R5
    color $COLOR_REGNAME
    printf "R5:"
    if ($r5 != $oldr5 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $r5
  # R6
    color $COLOR_REGNAME
    printf "R6:"
    if ($r6 != $oldr6 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X  ", $r6
  # R7
    color $COLOR_REGNAME
    printf "R7:"
    if ($r7 != $oldr7 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X\n", $r7
    printf "  "
  # R8
    color $COLOR_REGNAME
    printf "R8:"
    if ($r8 != $oldr8 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X  ", $r8
  # R9
    color $COLOR_REGNAME
    printf "R9:"
    if ($r9 != $oldr9 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $r9
  # R10
    color $COLOR_REGNAME
    printf "R10:"
    if ($r10 != $oldr10 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $r10
  # R11
    color $COLOR_REGNAME
    printf "R11:"
    if ($r11 != $oldr11 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X ", $r11
    printf "\n"
    # R12
    color $COLOR_REGNAME
    printf "  R12:"
    if ($r12 != $oldr12 && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X", $r12
    printf "  "
    # SP
    color $COLOR_REGNAME
    printf "SP:"
    if ($sp != $oldsp && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $sp
  # LR
    color $COLOR_REGNAME
    printf "LR:"
    if ($lr != $oldlr && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf "  0x%08X  ", $lr
  # PC
    color $COLOR_REGNAME
    printf "PC:"
    color $COLOR_REGVAL_MODIFIED
    printf "  0x%08X  ", $pc
    color_bold
    color_underline
    color $COLOR_CPUFLAGS
    flags
  color_reset
    printf "\n"
end
document regarm
Syntax: regarm
| Auxiliary function to display ARM registers.
end

define regx64
    # 64bits stuff
    printf "  "
    # RAX
    color $COLOR_REGNAME
    printf "RAX:"
    if ($rax != $oldrax && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $rax
    # RBX
    color $COLOR_REGNAME
    printf "RBX:"
    if ($rbx != $oldrbx && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $rbx
    # RBP
    color $COLOR_REGNAME
    printf "RBP:"
    if ($rbp != $oldrbp && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $rbp
    # RSP
    color $COLOR_REGNAME
    printf "RSP:"
    if ($rsp != $oldrsp && $SHOWREGCHANGES == 1)
        color $COLOR_REGVAL_MODIFIED
    else
        color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $rsp
    color_bold
    color_underline
    color $COLOR_CPUFLAGS
    flags
    color_reset
    printf "  "
    # RDI
    color $COLOR_REGNAME
    printf "RDI:"
    if ($rdi != $oldrdi && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%016lX  ", $rdi
  # RSI
    color $COLOR_REGNAME
    printf "RSI:"
  if ($rsi != $oldrsi && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%016lX  ", $rsi
  # RDX
    color $COLOR_REGNAME
    printf "RDX:"
  if ($rdx != $oldrdx && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%016lX  ", $rdx
  # RCX
    color $COLOR_REGNAME
    printf "RCX:"
  if ($rcx != $oldrcx && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $rcx
    # RIP
    color $COLOR_REGNAME
    printf "RIP:"
    color $COLOR_REGVAL_MODIFIED
    printf " 0x%016lX\n  ", $rip
    # R8
    color $COLOR_REGNAME
    printf "R8 :"
  if ($r8 != $oldr8 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r8
    # R9
    color $COLOR_REGNAME
    printf "R9 :"
    if ($r9 != $oldr9 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r9
    # R10
    color $COLOR_REGNAME
    printf "R10:"
    if ($r10 != $oldr10 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r10
    # R11
    color $COLOR_REGNAME
    printf "R11:"
  if ($r11 != $oldr11 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r11
    # R12
    color $COLOR_REGNAME
  printf "R12:"
    if ($r12 != $oldr12 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX\n  ", $r12
    # R13
    color $COLOR_REGNAME
    printf "R13:"
    if ($r13 != $oldr13 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r13
    # R14
    color $COLOR_REGNAME
    printf "R14:"
    if ($r14 != $oldr14 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r14
    # R15
  color $COLOR_REGNAME
    printf "R15:"
    if ($r15 != $oldr15 && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%016lX  ", $r15
end
document regx64
Syntax: regx64
| Auxiliary function to display X86_64 registers.
end

define regx86
    printf "  "
    # EAX
    color $COLOR_REGNAME
  printf "EAX:"
    if ($eax != $oldeax && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
    else
      color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $eax
    # EBX
    color $COLOR_REGNAME
    printf "EBX:"
    if ($ebx != $oldebx && $SHOWREGCHANGES == 1) 
      color $COLOR_REGVAL_MODIFIED    
    else
      color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $ebx
    # ECX
    color $COLOR_REGNAME
    printf "ECX:"
    if ($ecx != $oldecx && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%08X  ", $ecx
  # EDX
  color $COLOR_REGNAME
  printf "EDX:"
  if ($edx != $oldedx && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
        printf " 0x%08X  ", $edx
  color_bold
  color_underline
  color $COLOR_CPUFLAGS
    flags
    color_reset
    printf "  "
    # ESI
  color $COLOR_REGNAME
    printf "ESI:"
    if ($esi != $oldesi && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%08X  ", $esi
  # EDI
  color $COLOR_REGNAME
    printf "EDI:"
  if ($edi != $oldedi && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%08X  ", $edi
  # EBP
  color $COLOR_REGNAME
  printf "EBP:"
  if ($ebp != $oldebp && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
  end
  printf " 0x%08X  ", $ebp
  # ESP
  color $COLOR_REGNAME
    printf "ESP:"
  if ($esp != $oldesp && $SHOWREGCHANGES == 1)
      color $COLOR_REGVAL_MODIFIED
  else
      color $COLOR_REGVAL
    end
    printf " 0x%08X  ", $esp
    # EIP
    color $COLOR_REGNAME
    printf "EIP:"
    color $COLOR_REGVAL_MODIFIED
    printf " 0x%08X\n  ", $eip
    color $COLOR_REGNAME
end
document regx86
Syntax: regx86
| Auxiliary function to display X86 registers.
end

define reg
    if $ARM == 1
        regarm
      if ($SHOWREGCHANGES == 1)
          set $oldr0  = $r0
          set $oldr1  = $r1
          set $oldr2  = $r2
          set $oldr3  = $r3
          set $oldr4  = $r4
          set $oldr5  = $r5
          set $oldr6  = $r6
          set $oldr7  = $r7
          set $oldr8  = $r8
        set $oldr9  = $r9
        set $oldr10 = $r10
        set $oldr11 = $r11
      set $oldr12 = $r12
      set $oldsp  = $sp
      set $oldlr  = $lr
      end
    else
        if (sizeof(void *) == 8)
            regx64 
        else
            regx86
        end
        # call smallregisters
      smallregisters
        # display conditional jump routine
      if (sizeof(void *) == 8)
          printf "\t\t\t\t"
      end
        printf "\n"
        if ($SHOWREGCHANGES == 1)
          if (sizeof(void *) == 8)
            set $oldrax = $rax
          set $oldrbx = $rbx
          set $oldrcx = $rcx
          set $oldrdx = $rdx
          set $oldrsi = $rsi
          set $oldrdi = $rdi
          set $oldrbp = $rbp
          set $oldrsp = $rsp
          set $oldr8  = $r8
          set $oldr9  = $r9
          set $oldr10 = $r10
          set $oldr11 = $r11
          set $oldr12 = $r12
          set $oldr13 = $r13
          set $oldr14 = $r14
          set $oldr15 = $r15
        else
            set $oldeax = $eax
          set $oldebx = $ebx
          set $oldecx = $ecx
          set $oldedx = $edx
          set $oldesi = $esi
          set $oldedi = $edi
          set $oldebp = $ebp
          set $oldesp = $esp
        end
      end
    end
end
document reg
Syntax: reg
| Print CPU registers.
end

define smallregisters
    if (sizeof(void *) == 8)
    #64bits stuff
      # from rax
      set $eax = $rax & 0xffffffff
      set $ax  = $rax & 0xffff
      set $al  = $ax & 0xff
      set $ah  = $ax >> 8
      # from rbx
      set $ebx = $rbx & 0xffffffff
      set $bx  = $rbx & 0xffff
      set $bl  = $bx & 0xff
      set $bh  = $bx >> 8
      # from rcx
      set $ecx = $rcx & 0xffffffff
      set $cx  = $rcx & 0xffff
      set $cl  = $cx & 0xff
      set $ch  = $cx >> 8
      # from rdx
      set $edx = $rdx & 0xffffffff
      set $dx  = $rdx & 0xffff
      set $dl  = $dx & 0xff
      set $dh  = $dx >> 8
      # from rsi
      set $esi = $rsi & 0xffffffff
      set $si  = $rsi & 0xffff
      # from rdi
      set $edi = $rdi & 0xffffffff
      set $di  = $rdi & 0xffff    
    #32 bits stuff
    else
      # from eax
      set $ax = $eax & 0xffff
      set $al = $ax & 0xff
      set $ah = $ax >> 8
      # from ebx
      set $bx = $ebx & 0xffff
      set $bl = $bx & 0xff
      set $bh = $bx >> 8
      # from ecx
      set $cx = $ecx & 0xffff
      set $cl = $cx & 0xff
      set $ch = $cx >> 8
      # from edx
      set $dx = $edx & 0xffff
      set $dl = $dx & 0xff
      set $dh = $dx >> 8
      # from esi
      set $si = $esi & 0xffff
      # from edi
      set $di = $edi & 0xffff   
     end
end
document smallregisters
Syntax: smallregisters
| Create the 16 and 8 bit cpu registers (gdb doesn't have them by default).
| And 32bits if we are dealing with 64bits binaries.
end

# Flags 
define flagsarm
# conditional flags are
# negative/less than (N), bit 31 of CPSR
# zero (Z), bit 30
# Carry/Borrow/Extend (C), bit 29
# Overflow (V), bit 28
    # negative/less than (N), bit 31 of CPSR
    if ($cpsr->n & 1)
        printf "N "
      set $_n_flag = 1
    else
        printf "n "
      set $_n_flag = 0
    end
    # zero (Z), bit 30
    if ($cpsr->z & 1)
        printf "Z "
      set $_z_flag = 1
    else
        printf "z "
      set $_z_flag = 0
    end
    # Carry/Borrow/Extend (C), bit 29
    if ($cpsr->c & 1)
        printf "C "
      set $_c_flag = 1
    else
        printf "c "
      set $_c_flag = 0
    end
    # Overflow (V), bit 28
    if ($cpsr->v & 1)
        printf "V "
        set $_v_flag = 1
    else
        printf "v "
        set $_v_flag = 0
    end
    # Sticky overflow (Q), bit 27    
    if ($cpsr->q & 1)
        printf "Q "
        set $_q_flag = 1
    else
        printf "q "
        set $_q_flag = 0
    end
    # Java state bit (J), bit 24
    # When T=1:
    # J = 0 The processor is in Thumb state.
    # J = 1 The processor is in ThumbEE state.
    if ($cpsr->j & 1)
        printf "J "
        set $_j_flag = 1
    else
        printf "j "
        set $_j_flag = 0
    end
    # Data endianness bit (E), bit 9
    if ($cpsr->e & 1)
        printf "E "
        set $_e_flag = 1
    else
        printf "e "
        set $_e_flag = 0
    end
    # Imprecise abort disable bit (A), bit 8
    # The A bit is set to 1 automatically. It is used to disable imprecise data aborts. 
    # It might not be writable in the Nonsecure state if the AW bit in the SCR register is reset.
    if ($cpsr->a & 1)
        printf "A "
        set $_a_flag = 1
    else
        printf "a "
        set $_a_flag = 0
    end
    # IRQ disable bit (I), bit 7
    # When the I bit is set to 1, IRQ interrupts are disabled.
    if ($cpsr->i & 1)
        printf "I "
        set $_i_flag = 1
    else
        printf "i "
        set $_i_flag = 0
    end
    # FIQ disable bit (F), bit 6
    # When the F bit is set to 1, FIQ interrupts are disabled. 
    # FIQ can be nonmaskable in the Nonsecure state if the FW bit in SCR register is reset.
    if ($cpsr->f & 1)
        printf "F "
        set $_f_flag = 1
    else
        printf "f "
        set $_f_flag = 0
    end
    # Thumb state bit (F), bit 5
    # if 1 then the processor is executing in Thumb state or ThumbEE state depending on the J bit
    if ($cpsr->t & 1)
        printf "T "
        set $_t_flag = 1
    else
        printf "t "
        set $_t_flag = 0
    end
    # TODO: GE bit ?
end
document flagsarm
Syntax: flagsarm
| Auxiliary function to set ARM cpu flags.
end

define flagsx86
    # OF (overflow) flag
    if (((unsigned int)$eflags >> 0xB) & 1)
        printf "O "
        set $_of_flag = 1
    else
        printf "o "
        set $_of_flag = 0
    end
    # DF (direction) flag
    if (((unsigned int)$eflags >> 0xA) & 1)
        printf "D "
    else
        printf "d "
    end
    # IF (interrupt enable) flag
    if (((unsigned int)$eflags >> 9) & 1)
        printf "I "
    else
        printf "i "
    end
    # TF (trap) flag
    if (((unsigned int)$eflags >> 8) & 1)
        printf "T "
    else
        printf "t "
    end
    # SF (sign) flag
    if (((unsigned int)$eflags >> 7) & 1)
        printf "S "
        set $_sf_flag = 1
    else
        printf "s "
        set $_sf_flag = 0
    end
    # ZF (zero) flag
    if (((unsigned int)$eflags >> 6) & 1)
        printf "Z "
      set $_zf_flag = 1
    else
        printf "z "
      set $_zf_flag = 0
    end
    # AF (adjust) flag
    if (((unsigned int)$eflags >> 4) & 1)
        printf "A "
    else
        printf "a "
    end
    # PF (parity) flag
    if (((unsigned int)$eflags >> 2) & 1)
        printf "P "
      set $_pf_flag = 1
    else
        printf "p "
      set $_pf_flag = 0
    end
    # CF (carry) flag
    if ((unsigned int)$eflags & 1)
        printf "C "
      set $_cf_flag = 1
    else
        printf "c "
      set $_cf_flag = 0
    end
    printf "\n"
end
document flagsx86
Syntax: flagsx86
| Auxiliary function to set X86/X64 cpu flags.
end

define flags
    # call the auxiliary functions based on target cpu
    if $ARM == 1
        flagsarm
    else
        flagsx86
    end
end
document flags
Syntax: flags
| Print flags register.
end

define eflags
    if $ARM == 1
        printf "     N <%d>  Z <%d>  C <%d>  V <%d>",\
               ($cpsr->n & 1), ($cpsr->z & 1), \
               ($cpsr->c & 1), ($cpsr->v & 1)
        printf "  Q <%d>  J <%d>  GE <%d>  E <%d>  A <%d>",\
               ($cpsr->q & 1), ($cpsr->j & 1),\
               ($cpsr->ge), ($cpsr->e & 1), ($cpsr->a & 1)
        printf "  I <%d>  F <%d>  T <%d> \n",\
               ($cpsr->i & 1), ($cpsr->f & 1), \
               ($cpsr->t & 1)
     else
        printf "     OF <%d>  DF <%d>  IF <%d>  TF <%d>",\
               (((unsigned int)$eflags >> 0xB) & 1), (((unsigned int)$eflags >> 0xA) & 1), \
               (((unsigned int)$eflags >> 9) & 1), (((unsigned int)$eflags >> 8) & 1)
        printf "  SF <%d>  ZF <%d>  AF <%d>  PF <%d>  CF <%d>\n",\
               (((unsigned int)$eflags >> 7) & 1), (((unsigned int)$eflags >> 6) & 1),\
               (((unsigned int)$eflags >> 4) & 1), (((unsigned int)$eflags >> 2) & 1), ((unsigned int)$eflags & 1)
        printf "     ID <%d>  VIP <%d> VIF <%d> AC <%d>",\
               (((unsigned int)$eflags >> 0x15) & 1), (((unsigned int)$eflags >> 0x14) & 1), \
               (((unsigned int)$eflags >> 0x13) & 1), (((unsigned int)$eflags >> 0x12) & 1)
        printf "  VM <%d>  RF <%d>  NT <%d>  IOPL <%d>\n",\
               (((unsigned int)$eflags >> 0x11) & 1), (((unsigned int)$eflags >> 0x10) & 1),\
               (((unsigned int)$eflags >> 0xE) & 1), (((unsigned int)$eflags >> 0xC) & 3)
     end
end
document eflags
Syntax: eflags
| Print eflags register.
end

# -*- vim: set sts=2 sw=2 et fdm=marker: -*-
