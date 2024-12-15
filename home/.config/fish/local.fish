for i in /tmp/Debug/bin/*
  abbr my(string split -r -m1 / $i)[2] $i
  abbr rr(string split -r -m1 / $i)[2] "rr record $i"
end
alias myob=/tmp/Debug/bin/llvm-objdump
alias myopt=/tmp/Debug/bin/opt
alias rrmc="rr record /tmp/Debug/bin/llvm-mc"
alias rrob="rr record /tmp/Debug/bin/llvm-objdump"
alias rrlld="rr record /tmp/Debug/bin/ld.lld"

for i in /tmp/Rel/bin/*
  abbr f(string split -r -m1 / $i)[2] $i
end
alias fob=/tmp/Rel/bin/llvm-objdump
