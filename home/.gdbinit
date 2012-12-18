python
import sys
sys.path.insert(0, '/usr/share/gcc-data/x86_64-pc-linux-gnu/4.5.2/gcc-4.5.2/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)
end

set history save on
set history size 1000
set history filename ~/.gdb_history
