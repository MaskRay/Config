#!/usr/bin/env python3
import gzip, re, subprocess

def encode(x):
    return '<U{:08X}>'.format(x)

width = {}
mx = 0
is_width = False

# read original width information from charmaps/UTF-8
with gzip.open('/usr/share/i18n/charmaps/UTF-8.gz') as f:
    lines = f.read().decode().splitlines()
    for line in lines:
        if line == 'WIDTH':
            is_width = True
        elif line == 'END WIDTH':
            is_width = False
        elif is_width:
            m = re.match(r'<U(\w+)>(?:\.\.\.<U(\w+)>)?\t(\d)$', line)
            if m:
                head = int(m.group(1), 16)
                last = int(m.group(2), 16) if m.group(2) else head
                mx = max(mx, last)
                for code in range(head, last+1):
                    width[code] = m.group(3)

# incomplete list of full-width characters
for i, j in [(0x25a0, 0x27c0),      # Geometric Shapes, Miscellaneous Symbols, Dingbats
             (0x2b00, 0x2bf0),      # Miscellaneous Symbols and Arrows
             (0x1f300, 0x1f9c1)]:   # Miscellaneous Symbols and Pictographs ... Supplemental Symbols and Pictographs
    for code in range(i, j):
        width[code] = 2

# print new charmaps/UTF-8
with gzip.open('/usr/share/i18n/charmaps/UTF-8.gz', 'wb') as f:
    for line in lines:
        if line == 'WIDTH':
            is_width = True
            f.write((line+'\n').encode())
            i = 0
            while i <= mx:
                if i in width:
                    j = i+1
                    while j in width and width[i] == width[j]:
                        j += 1
                    if i == j-1:
                        f.write('{}\t{}\n'.format(encode(i), width[i]).encode())
                    else:
                        f.write('{}...{}\t{}\n'.format(encode(i), encode(j-1), width[i]).encode())
                    i = j
                else:
                    i += 1
        elif line == 'END WIDTH':
            is_width = False
            f.write((line+'\n').encode())
        elif not is_width:
            f.write((line+'\n').encode())

subprocess.run('localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8', shell=True)
