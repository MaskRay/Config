#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    const char cmds[][11] = {"route", "ifconfig", "ip", "wpa_gui", ""};
    const char prefixes[][11] = {"/sbin/", "/sbin/", "/bin/", "/usr/bin/", ""};
    char cmd[1256], *p = strrchr(argv[0], '/');
    if (!p) p = argv[0];
    else ++p;
    for (const char (*q)[11] = cmds; **q; ++q)
        if (strcmp(p, *q)) {
            strcpy(cmd, prefixes[q-cmds]);
            strcat(cmd, p);
            execv(cmd, argv);
            return 2;
        }
    return 1;
}
