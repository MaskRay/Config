import os
from fabric.api import local
from fabric.context_managers import lcd

def update_all():
    def add_command(path):
        cmd = ''
        if os.path.isdir(os.path.join(path, '.git')):
            cmd = 'git pull'
        elif os.path.isdir(os.path.join(path, '.hg')):
            cmd = 'hg pull -u'
        elif os.path.isdir(os.path.join(path, '.svn')):
            cmd = 'svn update'
        return (path, cmd)

    dirs = [add_command(d) for d in os.listdir('.') if os.path.isdir(d)]

    for path, cmd in dirs:
        if not cmd:
            continue

        with lcd(path):
            local(cmd)
