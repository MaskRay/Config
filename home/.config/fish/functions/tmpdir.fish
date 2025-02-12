# https://github.com/ansemjo/dotfiles/blob/main/fish/functions/tmpdir.fish
function tmpdir --description "Create and switch into an emphemeral directory"
    # create a tmpdir and cd into it
    set -l tmp (mktemp --tmpdir -d tmpdir-XXXXXX)
    cd $tmp; and echo >&2 \
        (set_color yellow)"tmpdir:" $tmp "will be removed on exit" \
        (set_color normal)

    # spawn a new shell, store the exit status and return to previous dir
    fish $argv
    set -l ret $status
    cd $dirprev[-1]

    # after exit, check if there are mounts inside tmpdir
    if awk '{ print $2 }' /etc/mtab | grep $tmp
        echo >&2 \
            (set_color red)"refusing to purge $tmp due to mounts!" \
            (set_color normal)
    else
        # if clear, purge directory
        echo >&2 \
            (set_color red)"purge $tmp ..." \
            (set_color normal)
        rm -rf $tmp
    end

    # return subshell exit status
    return $ret
end
