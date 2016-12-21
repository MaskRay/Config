# Jump to a character in the command line
() {
    typeset -g ZSH_JUMP_TARGET_CHOICES
    typeset -g ZSH_JUMP_TARGET_STYLE

    : ${ZSH_JUMP_TARGET_CHOICES:="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"}
    : ${ZSH_JUMP_TARGET_STYLE:="fg=red,bold,underline"}

    local orig_buffer=$BUFFER
    local -a orig_region_highlight
    orig_region_highlight=("${region_highlight[@]}")

    {
        zle -R "Char:"
        local char
        read -k char

        integer buffer_idx
        local target_choices_len=$#ZSH_JUMP_TARGET_CHOICES
        local target_idx=1
        local target_choice
        local -A target_to_buffer_idx
        local start
        region_highlight=()
        while buffer_idx=${BUFFER[(ib:buffer_idx+1:)$char]}
              (( buffer_idx <= $#BUFFER ))
        do
            target_choice=${ZSH_JUMP_TARGET_CHOICES[target_idx]}
            BUFFER[buffer_idx]=$target_choice
            target_to_buffer_idx[$target_choice]=$buffer_idx
            (( start = buffer_idx - 1 ))
            region_highlight+=( "$start $buffer_idx $ZSH_JUMP_TARGET_STYLE" )
            (( target_idx++ ))
            if (( target_idx > target_choices_len )); then
                break
            fi
        done

        if (( target_idx == 1 )); then
            return 0
        fi

        zle -R "Target:"
        read -k target_choice

        if (( ${+target_to_buffer_idx[$target_choice]} )); then
            (( CURSOR = target_to_buffer_idx[$target_choice] - 1 ))
        fi

    } always {
        BUFFER=$orig_buffer
        region_highlight=("${orig_region_highlight[@]}")
        zle -cR ''
    }
}
