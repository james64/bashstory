# bashstory
Helper setup for browsing bash history. Also my playground project to play with Haskell.

## My setup
Run `stack install` to put bashstory binary on your PATH. Then append the following to bashrc:

```bash
export HISTCONTROL=ignoredups # replace last cmd by this one if equal
export HISTSIZE=100000        # how many cmds to remember in living bash shell
export HISTFILESIZE=100000    # how many cmds to store in history file
export HISTTIMEFORMAT="[%F %T] "
shopt -s histappend           # append to history, don't overwrite it, otherwise cmds are lost

# Artificial history entry to mark start of new session
SESSION_DELIMITER='### Session ###'
PROMPT_COMMAND_OLD="$PROMPT_COMMAND"
PROMPT_COMMAND="history -s $SESSION_DELIMITER; PROMPT_COMMAND='$PROMPT_COMMAND_OLD'; $PROMPT_COMMAND"

alias hist="history | bashstory-exe --session-delim $SESSION_DELIMITER"
```

## Commands
* `hist` - show whole history list
* `hist -c` - show cmds from current session only


## Todo
* history during last amounth of minutes/hours
* include cmds from other running bash instances
* view of how different sessions evolved over time
