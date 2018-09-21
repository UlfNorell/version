#!/bin/bash

_version()
{
  local cur cmd
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  cmd="${COMP_WORDS[@]:0:COMP_CWORD} --complete"
  COMPREPLY=( $(compgen -W "`${cmd[@]}`" -- ${cur}) )
  return 0
}

complete -F _version version

