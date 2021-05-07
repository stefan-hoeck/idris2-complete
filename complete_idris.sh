#!/usr/bin/env bash

_idris2_completions()
{
  export IDRIS_WORDS="${COMP_WORDS[*]}"
  COMPREPLY=( $(/home/gundi/idris/complete/build/exec/complete) )
}

complete -F _idris2_completions idris2
