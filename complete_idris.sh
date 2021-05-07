#!/usr/bin/env bash

_idris2_completions()
{
  COMPREPLY=($(/home/gundi/idris/complete/build/exec/idris2_complete $2 $3))
}

complete -F _idris2_completions -o dirnames idris2
