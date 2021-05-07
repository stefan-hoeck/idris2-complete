# idris2-complete
bash tab completion for Idris2. This is still very much work
in progress, but you can already give it a try. Eventually this
functionality will be added to the `idris2` executable itself,
but for ease of development this is a free-standing project
right now.

## Building and activating autocompletion

Build the executable:
```
idris2 --build complete.ipkg
```
Adjust the `complete_idris.sh` script, so that the following
line points to the executable:

```
  COMPREPLY=( $(/home/me/idris/complete/build/exec/idris2_complete) )
```

### Bash
If you use `bash` as your shell, all you have to do is
source `complete_idris.sh` (after having adjusted the path
to the built executable as described above):

```
$> source complete_idris.sh
```

As an alternative, you can also automatically do this
by adding the corresponding line to your `.bashrc` file.

### ZSH
The `zsh` shell supports autocompletion via `bash`.
All you need to to is add the following lines to your `.zshrc`
file:

```
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
```

Now you can source `complete_idris.sh` (make sure to first
adjust the path to the executable as described above):

```
$> source complete_idris.sh
```

As an alternative, you can also automatically do this
by adding the corresponding line to your `.zshrc` file.

## Usage

After building and activating bash autocompletion as described
above, just enter part of a call to `idris2` and press tab once
or twice whenever you feel like it.

## Project TODO

- [ ] Autocompletion
  - [x] Complete command line option names
  - [x] Lookup and complete Idris2 package names
  - [x] Complete directory paths
  - [x] Complete file names
  - [x] Complete available backends (only the defaults so far)
  - [x] Complete package file names
  - [ ] Complete console width
  - [x] Complete log level
  - [ ] Complete available codegen directives
  - [ ] Support completion in the middle of a command
