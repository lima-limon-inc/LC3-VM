# LC3 Virtual Machine
This repository (will) contains an implementation of the LC3 virtual. This is the coolest virtual machine, since when it's written down in lowercase it kinda looks like `lc3`🧊.

## Installation instructions
Install the repository:
```shell
git clone --recursive git@github.com:lima-limon-inc/LC3-VM.git && cd LC3-VM/
git submodule update --init --recursive
```

Note: This will also install the `laser` lc3 compiler.

## Compile the project

```shell
make build
```

## Run the project

```shell
make run FILENAME=examples/<file>.asm DEBUGMODE=f
```

## Interpreter mode

You can run the VM in a GDB-like interpreter mode with
```shell
make run FILENAME=examples/<file>.asm DEBUGMODE=t
```
