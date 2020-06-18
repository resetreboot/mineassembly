Minesweeper in Assembly
=======================

Hello, this is my attempt to build an x86 assembly Minesweeper for DOS, 
in plain 16 bits glory and learning to use mode 13h.

Compile
=======

This project has been built using flat assembler and has a Makefile handy
to make it easy to be compiled, just do `make` and it'll be built as an
`.exe` for you to run either on your VM, your old DOS machine or as I do,
running it inside [DOSBox](https://www.dosbox.com/)

Play
====

Simply invoke the game inside DOS, wait for the map to generate. You can
move the cursor around with the arrow key, the space reveals a cell and
with the left Alt key you can put a flag. 
