FASM  = fasm

mine.exe: mine.asm
	$(FASM) $? $@
