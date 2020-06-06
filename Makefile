FASM  = fasm

mine.exe: mine.asm
	$(FASM) $? $@

clean:
	-rm *.exe
