FASM  = fasm
FLAGS = -s mine.tds

mine.exe: mine.asm
	$(FASM) $(FLAGS) $? $@

clean:
	-rm *.exe
