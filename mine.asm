format MZ			;Specify the 
org 100h                        ;specify .COM file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

COLUMNS = 8
LINES   = 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
        ; mov     al,13h          ;AX=0000 at program start
        ; int     10h             ;init mode 13h
        ; push    word 0A000h     ;Requires 80186 or higher to PUSH IMMED
        ; pop     es              ;ES now points to mode 13h screen segment
		; push    word 00000h     ;
        ; pop     si

mainloop:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is where you do your mega-amazing tiny program.
;Write 8-bit values to A000:0000 to draw some pixels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
		;Check for user wanting to leave by pressing ESC
        in      al,60h                  ;read whatever is at keyboard port; looking for ESC which is #1
		cmp     al,48h
		je      upkey
		cmp     al,4bh
		je      leftkey
        cmp     al,4dh
        je      rightkey
        cmp     al,50h
        je      downkey
		cmp     al,39h			
		je      space
        cmp     al,38h
        je      lalt
		mov     dword [lastkeypressed], 0h	;Reset the current key as it is not one we are checking
        dec     al						;if ESC, AL now 0
        jnz     mainloop        		;fall through if 0, jump otherwise
		
exit:
        mov     al,03           		;AX=0000 due to mainloop exit condition
        int     10h             		;Switch back to text mode as a convenience
		mov     ah,4Ch					;Function to exit, now we are an EXE, do it correctly
		mov     al,00					;Exit code as 0, everything went well
		int     21h

upkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
        mov     ah,02
        mov     dl,31h
        int     21h
        jmp     mainloop

leftkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
        mov     ah,02
        mov     dl,32h
        int     21h
        jmp     mainloop

rightkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
        mov     ah,02
        mov     dl,33h
        int     21h
        jmp     mainloop

downkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
        mov     ah,02
        mov     dl,34h
        int     21h
        jmp     mainloop

space:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
        mov     ah,02
        mov     dl,39h
        int     21h
        jmp     mainloop

lalt:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
        mov     ah,02
        mov     dl,38h
        int     21h
        jmp     mainloop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Data segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lastkeypressed:		db		0
