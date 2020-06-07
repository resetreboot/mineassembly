format MZ			            ;Specify the .EXE format
entry MainSeg:start
org    0100h
stack  $3000

;org 100h                        ;specify origin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

COLUMNS = 8
LINES   = 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

segment MainSeg
use16

start:
        mov     al,13h          ;AX=0000 at program start
        int     10h             ;init mode 13h
        push    word 0A000h     ;Requires 80186 or higher to PUSH IMMED
        pop     es              ;ES now points to mode 13h screen segment
		mov     ax, DataSeg     
		mov     ds, ax          ;DS now points to our data segment
		; Test draw a box
		mov     cx, 00h         ;Y is 0
		mov     ax, 00h         ;X is 0
		mov     bx, covercell
		call    drawbox         ;Draw a box with this data
		jmp     mainloop

;This is thought to pick up from the registers the address of what we need to draw
;Draw it, and return. The params are:
;- AX -> X position
;- CX -> Y position
;- BX -> base memory address of the bitmap
drawbox:
		push    ax              ;Save the X value for later
		mov     ax, cx          ;Prepare for multiplications
		mov     cl, 8
		mul     cl      		;This routine maintains the 8x8 positions
		mov     cx, 320         ;
		mul     cl      	    ;And now we count whole lines for the address
		mov     cx, ax          ;Retrieve the line into CX
        pop     ax              ;Retrieve the X value into AX register
		mov     dl, 8
		mul     dl          	;We multiply the X value too for the 8x8
		add     cx, ax          ;Now we've put in AX the pointer value 
		mov     si, cx          ;Set SI to the correct address we'll draw
		mov     cx, 00
		mov     ax, 00
		mov     dx, 00

drawloop:
		mov     al, byte [ds:bx]		;Current address in BX
		mov     byte [es:si], al        ;Put the pixel into RAM
		inc     bx
		inc     cx
		inc     dx
		cmp     dx, 08h           ;Sort of modulo
		je      jumplinedraw    ;Time to increment SI in a more complex way
		inc     si

continuecomp:
		cmp     cx, 040h        ;Compare CX to hex 64 (8x8)
		jne     drawloop        ;Continue
		ret                     ;Return

jumplinedraw:
		mov     dx, 00h
		add     si, 313			;Jump a whole line
		jmp     continuecomp    ;Continue the routine
		

mainloop:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is where you do your mega-amazing tiny program.
;Write 8-bit values to A000:0000 to draw some pixels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
		;Check for user input or wanting to leave by pressing ESC
        in      al,60h						;read whatever is at keyboard port; looking for ESC which is #1
		; cmp     al,48h
		; je      upkey
		; cmp     al,4bh
		; je      leftkey
        ; cmp     al,4dh
        ; je      rightkey
        ; cmp     al,50h
        ; je      downkey
		; cmp     al,39h			
		; je      space
        ; cmp     al,38h
        ; je      lalt
		; mov     word [lastkeypressed], 0h	;Reset the current key as it is not one we are checking
        dec     al							;if ESC, AL now 0
        jnz     mainloop					;fall through if 0, jump otherwise
		
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

segment DataSeg
use16

lastkeypressed:		db		0
;Game map. First 8 bytes are for the cell status
;the last 8 bytes are for the cell contents.
;
;Cell status: 0 covered, 1 uncovered, 2 flagged
;Cell content: 0 empty, 1-8 number, 9 mine
map:				dw		COLUMNS * LINES dup ?

covercell:			db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 
                    db      0fh, 17h, 17h, 17h, 17h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 17h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 17h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 17h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 17h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 17h, 17h, 17h, 14h 
                    db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
