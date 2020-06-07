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
FIELDX  = 02h
FIELDY  = 02h

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
		call    createminefield ;Initialize the map with mines
		call    drawfield       ;Draw the map
		jmp     mainloop		;Main event and game loop


; Draw the field
drawfield:
		mov     cx, 00h
		mov     ax, 00h
		call    getbmp
        ; mov     bx, covercell
drawfieldloop:
		push    cx
		push    ax
		add     cx, FIELDY          ; Make sure we get the field displaced correctly
		add     ax, FIELDX
		call    drawbox
		pop     ax
		pop     cx
		inc     cx
		call    getbmp
		cmp     cx, 0Ah
		jne     drawfieldloop
		mov     cx, 0'h
		inc     ax
		call    getbmp
		cmp     ax, 0Ah
		jne     drawfieldloop
		ret    
getbmp:
		mov     bx, map	    	   ; Store the base of the map in BX
		push	ax				   ; Don't mangle AX
		mov     ax, COLUMNS        ; How many columns an AX
		mul     cx                 ; So we now multiply CX (Y coord) by COLUMNS
		add     bx, cx             ; Add it to the pointer
		pop     ax				   ; Recover AX (X coord)
		add     bx, ax			   ; Add the X coord to BX, now we can read
		cmp     bx, 0009h		   ; There's a mine
		jne     nomine	
		mov     bx, minebmp      ; Point to the mine bitmap
		ret
nomine:	
		mov     bx, covercell		;  Point to the covered cell
		ret     

;Random subroutines here
initseed:
		mov     ah, 02Ch		;Select Get System Time function
		int     021h			;Call DOS 
		add     cx, dx          ;Add the seconds and milliseconds to add more randomness
		mov     word [seed], cx ;In CX we have our new seed
		ret

random:
		mov		ax, 25173		;LCG multiplier
		mul     word [seed]		;DX:AX  = LCG multiplier * seed
		add     ax, 13849       ;Add LCG increment value
		mov     word [seed], ax ;Update seed, AX has the random number now
		ret 

;This is thought to pick up from the registers the address of what we need to draw
;Draw it, and return. The params are:
;- AX -> X position
;- CX -> Y position
;- BX -> base memory address of the bitmap
drawbox:
		push    ax              ;Save the X value for later
		mov     ax, cx          ;Prepare for multiplications
		mov     dx, 0A00h
		mul     dx
		mov     cx, ax          ;Retrieve the line into CX
        pop     ax              ;Retrieve the X value into AX register
		mov     dx, 08h
		mul     dx          	;We multiply the X value too for the 8x8
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

;This function populates the minefield with mines
createminefield:
		call    initseed		;Make sure we have a good seed 
		mov     dx, 00h			;DX will be our pointer
		mov     bx, [map]		;BX our base pointer
cmfloop:
		push    dx				;Save our counters before generating random number
		push    bx
		call    random          ;This leaves a random num in AX
		pop     bx
		pop     dx
		cmp     al, 0F0h		;Compare if it's greater than 240
		jg		putmine
continuecreateminefield:
        inc     bx
		inc     dx
		cmp     dx, COLUMNS * LINES  ; Check we have done the whole map
		jne     cmfloop
		cmp     byte [minecount], 10	; Check we've put all mines!
		jne     createminefield
finishcreateminefield:
		ret								; All is ok

putmine:
		cmp     byte [minecount], 10
		je      finishcreateminefield
		cmp     word [bx], 00009h		;Check memory for placed mine 
		je      continuecreateminefield
		mov     word [bx], 00009h
		inc     byte [minecount]
		jmp     continuecreateminefield
		
;This routine checks the map and puts numbers down given the 
;amount of 
;calculateminenumbers:
;		mov		bx, [map]
;		mov     dx, 00h

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
seed:				dw      0
minecount:          db      0
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

emptycell:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 17h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

minebmp:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 10h, 17h, 10h, 17h, 17h, 10h, 0fh 
                    db      14h, 17h, 10h, 10h, 10h, 10h, 17h, 0fh 
                    db      14h, 17h, 10h, 10h, 0fh, 10h, 10h, 0fh 
                    db      14h, 10h, 10h, 10h, 10h, 10h, 17h, 0fh 
                    db      14h, 17h, 10h, 10h, 10h, 10h, 17h, 0fh 
                    db      14h, 10h, 17h, 17h, 10h, 17h, 10h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

flagbmp:			db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 
                    db      0fh, 17h, 17h, 04h, 10h, 17h, 17h, 14h 
                    db      0fh, 17h, 04h, 04h, 10h, 17h, 17h, 14h 
                    db      0fh, 04h, 04h, 04h, 10h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 10h, 17h, 17h, 14h 
                    db      0fh, 17h, 17h, 17h, 10h, 17h, 17h, 14h 
                    db      0fh, 17h, 10h, 10h, 10h, 10h, 10h, 14h 
                    db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
