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
FIELDX  = 16
FIELDY  = 2
MAXMINES = 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

segment MainSeg
use16

start:
        mov     al,13h                   ;AX=0000 at program start
        int     10h                      ;init mode 13h
        push    word 0A000h              ;Requires 80186 or higher to PUSH IMMED
        pop     es                       ;ES now points to mode 13h screen segment
		mov     ax, DataSeg              
		mov     ds, ax                   ;DS now points to our data segment
		call    createminefield          ;Initialize the map with mines
		call    calculateminenumbers     ;Populate the numbers
		call    drawfield                ;Draw the map
		jmp     mainloop		         ;Main event and game loop


; Draw the field
drawfield:
		mov     cx, 00h
		mov     ax, 00h
drawfieldloop:
		push    cx
		push    ax
		call    getbmp
		add     cx, FIELDY          ; Make sure we get the field displaced correctly
		add     ax, FIELDX
		call    drawbox
		pop     ax
		pop     cx
		inc     cx
		cmp     cx, LINES
		jne     drawfieldloop
		mov     cx, 0h
		inc     ax
		cmp     ax, COLUMNS
		jne     drawfieldloop
		mov     ax, [cursorx]
		add     ax, FIELDX
		mov     cx, [cursory]
		add     cx, FIELDY          ; Make sure we get the cursor displaced correctly
		mov     bx, cursorbmp
		call    drawbox
		ret    
getbmp:
		push    cx                         ; Store the values before scr$%&ing them
		push    ax
		call    peektable
		pop     ax                         ; Recover the values of AX and BX
		pop     cx

;;;;; Normal reading of the field, covering the cells

		cmp     dh, 01h                   ; Covered cell
		je      iscovered
		cmp     dh, 02h                   ; Flagged cell
		je      isflagged
		cmp     dx, 00009h		           ; There's a mine
		je      mine	
		cmp     dx, 00008h
		je      puteight
		cmp     dx, 00007h
		je      putseven
		cmp     dx, 00006h
		je      putsix
		cmp     dx, 00005h
		je      putfive
		cmp     dx, 00004h
		je      putfour
		cmp     dx, 00003h
		je      putthree
		cmp     dx, 00002h
		je      puttwo
		cmp     dx, 00001h
		je      putone

;;;;;;;;;;;;;;; Uncomment this to see the field uncovered
		; cmp     dh, 02h                   ; Flagged cell
		; je      isflagged
		; cmp     dx, 00109h		           ; There's a mine
		; je      mine	
		; cmp     dx, 00108h
		; je      puteight
		; cmp     dx, 00107h
		; je      putseven
		; cmp     dx, 00106h
		; je      putsix
		; cmp     dx, 00105h
		; je      putfive
		; cmp     dx, 00104h
		; je      putfour
		; cmp     dx, 00103h
		; je      putthree
		; cmp     dx, 00102h
		; je      puttwo
		; cmp     dx, 00101h
		; je      putone

		mov     bx, emptycell   ; Point to the emptycell bitmap
		ret
iscovered:	
		mov     bx, covercell	;  Point to the covered cell
		ret     
isflagged:
		mov     bx, flagbmp     ; Point to the flag bitmap
		ret
mine:	
		mov     bx, minebmp		;  Point to the mine cell
		ret     
putone:	
		mov     bx, numone		;  Point to the corresponding number cell
		ret     
puttwo:	
		mov     bx, numtwo		;  Point to the corresponding number cell
		ret     
putthree:	
		mov     bx, numthree		;  Point to the corresponding number cell
		ret     
putfour:	
		mov     bx, numfour		;  Point to the corresponding number cell
		ret     
putfive:	
		mov     bx, numfive		;  Point to the corresponding number cell
		ret     
putsix:	
		mov     bx, numsix		;  Point to the corresponding number cell
		ret     
putseven:	
		mov     bx, numseven		;  Point to the corresponding number cell
		ret     
puteight:	
		mov     bx, numeight		;  Point to the corresponding number cell
		ret     

;Random subroutines here
initseed:
		mov     ah, 02Ch		;Select Get System Time function
		int     021h			;Call DOS 
		add     cx, dx          ;Add the seconds and milliseconds to add more randomness
		mov     word [seed], cx ;In CX we have our new seed
		ret

random:
		mov		ax, 25173		     ;LCG multiplier
		mul     word [seed]		     ;DX:AX  = LCG multiplier * seed
		add     ax, 13849            ;Add LCG increment value
		mov     word [seed], ax      ;Update seed, AX has the random number now
		ret 

;This is thought to pick up from the registers the address of what we need to draw
;Draw it, and return. The params are:
;- AX -> X position
;- CX -> Y position
;- BX -> base memory address of the bitmap
drawbox:
		push    ax              ;Save the X value for later
		mov     ax, cx          ;Prepare for multiplications
		mov     dx, 320 * 8     ;The box is 8x8, the screen width is 320
		mul     dx
		mov     cx, ax          ;Retrieve the line into CX
        pop     ax              ;Retrieve the X value into AX register
		mov     dx, 8			;The box is 8x8, each box is 8 pixel wide
		mul     dx          	;We multiply the X value too for the 8x8
		add     cx, ax          ;Now we've put in AX the pointer value 
		mov     si, cx          ;Set SI to the correct address we'll draw
		mov     cx, 00
		mov     ax, 00
		mov     dx, 00
drawloop:
		mov     al, byte [ds:bx]		;Current address in BX
		cmp     al, 0ffh				;This is transparent pixel, don't draw
		je      dontdraw
		mov     byte [es:si], al        ;Put the pixel into RAM
dontdraw:
		inc     bx
		inc     cx
		inc     dx
		cmp     dx, 8           ;Sort of modulo
		je      jumplinedraw    ;Time to increment SI in a more complex way
		inc     si
continuecomp:
		cmp     cx, COLUMNS * LINES        ;Compare CX the max the field will be
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
		mov     bx, map	    	;BX our base pointer
cmfloop:
		push    dx				;Save our counters before generating random number
		push    bx
		call    random          ;This leaves a random num in AX
		pop     bx              ;Recover our counters
		pop     dx
		cmp     al, 0FFh		;Compare if it's greater than 240
		je		putmine			;Put a mine here
continuecreateminefield:
        add     bx, 02h
		inc     dx
		cmp     dx, COLUMNS * LINES  ; Check we have done the whole map
		jl      cmfloop
		cmp     byte [minecount], MAXMINES	; Check we've put all mines!
		jl      createminefield
finishcreateminefield:
		ret								; All is ok
putmine:
		cmp     byte [minecount], MAXMINES
		jge     finishcreateminefield
		cmp     word [ds:bx], 00109h		;Check memory for placed mine 
		je      continuecreateminefield
		inc     byte [minecount]
		mov     word [ds:bx], 00109h		;Place mine
		jmp     continuecreateminefield

;This routine returns in DX the value of the cell pointed by CX (Y coord) and
;AX (X coord)
;Mangles AX, BX, CX and DX
peektable:
		mov     dx, 0						;In case we return prematurely, we return nothing
		cmp     ax, COLUMNS					;If we are out of bounds, return
		jge     peekend
		cmp     cx, LINES
		jge     peekend
		push    ax							;Calculate offset
		mov     ax, cx
		mov     cx, COLUMNS
		mul     cx
		mov     cx, ax
		pop     ax
		add     ax, cx
		mov     cx, 2
		mul     cx
		mov     bx, map
		add     bx, ax
		mov		dx, word [ds:bx]			;Read     
peekend:
		ret

calcnumber:
		push    ax
		push    cx
		call    peektable
		pop     cx
		cmp     dx, 00109h
		jne     calcend
		mov     al, byte [tempnumber]
		inc     al
		mov     byte [tempnumber], al
calcend:
		pop     ax
		ret

; We don't check Y bounds as we have surrounded the field array
; with padding zeroes, rendering this calculation correct if CX
; is below or beyond the limits
checksurroundings:
		mov     byte [tempnumber], 0
		push    ax
		push    cx
		sub     cx, 1
		test    ax, ax
		jz      axiszerofirstrow
		sub     ax, 1
		call    calcnumber
		add     ax, 1
axiszerofirstrow:
		call    calcnumber
		add     ax, 1
		cmp     ax, COLUMNS
		je      axoutboundsfirstrow
		call    calcnumber
axoutboundsfirstrow:
		pop     cx
		pop     ax
		push    ax
		push    cx
		test    ax, ax
		jz      axiszerosecondrow
		sub     ax, 1
		call    calcnumber
		inc     ax
axiszerosecondrow:
		inc     ax
		cmp     ax, COLUMNS
		je      axoutboundssecondrow
		call    calcnumber
axoutboundssecondrow:
		pop     cx
		pop     ax
		push    ax
		push    cx
		add     cx, 1
		test    ax, ax
		jz      axiszerothirdrow
		sub     ax, 1
		call    calcnumber
		add     ax, 1
axiszerothirdrow:
		call    calcnumber
		add     ax, 1
		cmp     ax, COLUMNS
		je      axoutboundsthirdrow
		call    calcnumber
axoutboundsthirdrow:
		pop     cx
		mov     bx, map
		mov     ax, cx
		mov     dx, COLUMNS
		mul     dx
		mov     dx, ax
		pop     ax
		push    ax
		add     dx, ax
		mov     ax, dx
		mov     dx, 2
		mul     dx
		add     bx, ax
		pop     ax
		mov     dx, 00100h
		mov     dl, byte [tempnumber]
		mov     word [ds:bx], dx
		ret
		
;This routine checks the map and puts numbers down given the 
;amount of mines around
calculateminenumbers:
		mov     ax, 0
        mov     cx, 0
cmnloop:
		push    ax
		push    cx
		call    peektable
		pop     cx
		pop     ax
		cmp     dx, 00109h
		je      cmncontinue
		call    checksurroundings
cmncontinue:
		inc     ax
		cmp     ax, COLUMNS
		jl      cmnloop
		mov     ax, 0
		inc     cx
		cmp     cx, LINES
		jl      cmnloop
		ret

mainloop:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is where you do your mega-amazing tiny program.
;Write 8-bit values to A000:0000 to draw some pixels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
		
		;Check for user input or wanting to leave by pressing ESC
        in      al,60h						;read whatever is at keyboard port; looking for ESC which is #1
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
		mov     word [lastkeypressed], 0h	;Reset the current key as it is not one we are checking
        dec     al							;if ESC, AL now 0
        jnz     mainloop					;fall through if 0, jump otherwise
		
exit:
        mov     al,03           		;AX=0000 due to mainloop exit condition
        int     10h             		;Switch back to text mode as a convenience
		mov     ah,4Ch					;Function to exit, now we are an EXE, do it correctly
		mov     al,00					;Exit code as 0, everything went well
		int     21h

gameover:
		call    drawfield
		mov     ax, 0
		mov     ah, 02Ch                ;Get current system time
		int     021h
		mov     bh, dh                  ;Get current seconds
waitloop:
		mov     ah, 02Ch
		int     021h
		sub     dh, bh                  ;Get seconds elapsed
		cmp     dh, 2
		jl      waitloop
		mov     ah, 0
        mov     al,03           		;AX=0000 due to mainloop exit condition
        int     10h             		;Switch back to text mode as a convenience
		mov     dx, gameoverstr
		mov     ah, 09h
		int     21h
		mov     ah,4Ch					;Function to exit, now we are an EXE, do it correctly
		mov     al,00					;Exit code as 0, everything went well
		int     21h

redraw:
		call    drawfield
		jmp     mainloop

leftkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
		mov     ax, [cursorx]
		dec     ax

updatelateralmovement:
		cmp     ax, COLUMNS - 1
		jg      redraw
		cmp     ax, 0
		jl      redraw
		mov     [cursorx], ax
        jmp     redraw

rightkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
		mov     ax, [cursorx]
		inc     ax
        jmp     updatelateralmovement

upkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
		mov     cx, [cursory]
		dec     cx

updateverticalmovement:
		cmp     cx, LINES - 1
		jg      redraw
		cmp     cx, 0
		jl      redraw
		mov     [cursory], cx
        jmp     redraw

downkey:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
		mov     cx, [cursory]
		inc     cx
		jmp     updateverticalmovement

space:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
		mov     ax, [cursorx]
        mov     cx, [cursory]
		call    uncover
		mov     ax, [cursorx]
		mov     cx, [cursory]
		call    peektable
		cmp     dx, 00009h
		je      gameover
        jmp     redraw

lalt:
		cmp     ax, [lastkeypressed]
		je      mainloop
		mov     [lastkeypressed], ax
		mov     ax, [cursorx]
        mov     cx, [cursory]
		call    setflag
        jmp     redraw

getoffset:
		cmp     ax, COLUMNS					;If we are out of bounds, return
		jl      checky
		ret
checky:
		cmp     cx, LINES
		jl      inbounds
		ret
inbounds:
		push    ax							;Calculate offset
		mov     ax, cx
		mov     cx, COLUMNS
		mul     cx
		mov     cx, ax
		pop     ax
		add     ax, cx
		mov     cx, 2
		mul     cx
		mov     bx, map
		add     bx, ax
		ret

uncover:
		call    getoffset
		mov		dx, word [ds:bx]			;Read     
		mov     dh, 00h						;Set upperbit as 0
		mov     word [ds:bx], dx
		ret
		
setflag:
		call    getoffset
		mov		dx, word [ds:bx]			;Read     
		mov     dh, 02h						;Set upperbit as 0
		mov     word [ds:bx], dx
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Data segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

segment DataSeg
use16

lastkeypressed:		dw		0
seed:				dw      0
minecount:          db      0
tempnumber:         db      0
cursorx:			dw      0
cursory:			dw      0
gameoverstr:		db      "Game over!$"
youwinstr:          db      "You win!$"
;Game map. First 8 bytes are for the cell status
;the last 8 bytes are for the cell contents.
;
;Cell status: 0 covered, 1 uncovered, 2 flagged
;Cell content: 0 empty, 1-8 number, 9 mine
paddmapbf:			dw      COLUMNS dup 00h
map:				dw		COLUMNS * LINES dup 00h
paddmapaft:			dw      COLUMNS dup 00h

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

numone: 			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 17h, 01h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 01h, 01h, 17h, 17h, 0fh 
                    db      14h, 17h, 01h, 17h, 01h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 01h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 01h, 17h, 17h, 0fh 
                    db      14h, 17h, 01h, 01h, 01h, 01h, 01h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numtwo:	     		db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 02h, 02h, 02h, 17h, 0fh 
                    db      14h, 17h, 02h, 17h, 17h, 17h, 02h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 02h, 0fh 
                    db      14h, 17h, 17h, 02h, 02h, 02h, 17h, 0fh 
                    db      14h, 17h, 02h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 02h, 02h, 02h, 02h, 02h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numthree:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 04h, 04h, 04h, 17h, 0fh 
                    db      14h, 17h, 04h, 17h, 17h, 17h, 04h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 04h, 0fh 
                    db      14h, 17h, 17h, 17h, 04h, 04h, 17h, 0fh 
                    db      14h, 17h, 04h, 17h, 17h, 17h, 04h, 0fh 
                    db      14h, 17h, 17h, 04h, 04h, 04h, 17h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numfour:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 17h, 05h, 05h, 17h, 0fh 
                    db      14h, 17h, 17h, 05h, 17h, 05h, 17h, 0fh 
                    db      14h, 17h, 05h, 17h, 17h, 05h, 17h, 0fh 
                    db      14h, 17h, 05h, 05h, 05h, 05h, 05h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 05h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 05h, 05h, 05h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numfive:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 06h, 06h, 06h, 06h, 06h, 0fh 
                    db      14h, 17h, 06h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 06h, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 06h, 06h, 06h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 06h, 0fh 
                    db      14h, 17h, 06h, 06h, 06h, 06h, 17h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numsix: 			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 17h, 2bh, 2bh, 17h, 0fh 
                    db      14h, 17h, 17h, 2bh, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 2bh, 17h, 17h, 17h, 17h, 0fh 
                    db      14h, 17h, 2bh, 2bh, 2bh, 2bh, 17h, 0fh 
                    db      14h, 17h, 2bh, 17h, 17h, 17h, 2bh, 0fh 
                    db      14h, 17h, 17h, 2bh, 2bh, 2bh, 17h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numseven:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 35h, 35h, 35h, 35h, 35h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 17h, 35h, 0fh 
                    db      14h, 17h, 17h, 17h, 17h, 35h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 35h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 35h, 17h, 17h, 0fh 
                    db      14h, 17h, 17h, 17h, 35h, 17h, 17h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

numeight:			db      14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h 
                    db      14h, 17h, 17h, 40h, 40h, 40h, 17h, 0fh 
                    db      14h, 17h, 40h, 17h, 17h, 17h, 40h, 0fh 
                    db      14h, 17h, 17h, 40h, 40h, 40h, 17h, 0fh 
                    db      14h, 17h, 40h, 17h, 17h, 17h, 40h, 0fh 
                    db      14h, 17h, 40h, 17h, 17h, 17h, 40h, 0fh 
                    db      14h, 17h, 17h, 40h, 40h, 40h, 17h, 0fh 
                    db      0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh, 0fh 

cursorbmp:			db      2ah, 2ah, 2ah, 2ah, 2ah, 2ah, 2ah, 2ah 
                    db      2ah, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 2ah 
                    db      2ah, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 2ah 
                    db      2ah, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 2ah 
                    db      2ah, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 2ah 
                    db      2ah, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 2ah 
                    db      2ah, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 2ah 
                    db      2ah, 2ah, 2ah, 2ah, 2ah, 2ah, 2ah, 2ah 
