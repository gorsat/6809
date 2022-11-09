* Simple macros are supported
    .macro	clc
        andcc	#$fe
    .endm
* Here's an example with a parameter
    .macro my_bne
        bne @0
    .endm

* If no address is specified (via org) then the assembled code will start at address 0.
* Likewise, if the reset vector (fffe) is not explicitly set then it defaults to 0.
* Hence, the following code will start at address 0 and be executed upon reset:
        lds     #heap   ;stack resides just below the heap
        ldx     #heap   ;our data is stored at the bottom of the heap
        ldy     #heap+4
        ldu     #output ;just using u as another index register
        ldb     #4
        jsr     mbadd
        swi

* Multi-byte addition 
* x - pointer to first number (lsb first)
* y - pointer to second number
* u - pointer to storage for result
* b - count of bytes to add 
* If overflow occurs then carry is set upon return
mbadd   clc
mba001  lda     ,x+
        adca    ,y+
        sta     ,u+
        decb
        my_bne  mba001
        rts

heap    org     $1000
        fcb     $ff,$ff,$ff,$7f
        fcb     1,0,0,0
output  rmb     4

;
; The lines beginning with ";!" each define a TestCriterion (see test.rs)
; When the program is run with the --run (and not the --debug) option
; these criteria will be checked after the program is run.
;! output = #0
;! output+1 = #0
;! output+2 = #0
;! output+3 = #$80
;! x = #heap+4
;! s = #heap