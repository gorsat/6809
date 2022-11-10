    .macro	clc
        andcc	#$fe
    .endm

        org     $fffe
        fdb     start

code    org     $1000

* add two multi-byte BCD numbers pointed to by x and y (least significant byte first)
* result stored at x
addbcd  clc
abcd01  lda     ,x
        adca    ,y+
        daa
        sta     ,x+
        decb
        bne     abcd01
        rts

data    fcb     $20,4
        fcb     $69,0

start   lds     #code
        ldx     #data
        ldy     #data+2
        ldb     #2
        jsr     addbcd
        swi

; expected result (run with -r option to check)
;! data = #$8904 ;489 in bcd with least significant byte first