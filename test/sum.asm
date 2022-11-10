; 8-bit, unsigned sum of a series of bytes
        lds     #data ; top of stack, i.e., #data-1 is the first byte of stack that will be used
        ldx     #data ; so we can store stuff starting at #data
        ldb     #3
        jsr     sum
        swi
        org *+32
sum     clra
sum001  adda    ,x+
        decb
        bne     sum001
        rts

data    org     $1000
        fcb     13,22,34

; expected result (run with -r option to check)
;! a = #$45