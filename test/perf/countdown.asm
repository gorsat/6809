* count down from 65535 to 0 and exit
    ldd #$ffff
lsb:   
    tstb        ;if lsb == 0
    beq msb     ;then adjust msb
    decb        ;else lsb -= 1
    bra lsb     ;repeat lsb loop
msb:
    tsta        ;lsb == 0; does msb == 0?
    beq done    ;if so then we're done
    decb        ;lsb = 0xff 
    deca        ;msb -= 1
    bra lsb     ;repeat lsb loop
done:
    swi

;! a = #0
;! b = #0
