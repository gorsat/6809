* read every byte from 0 to 65535 in memory
* and write it to a given location
    org $fffe
    fdb start
    org 0
    fcb $cc     ;store 0xcc at address 0
                ;we use this as a sanity check below
start:
    ldx #0
loop:
    lda ,-x     ;read byte
    sta data    ;write it
    cmpx #0     ;done?
    bne loop    ;if not then repeat
    swi
data:
    rmb 1
;! x = #0000
;! data = #$cc
