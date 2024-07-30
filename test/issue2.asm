; issue #2
; added LSL as an alias for ASL
    lds     #$100
    lda     #%01010101
    pshs    a
    lsra
    rola
    ldb     #2
    mul
    stb     bb
    puls    a
    lsla
    swi
bb  fcb     0

;! a = #%10101010
;! a = bb