x1  leax    -200,x  ; should be 30 89 FF38
x2  leax    200,x   ; should be 30 89 00C8
x3  ldx     16,s    ; should be AE E8 10
x4  leax    127,x
x5  leax    128,x
x6  leax    -128,x
x7  leax    -129,x
    swi

;! x1 = #$3089
;! x1+2 = #$FF38
;! x2 = #$3089
;! x2+2 = #$00C8
;! x3 = #$AEE8
;! x3+2 = #$10