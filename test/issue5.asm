x1  leax    -200,x  ; should be 30 89 FF38
x2  leax    200,x   ; should be 30 89 00C8
x3  ldx     16,s    ; should be AE E8 10
x4  leax    127,x   ; should be 30 88 7F
x5  leax    128,x   ; should be 30 89 0080
x6  leax    -128,x  ; should be 30 89 FF80
x7  leax    -129,x  ; should be 30 89 FF7F
    swi

;! x1 = #$3089
;! x1+2 = #$FF38
;! x2 = #$3089
;! x2+2 = #$00C8
;! x3 = #$AEE8
;! x3+2 = #$10
;! x4 = #$3088
;! x4+2 = #$7F
;! x5 = #$3089
;! x5+2 = #$0080
;! x6 = #$3089
;! x6+2 = #$FF80
;! x7 = #$3089
;! x7+2 = #$FF7F