    org     0       ; this is implicit; including for clarity
    ldx     #199
x0  leax    -200,x  ; should be 30 89 FF38
                    ;! x0 = #$3089
                    ;! x0+2 = #$FF38

    stx     tbl     ; should be 9F 40 and tbl should contain $FFFF
                    ;! x0+4 = #$9F40
                    ;! tbl = #$FFFF

x1  leax    200,x   ; should be 30 89 00C8
                    ;! x1 = #$3089
                    ;! x1+2 = #$00C8

    stx     tbl+2   ; should be 9F 42 and tbl+2 should contain $00C7
                    ;! x1+4 = #$9F42
                    ;! tbl+2 = #$00C7

    lds     #tbl
x2  stx     200,s   ; should be AF E9 00C8 and tbl+200 should contain $00C7
                    ;! x2 = #$AFE9
                    ;! x2+2 = #$00C8
                    ;! tbl+200 = #$00C7

x3  leax    127,x   ; should be 30 88 7F
                    ;! x3 = #$3088
                    ;! x3+2 = #$7F

x4  leax    128,x   ; should be 30 89 0080 (+128 doesn't fit in 8 bits)
                    ;! x4 = #$3089
                    ;! x4+2 = #$0080

x5  leax    -128,x  ; should be 30 88 80 (-128 does fit in 8 bits)
                    ;! x5 = #$3080
                    ;! x5+2 = #$80

x6  leax    -129,x  ; should be 30 89 FF7F (-129 doesn't fit in 8 bits)
                    ;! x6 = #$3089
                    ;! x6+2 = #$FF7F

    ldx     #126
x7  leax    -127,x  ; should be 30 88 81 (-127 does fit in 8 bits)
                    ;! x7 = #$3088
                    ;! x7+2 = #$81

    stx     tbl+14  ; should be 9F 4E and tbl+14 should contain $FFFF
                    ;! x7+3 = #$9F4E
                    ;! tbl+14 = #$FFFF

    swi
    org     $40
tbl rmb     32
