label1 fdb fn1,fn2,fn3,fn4  comment
label2 fdb fn1,fn2,fn3,fn4 ; comment, 
label3 fcb 1,2,3,4 ;comment, and comment
label4 fcb 1,2,3,4 comment, and comment
label5 fcb 1,2,3,4 * comment, and comment
label6 fdb fn1, fn2, fn3, fn4 I'm a comment, but I have a comma
label7 fdb fn1, fn2, 
label8 fcb 0, 
label9 fdb fn1 comment, comment
    org  $0000
    lds  #$1000

    lda  #$99
    ldb  #$88
    std  $100
    pshs d
    clra
    clrb
    puls d
    swi
fn1: nop
fn2: nop
fn3: nop
fn4: nop

;! a = #$99
;! b = #$88
;! $100 = #$9988