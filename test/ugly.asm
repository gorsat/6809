label1
label2:  equ     '"
        * this is a comment
start:
        tsta            *this is also a comment
        tstb            even this is a comment
        lbra     my_org

_label3 lda #$40  this is yet another comment
also
also_2  equ     *
my_org  org     $100
        ldy     #*-1
        ldx     #my_org - start
        swi

;! y = #my_org-1
;! x = #$100
