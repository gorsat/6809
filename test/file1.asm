; this is file1
    lda #1
    .include include/file2.asm
    swi
; this is the end of file1
;! a=#0
;! b=#3