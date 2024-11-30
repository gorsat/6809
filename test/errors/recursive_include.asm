; this should fail due to recursive .include 
    lda #1
    .include include/ri2.asm
    swi