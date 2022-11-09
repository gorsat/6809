; convert hex nibble at $40 to ascii  
; and store it at $41
       org    $0
       lds    #$100
       lda    <$40  ; force direct addressing using '<'
       jsr    asdec
       sta    <$41
       swi
       org    $20
asdec  cmpa   #9
       bls    ascz
       adda   #'A-'9-1
ascz   adda   #'0
       rts
       org    $40
       fcb    $0c

;! a = #'C
;! $41 = #$43

