        LDY      #output
        LDX      #label1
        STX      ,y++
	;! output = #label1
        LDA      label2
        STA      ,y+
	;! output+2 = label2
        LDX      label3
        STX      ,y++
	;! label3 = output+3
        ldx #$fa
        stx $fa
         SWI
data     ORG      $800*2
label1   EQU      1+$45A
label2   FCB      $10*2
label3   FDB      1983+label1,$face,$45,'E
output   RMB      100
foo     org     $f9
;! x = #$fa <-- compare X to the number 00fa
;! x = $fa  <-- compare X to the 16-bits at address 00fa
;! foo+2 = #$fa <-- compare the 8-bits at address foo+2 (00fb) to the value fa
;! foo+1 = #$00fa <-- compare the 16-bits at address foo+1 (00fa) to the value 00fa
;! x = foo+1 <-- compare X to the 16-bits at address foo+1 (00fa)