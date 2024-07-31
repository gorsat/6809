; issue #3
; direct page addressing
        org     $fffe
        fdb     start
        org     $1000
start   ldd     #table
        tfr     a,dp
        decb
        stb     table       ; assembler doesn't know DP points at table, so this is implicit extended mode
ba      stb     <table+1    ; explicit direct mode (1 byte operand)
bb      stb     >table+2    ; explicit extended mode addressing
bc      stb     <$03        ; should store b in table+3 (DP still points to table)
        clra
        tfr     a,dp
        stb     <$04        ; should store b in $0004

        swi
        org     $7f00
table   rmb     100 

;! b = #$ff
;! b = table
;! b = table+1 
;! b = table+2
;! b = table+3
;! b = $0004
; direct mode stb should be 2 bytes long
;! ba+2 = bb
; extended mode stb should be 3 bytes long
;! bb+3 = bc