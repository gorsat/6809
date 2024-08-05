; issue #4
; updated direct page addressing with SETDP
        org     $fffe
        fdb     start
stktop  org     $1000
start   lds     #stktop
        lda     #1
        pshs    a
        ldd     #table
        tfr     a,dp
        decb
a0      stb     table       ; assembler doesn't know DP points at table, so this is implicit extended mode
                            ;! a0 = #$f7
                            ;! a0+1 = #$7f00
                            ;! table = #$ff
        decb
a1      stb     <table+1    ; explicit direct mode (1 byte operand)
                            ;! a1 = #$d701
                            ;! table+1 = #$fe
        decb
a2      stb     >table+2    ; explicit extended mode addressing
                            ;! a2 = #$f7
                            ;! a2+1 = #table+2
                            ;! table+2 = #$fd
        decb
        setdp   table       ; assembler should use table's page as the direct page
a3      stb     table+3     ; implicit direct mode
                            ;! a3 = #$d703
                            ;! table+3 = #$fc
        decb
        setdp               ; turn off auto direct mode
        stb     <$04        ; should store b in table+4 (DP still points to table)
                            ;! table+4 = #$fb
        decb
a4      stb     table+5     ; extended (auto direct mode is off)
                            ;! a4 = #$f7
                            ;! a4+1 = #table+5
                            ;! table+5 = #$fa
        decb
        puls    dp          ; restore dp from stack
        stb     <0          ; should store b in $100
                            ;! $100 = #$f9
        decb
        setdp   $100        ; let the assembler know we're using $100 as the direct page
a5      stb     $101        ; should be direct mode again
                            ;! a5 = #$d701
                            ;! $101 = #$f8
        swi
table   org     $7f00
