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
        decb
        lda     #2
        tfr     a,dp
        setdp   2           ; now we're using $200 as the direct page
a6      stb     $200        ; should be direct
                            ;! a6 = #$d700
                            ;! $200 = #$f7
        decb
        setdp   $200        ; still using $200 as the direct page
a7      stb     $201        ; direct again
                            ;! a7 = #$d701
                            ;! $201 = #$f6
        decb
        lda     #$7f
        tfr     a,dp        
        setdp   $7f00       ; tell the assembler that DP is now $7F
a8      stb     $7ffe       ; direct
                            ;! a8 = #$d7fe
                            ;! $7ffe = #$f5
        decb
        deca
        tfr     a,dp
        setdp   $7e         ; DP changed to $7E
a9      stb     $7eee       ; direct
                            ;! a9 = #$d7ee
                            ;! $7eee = #$f4
        decb
aa      stb     $7fff       ; extended
                            ;! aa = #$f7
                            ;! aa+1 = #$7fff
                            ;! $7fff = #$f3

        swi
table   org     $7f00
