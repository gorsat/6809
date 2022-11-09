; should result in a stack overflow
    org $fffe
    fdb $1000
    org $1000
    lds #0
    pshs a
    swi ; should not get here