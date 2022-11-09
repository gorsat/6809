; should result in a bad instruction fault
    org $fffe
    fdb $0000
    org 0
    fdb $11ff ; invalid instruction