xa      LEAX     $-10,x    ; should be 30 10
xb      LEAX     -16,x
;! xa = xb
xc      LEAX     $-F,x     ; should be 30 11
xd      LEAX     -15,x
;! xc = xd
xe      LEAX     $-1,x     ; should be 30 1F
xf      LEAX     -1,x
;! xe = xf
xg      LEAX     $1,x
xh      LEAX     1,x
;! xg = xh
xi      LEAX     $F,x
xj      LEAX     15,x
;! xi = xj
xk      LDA     #$-10
xl      LDA     #-16
;! xk = xl
xm      LDA     #-$10
xn      LDA     #240
;! xm = xn
;! xk = xm

        SWI