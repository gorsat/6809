        org  $0000

        leax message,pc
        stx  xbuf
	    leay message,pcr
        jmp  here
xbuf:   rmb  2

	    org  $ff00
here:   leax message,pcr
        lda  ,x
        swi

message:
        fcc 'Hello, world.'
        fcb $0d
        fcb $0a
        fcb 0

;! y = #message
;! x = #message
;! a = #'H
;! xbuf = #message+4