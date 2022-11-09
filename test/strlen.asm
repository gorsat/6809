	org	$0000
	lds	#$100
    ldx #string
	jsr strlen
	swi
string
	fcc	"Hello, World!"
	fcb	0

	org	$1000
strlen	
    pshs cc
	lda	#$ff
loop
    inca
	tst	,x+
	bne	loop	
	puls cc,pc

;! a=#13