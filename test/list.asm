* point the reset vector at the start of our program
    org $fffe
    fdb start

* code segment starts at $1000
code org $1000

*
* list elements must have <next> pointer as first two bytes
*
* insert an element into a singly linked list
* the new element becomes the head of the list
* x - **head_of_list
* y - *new_element 
* no return value
push:
	ldd	,x  ;d = prev_head
	std	,y  ;new_element->next = prev_head
	sty	,x	;head = new_element
    rts

* remove an element from the head of a singly linked list
* x - **head_of_list
* return:
* x - *removed_element (or zero if none)
pop:
	ldy	,x	    ;y = head
	beq	pop01   ;if the list is empty we're done
	ldd	,y	    ;d = head->next	
	std	,x		;head = head->next 
pop01:
	tfr	y,x		;x = removed element
    rts

* beginning of program
start:
    lds #code   ;stacks reside below our code
    ldu #code/2
test:
    ldd #0
    std head
	ldy	#elt1   
	ldx	#head		
	jsr	push	;push elt1 onto the list		
	ldy	#elt2	
	ldx	#head	
	jsr	push	;push elt2 onto the list		
	ldx	#head	
	jsr	pop     ;pop x from the list (x should equal elt2 now)		
    swi

elt1 rmb	2		
elt2 rmb	2		
head rmb	2		

; expected results (run with -d flag to check)
;! head = #elt1
;! elt1 = 0
;! x = #elt2