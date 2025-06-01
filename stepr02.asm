        LIST    p=PIC16C84

ind0    equ     00h	; index register
RTTC    equ     1h
PC      equ     2h	; program counter low byte
STATUS  equ     3h	; status register
FSR     equ     4h	; file select register

Port_A  equ     5h	; port A
Port_B  equ     6h	; port B

INTCON	equ	0bh	; interrupt control register

OPT	equ	01h	; option register

C       equ     0h
DC      equ     1h
Z       equ     2h
PD      equ     3h
TO      equ     4h
PA0     equ     5h
PA1     equ     6h
PA2     equ     7h

F       equ     0h
W       equ     1h

LSB     equ     0h
MSB     equ     7h

TRUE    equ     1h
YES     equ     1h
FALSE   equ     0h
NO      equ     0h

rp0     equ     5

DELAY	equ	.5	; delay between pulses 5 ms
NUMP	equ	.250	; number of cycles in one direction

        ORG	10h

m0	RES	1
m1	RES	1
m2	RES	1
m3	RES	1
m4	RES	1
m5	RES	1
cnt	RES	1
idx	RES	1
pass	RES	1

	ORG	0

	clrf	INTCON		; disable interrupts
	bsf	STATUS,rp0	; open page 1
	clrf	OPT		; clear option register
	bsf	OPT,7		; disable pullup
	clrf	Port_A		; Port A all outputs
	clrf	Port_B		; Port B all outputs
	bcf	STATUS,rp0	; open page 0
	clrf	Port_A		; Port A all 0
	clrf	Port_B		; Port B all 0

main	movlw	NUMP
	movwf	pass

big1	movlw	0
	call	step
	movwf	idx

dir1	movf	idx,W
	call	step
	call	waitla
	decfsz	idx,F
	goto	dir1

	decfsz	pass,F
	goto	big1

	movlw	NUMP
	movwf	pass

big2	movlw	0
	call	step
	movwf	cnt
	movlw	1
	movwf	idx

dir2	movf	idx,W
	call	step
	call	waitla
	incf	idx,F
	decfsz	cnt,F
	goto	dir2

	decfsz	pass,F
	goto	big2

	goto	main

step	addwf	PC,F
	retlw	.8	; number of steps
	retlw	0x06
	retlw	0x02
	retlw	0x0A
	retlw	0x08
	retlw	0x09
	retlw	0x01
	retlw	0x05
	retlw	0x04

; this long wait loop waits about W * 1 ms, e.g w=50 - 50ms
waitlb	movwf	Port_B
waitla	movlw	DELAY
waitl	movwf	m3	; 1				1
l4	movlw	.125	; 1				1
	movwf	m2	; 1				1
l3	movlw	.1	; 1				1 | 
	movwf	m1	; 1				1 | 
l1	decfsz	m1,F	; 1/2	|
	goto	l1	; 2	| 10*3=30+4=34 us	  | 91+4=95 us
	nop		; 1	|
;	nop		; 1	|
	decfsz	m2,F	; 1/2		1 | 
	goto	l3	; 2		2 | 100 * ( 97 + 3 ) + 2 = 10 ms
	decfsz	m3,F	; 1/2			1 |
	goto	l4	; 2			2 | ( 10002 + 3 ) * w
	return		; 2				2 | ( 50434 + 3 ) * w + 6

; this short wait loop waits about W * 10 us
waitsb	movwf	Port_B
waitsa	movlw	DELAY
waits	movwf	m2	;			1 |
s2	movlw	2	; 		1 |
	movwf	m1	; 		1 |
s1	decfsz	m1,F	; 1/2	| (m1 - 1) * 3 + 2 = 5
	goto	s1	; 2	| 
	decfsz	m2,F	; 1/2		1 | 
	goto	s2	; 2		2 | w * ( 5 + 3 ) + 3 = 19 us
	return		; 2			2 | 19 + 3

; get pattern pointed to by W, max 256 patterns

; startup pattern
gp_strt	movfw	idx
gp_sta	addwf	PC,F
	retlw	B'00000101'	; first index contains number of steps in pattern
	retlw	B'00000000'	;
	retlw	B'10000001'	;
	retlw	B'11000011'	;
	retlw	B'11100111'	;
	retlw	B'11111111'	; my first displayed pattern

; end pattern
gp_stop	movfw	idx
gp_sto	addwf	PC,F
	retlw	B'00000101'	; first index contains number of steps in pattern
	retlw	B'11111111'	; patterns are displayed from last to first
	retlw	B'11100111'	; 
	retlw	B'11000011'	; 
	retlw	B'10000001'	; 
	retlw	B'00000000'	; 

	retlw	B'00000000'	; extra

	end
