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
NUMP	equ	.255	; number of cycles in one direction
plslen	equ	.50	; length of pulse 50us

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

strtp	clrw			; pattern #0 contains number of patterns
	call	gp_sta
	movwf	idx		; store it in index
strtl	call	gp_strt	; get pattern
	call	waitlb	; display the pattern and wait delay time
	decfsz	idx,F
	goto	strtl

d1	movlw	NUMP
	movwf	pass
	clrw
	call	gp_p1a
	movwf	idx		; store it in index

	bcf	Port_A,3
d1a	movf	pass,W
	movwf	m1
	rrf	m1,F
	rrf	m1,F
	rrf	m1,F
	rrf	m1,F
	rrf	m1,F
	movlw	0x07
	andwf	m1,W
	call	gp_p1a
	movwf	Port_B
d1b	call	pulse
	decfsz	pass,F
	goto	d1a

d2	movlw	NUMP
	movwf	pass
	clrw
	call	gp_p2a
	movwf	idx		; store it in index

	bsf	Port_A,3
d2a	movf	pass,W
	movwf	m1
	rrf	m1,F
	rrf	m1,F
	rrf	m1,F
	rrf	m1,F
	rrf	m1,F
	movlw	0x07
	andwf	m1,W
	call	gp_p3a
	movwf	Port_B
d2b	call	pulse
	decfsz	pass,F
	goto	d2a

	goto	d1

pulse	bsf	Port_A,2	; send pulse of width= w*10 us
	movlw	plslen
	call	waits
	bcf	Port_A,2
	goto	waitla
;	call	waitl
;	return

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

;pattern1
gp_p1	movfw	idx		; get pattern index
gp_p1a	addwf	PC,F
;	retlw	B'00001110'	; first index contains number of steps in pattern
	retlw	B'10000000'	;
	retlw	B'01000000'	;
	retlw	B'00100000'	;
	retlw	B'00010000'	;
	retlw	B'00001000'	;
	retlw	B'00000100'	;
	retlw	B'00000010'	;
	retlw	B'00000001'	;

;pattern3
gp_p3	movfw	idx		; get pattern index
gp_p3a	addwf	PC,F
	retlw	B'00000001'	;
	retlw	B'00000010'	;
	retlw	B'00000100'	;
	retlw	B'00001000'	;
	retlw	B'00010000'	;
	retlw	B'00100000'	;
	retlw	B'01000000'	;
	retlw	B'10000000'	;

;pattern2
gp_p2	movfw	idx		; get pattern index
gp_p2a	addwf	PC,F
	retlw	B'00001110'	; first index contains number of steps in pattern
	retlw	B'10111111'	;
	retlw	B'11011111'	;
	retlw	B'11101111'	;
	retlw	B'11110111'	;
	retlw	B'11111011'	;
	retlw	B'11111101'	;
	retlw	B'11111110'	;
	retlw	B'11111101'	;
	retlw	B'11111011'	;
	retlw	B'11110111'	;
	retlw	B'11101111'	;
	retlw	B'11011111'	;
	retlw	B'10111111'	;
	retlw	B'01111111'	;

	retlw	B'00000000'	; extra

	end
