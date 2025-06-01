        LIST    p=PIC16C84

ind0    equ     00h			; index register
RTTC    equ     1h
PC      equ     2h			; program counter low byte
STATUS  equ     3h			; status register
FSR     equ     4h			; file select register

Port_A  equ     5h			; port A
Port_B  equ     6h			; port B

INTCON	equ		0bh			; interrupt control register

OPT		equ		01h			; option register

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

NP		equ		.63		; number of patterns
DELAY	equ		.2		; delay between pattern changes 5 * 50 ms
PASSES	equ		.10		; number of times to display pattern

        ORG		10h

m1		RES		1
m2		RES		1
m3		RES		1
idx		RES		1
pass	RES		1

		ORG		0

		clrf	INTCON			; disable interrupts
		bsf		STATUS,rp0		; open page 1
		clrf	OPT				; clear option register
		bsf		OPT,7			; disable pullup
		clrf	Port_B			; Port B all outputs
		bcf		STATUS,rp0		; open page 0
		clrf	Port_B			; Port B all 0
		movlw	PASSES			; get number of times we want to display entire pattern
		movwf	pass			; store it in pass counter
main_1	movlw	NP				; number of patterns
		movwf	idx				; store it in index
main_2	movfw	idx				; get pattern index
		call	g_pat			; get pattern for that index
		movwf	Port_B			; display the pattern
		decfsz	idx,F			; point to next pattern
		goto	main_3			; continue with loop if not done with all patterns
; we come here after one pattern cycle
		decfsz	pass,F			; count one pass, skip if done
		goto	main_4			; otherwise start over the pattern loop
; all cycles done, finish the show
		clrf	Port_B			; Port B all 0
		comf	Port_B
loop	goto	loop			; stay in loop forever

main_4	movlw	NP				; number of patterns
		movwf	idx				; store it in index
main_3	movlw	DELAY			; get delay between pattern changes
		call	wait			; wait about W * 50ms
		goto	main_2			; display next pattern


; this loop waits about W * 50 ms
wait	movwf	m3		; 1							1
		clrf	m2		; 1							1
l2		movlw	63		; 1							1 | 
		movwf	m1		; 1							1 | 
l1		decfsz	m1,F	; 1/2	|					  |
		goto	l1		; 2		| 63*3=189+2=191  191 |
		decfsz	m2,F	; 1/2						1 | 
		goto	l2		; 2							2 | 256 * ( 191 + 5 ) + 2 = 50178
		decfsz	m3,F	; 1/2						1
		goto	l2		; 2							2
		return			; 2							2 | ( 50178 + 3 ) * w + 8



; get pattern pointed to by W, max 256 patterns
g_pat	addwf	PC,F
		retlw	B'00000000'		; this one never gets displayed, index # 0
		retlw	B'11111111'		; patterns are displayed from last to first
		retlw	B'11100111'		; 
		retlw	B'11000011'		; 
		retlw	B'10000001'		; 
		retlw	B'00000000'		; 
		retlw	B'10000000'		; 
		retlw	B'01000000'		; 
		retlw	B'10100000'		; 
		retlw	B'01010000'		; 
		retlw	B'10101000'		; 
		retlw	B'01010100'		; 
		retlw	B'00101010'		; 
		retlw	B'00010101'		; 
		retlw	B'00001010'		; 
		retlw	B'00000101'		; 
		retlw	B'00000011'		; 
		retlw	B'00000101'		; 
		retlw	B'00001010'		; 
		retlw	B'00010101'		; 
		retlw	B'00101010'		; 
		retlw	B'01010100'		; 
		retlw	B'10101000'		; 
		retlw	B'01010000'		; 
		retlw	B'10100000'		; 
		retlw	B'01000000'		; 
		retlw	B'10000000'		; 
		retlw	B'01000000'		; 
		retlw	B'10100000'		; 
		retlw	B'01010000'		; 
		retlw	B'00101000'		; 
		retlw	B'00010100'		; 
		retlw	B'00001010'		; 
		retlw	B'00000101'		; 
		retlw	B'00000010'		; 
		retlw	B'00000001'		; 
		retlw	B'00000010'		; 
		retlw	B'00000101'		; 
		retlw	B'00001010'		;
		retlw	B'00010100'		;
		retlw	B'00101000'		;
		retlw	B'01010000'		;
		retlw	B'10100000'		;
		retlw	B'01000000'		;
		retlw	B'10000000'		;
		retlw	B'01000000'		;
		retlw	B'00100000'		;
		retlw	B'00010000'		;
		retlw	B'00001000'		;
		retlw	B'00000100'		;
		retlw	B'00000010'		;
		retlw	B'00000001'		;
		retlw	B'00000010'		;
		retlw	B'00000100'		;
		retlw	B'00001000'		;
		retlw	B'00010000'		;
		retlw	B'00100000'		;
		retlw	B'01000000'		;
		retlw	B'10000000'		;
		retlw	B'00000000'		;
		retlw	B'10000001'		;
		retlw	B'11000011'		;
		retlw	B'11100111'		;
		retlw	B'11111111'		; my first displayed pattern
		retlw	B'00000000'		; extra

		end