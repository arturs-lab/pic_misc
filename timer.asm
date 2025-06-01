        LIST    p=PIC16C84

; Pin Assignment:
;
; RA0 LSD Select, Key 1 select
; RA1 MSD Select, Key 2 select
; RA2 Tray switch sense
; RA3 Relay Out
; RB0 segment a
; RB1 segment b
; RB2 segment c
; RB3 segment d
; RB4 segment e
; RB5 segment f
; RB6 segment g
; RB7 keyboard input
; RTCC clock frequency

RTTC	equ	1h
PC	equ	2h
STATUS	equ	3h
FSR	equ	4h

OPTREG	equ	1h

Port_A	equ	5h
Port_B	equ	6h

C	equ	0h
DC	equ	1h
Z	equ	2h
PD	equ	3h
TO	equ	4h
PA0	equ	5h
PA1	equ	6h
PA2	equ	7h

F	equ	0h
W	equ	1h

	ORG	0
; START
	clrw		; reset everything
        clrf    0bh
        clrf    Port_A
        clrf    Port_B
        bsf     STATUS,PA0	; Select high memory bank
        movlw   28h		; configure OPTION register
        movwf   OPTREG		; clock from RTCC pin, no prescaller
        movlw   14h		; setup Port A bit 4 & 2 as input, rest output
        movwf   Port_A
        clrf    Port_B		; setup port B as output, bit 7 as input
        bsf     Port_B,07h
        bcf     STATUS,PA0	; select main register file
        bsf     Port_A,0	; select lower display digit
        clrf    14h
        clrf    15h
        clrf    12h
        clrf    13h
        clrf    0ch
        clrf    0dh
        clrf    0fh
        clrf    10h
        clrf    11h
        clrf    RTTC		; reset counter

; MAIN LOOP
l17     movf    RTTC,W		; fetch counter
        xorwf   11h,W		; has it changed? 
        btfsc   STATUS,Z
	goto	l17		; no, go to l17, keep waiting


        movf    RTTC,W		; RTCC changed !
        movwf   11h		; store new value in reg. 11
        xorlw   3bh		; is rtcc=60(dec) ?
        btfss   STATUS,Z	
	goto	l36		; no, just display next digit

; 60 PULSES FROM POWER LINE COUNTED
l20     clrf    RTTC		; reset counter
        clrf    11h		; reset last RTCC memory
        movf    12h,W		; fetch seconds counter
        btfss   STATUS,Z	; is it 0?
	goto	l2b		; no, go to l2b

; RESET SECONDS COUNTER
l25     movlw   3bh		; set 12h to 60 dec.
        movwf   12h
        movf    Port_A,W	; fetch port A
        xorlw   08h		; toggle relay
        movwf   Port_A		; store modified port A
	goto	l2c

; DECREMENT SECONDS COUNTER
l2b     decf    12h,F		; decrement seconds counter

; DISPLAY MINUTES
l2c     movf    14h,W		; fetch minutes
        btfss   STATUS,Z	; see if = 0
	goto	l30		; no, go to l30

; DISPLAY SECONDS
        movf    12h,W		; fetch seconds counter

; GET DISPLAY DATA
l30     movwf   0eh		; load temp. register
	call	l43		; get lower digit code
        movwf   0fh		; store in F
        swapf   0eh,W		; fetch high nybble of time counter
	call	l43		; get upper digit code
        movwf   10h		; store in 10

; DISPLAY DATA
l36     movlw   0ffh		; turn off display
        movwf   Port_B
        movf    Port_A,W	; fetch port A
        xorlw   03h		; invert display select bits
        movwf   Port_A		; store modified value
        btfsc   Port_A,0	; see if lower digit is to be displayed
	goto	l40		; no, display high digit
        movf    0fh,W		; fetch low digit
        movwf   Port_B		; send it to display
	goto	l42		

l40     movf    10h,W		; fetch high digit
        movwf   Port_B		; send it to display
l42	goto	l17

; CONVERT TO 7-SEGMENT CODE
l43     andlw   0fh	; we: W=digit in hex in low nybble
        addwf   PC,F	; wy: W=display code for 7-segment LED display
        retlw   0c0h	; 0
        retlw   0fch	; 1
        retlw   092h	; 2
        retlw   098h	; 3
        retlw   0ach	; 4
        retlw   089h	; 5
        retlw   081h	; 6
        retlw   0dch	; 7
        retlw   080h	; 8
        retlw   088h	; 9
        retlw   084h	; a
        retlw   0a1h	; b
        retlw   0c3h	; c
        retlw   0b0h	; d
        retlw   083h	; e
        retlw   087h	; f

	end

