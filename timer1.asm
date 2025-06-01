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

IND0    equ     0h
RTTC	equ	1h
PC	equ	2h
STATUS	equ	3h
FSR	equ	4h
INTCON	equ	0bh

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

; PROGRAM EQUS

RTCCS	equ	0ch	; RTCC shadow
FLAGS	equ	0dh	; status of the switches
TRAY	equ	0eh	; tray debouncer timeout
SW1	equ	0fh	; switch 1 debouncer, minutes
SW2	equ	10h	; switch 2 debouncer, tens of minutes
SEC	equ	11h	; seconds counter
TSEC	equ	12h	; tens of seconds
MIN	equ	13h	; minutes counter
TMIN	equ	14h	; tens of minutes

	ORG	0
start
        clrf    INTCON	; disable interrupts
        clrf    Port_A
        clrf    Port_B

;        bsf     STATUS,PA0	; Select high memory bank
;        movlw   14h		; setup Port A bit 4 & 2 as input, rest output
;        movwf   Port_A
;        movlw   80h		; setup port B as output, bit 7 as input
;        movwf   Port_B
;        movlw   28h		; configure OPTION register
;        movwf   OPTREG		; clock from RTCC pin, no prescaller
;        bcf     STATUS,PA0	; select main register file
        movlw   14h		; setup Port A bit 4 & 2 as input, rest output
	tris	Port_A
        movlw   80h		; setup port B as output, bit 7 as input
	tris	Port_B
        movlw   28h		; configure OPTION register
	option

        bsf     Port_A,0	; select lower display digit
	clrf	FLAGS		; clear switch flags
	clrf	TRAY		; clear tray debounce counter
	clrf	SW1
	clrf	SW2
	clrf	SEC
	clrf	TSEC
	clrf	MIN
	clrf	TMIN
	clrf	RTCCS		; reset RTCC shadow register
        clrf    RTTC		; reset RTCC

; MAIN LOOP
main    
tray1	btfsc	Port_A,2	; is tray pushed in?
	goto	tray3		; no, make sure that the lamp is off
	btfsc	Port_A,3	; is the lamp already on?
	goto	tray2		; yes
	incf	TRAY,F		; increment debounce counter
	btfss	TRAY,4		; see if > 16 cycles
	goto	trayx		; you're done
        movfw   SEC             ; see if time is non-zero
	iorwf	TSEC,W
	iorwf	MIN,W
	iorwf	TMIN,W
	btfss	STATUS,Z
	bsf	Port_A,3	; turn on the lamp
	goto	trayx		; you're done
tray2	clrf	TRAY		; lamp already on, make sure that bounce counter is 0
	goto	trayx		; done
tray3	btfss	Port_A,3	; is lamp on?
	goto	tray4		; no
	incf	TRAY,F		; increment debounce counter
	btfsc	TRAY,4		; see if > 16 cycles
	bcf	Port_A,3	; turn the lamp off
	goto	trayx
tray4	clrf	TRAY
trayx

; PROCESS SWITCHES
switch	btfsc	Port_A,0	; which switch is it?
	goto	swi2		; tens of minutes
	btfsc	Port_B,7	; is a switch down?
	goto	swi1		; no, just do some house cleaning
	btfsc	SW1,4		; was it already processed?
	goto	swout		; yes, disregard it
        incf    SW1,F           ; increment debounce counter
	btfss	SW1,4		; is this the 16th cycle
	goto	swout
	movf	SW2,W		; fetch other counter
	btfsc	STATUS,Z	; is it zero?
	goto	start		; both switches down, reset system
        incf    MIN,F           ; increment minutes counter
	movlw	0ah		; did it go over 9?
	xorwf	MIN,W
	btfss	STATUS,Z
	goto	swout		; no, done processing kbd.
	clrf	MIN		; yes, set minutes to 0 and increment tens of minutes
	goto	swi4
swi1	clrf	SW1		; reset debounce counter
	goto	swout

swi2	btfsc	Port_B,7	; is a switch down?
	goto	swi3		; no, just do some house cleaning
	btfsc	SW2,4		; was it already processed?
	goto	swout		; yes, disregard it
	incf	SW2,F		; increment debounce counter
	btfss	SW2,4		; is this the 16th cycle
	goto	swout
	movf	SW1,W		; fetch other counter
	btfsc	STATUS,Z	; is it zero?
	goto	start		; both switches down, reset system
swi4	incf	TMIN,F		; increment tens of minutes counter
	movlw	10h		; did it go over 0x0f?
	xorwf	TMIN,W
	btfss	STATUS,Z
	goto	swout		; no, done processing kbd.
	clrf	TMIN		; yes, set tens of minutes to 0
	goto	swout

swi3	clrf	SW2		; reset debounce counter
	goto	swout
swout	

	movf    RTTC,W		; fetch counter
        xorwf   RTCCS,W		; has it changed? 
        btfsc   STATUS,Z
	goto	main		; no, close loop, keep waiting

        movf    RTTC,W		; RTCC changed !
        movwf   RTCCS		; store new value in shadow register
        xorlw   3bh		; is rtcc=60(dec) ?
        btfss   STATUS,Z	
        goto    dtime           ; no, just display next digit

; 60 PULSES FROM POWER LINE COUNTED
l20     clrf    RTTC		; reset counter
        clrf    RTCCS		; reset last RTCC memory

        movwf   SEC             ; see if time is non-zero
	iorwf	TSEC,W
	iorwf	MIN,W
	iorwf	TMIN,W
	btfsc	STATUS,Z
	goto	decsec		; time is non-zero
	bcf	Port_A,3	; time is zero, turn off the lamp
	goto	dtime		; you're done

; DECREMENT SECONDS COUNTER
decsec  decf    SEC,F           ; decrement seconds counter
        movlw   0ffh		; see if seconds counter went past 0
	xorwf	SEC,W
        btfsc   STATUS,Z
	goto	dtime		; not yet
	movlw	9		; set seconds to 9
	movwf	SEC
; DECREMENT TENS OF SECONDS
dects   decf    TSEC,F          ; decrement tens of seconds counter
        movlw   0ffh		; see if tens of seconds counter went past 0
	xorwf	TSEC,W
        btfsc   STATUS,Z
	goto	dtime		; not yet
	movlw	5		; set  tens of seconds to 5
        movwf   TSEC  
; DECREMENT MINUTES COUNTER
decm    decf    MIN,F           ; decrement minutes counter
        movlw   0ffh		; see if minutes counter went past 0
	xorwf	MIN,W
        btfsc   STATUS,Z
	goto	dtime		; not yet
	movlw	9		; set minutes to 9
	movwf	MIN
; DECREMENT TENS OF MINUTES
dectm   decf    TMIN,F          ; decrement tens of minutes counter
;	movlw   0ffh		; see if tens of tens of minutes counter went past 0
;	xorwf	TMIN,W
;	btfsc   STATUS,Z
;	goto	dtime		; not yet
;	movlw	5		; set tens of minutes to 5
;	movwf	TMIN
	
; DISPLAY TIME
dtime   movf    TMIN,W		; fetch tens of minutes
        iorwf   MIN,W           ; and minutes
        btfss   STATUS,Z	; see if both = 0
	goto	dispsec		; no, display minutes
	movlw	MIN		; set pointer to minutes
	goto	display
dispsec	movlw	SEC		; minutes=0, display seconds

; DISPLAY
display
	movwf	FSR		; store pointer in FSR
	movlw   0ffh		; turn off display
        movwf   Port_B
        movf    Port_A,W	; fetch port A
        xorlw   03h		; invert display select bits
        movwf   Port_A		; store modified value
	btfsc	Port_A,0	; see which digit to display
	incf	FSR,F		; display tens
	movf	IND0,W		; fetch data to be displayed
	call	l43		; convert to 7-segment code
        movwf   Port_B		; send it to display
	goto	main

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

