;; 
;;    $Id: ds1820.asm,v 1.1 2001/12/27 22:46:51 james Exp james $
;; 
;;    ds1820.asm, a PIC 12C509 or 16F84 based DS1820 translator
;;    Copyright (C) 2002  James Cameron (quozl@us.netrek.org)
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;

;;
;;    Refer to http://quozl.netrek.org/ts/ or http://quozl.linux.org.au/ts/
;;    for further documentation.
;; 
;;    Variable prefix conventions
;;       b_	bit number
;;       m_	bit mask
;;       r_	static file register
;; 
;;    Function prefix conventions
;;       t_	entry point for a lookup table
;;       ow_	one wire bus communication functions
;;       tx_	serial data transmission functions
;;       sb_	sixteen bit math functions
;;
;;    Source code reading hints
;;       - INDF is a data stack pointer, FSR is top of stack,
;;       - stack macros are used, check their definitions carefully,
;;       - explicit bank switching is done in the code, using macros.
;; 

	ifdef		__16f84
	processor	16f84
	list		f=inhx8m
	include		"p16f84.inc"
	__config	_cp_off & _wdt_on & _hs_osc
	endif

	ifdef		__12c509
	processor	12c509
	list		f=inhx8m
	include		"p12c509a.inc"
	ifdef		jw
	__config	_mclre_off & _cp_off & _wdt_on & _iNTrc_osc
	endif
	ifdef		cp
	__config	_mclre_off & _cp_on & _wdt_on & _iNTrc_osc
	endif
        endif
	
	ifdef	__16f84
base	equ	0x0c		; first free file register address
stack	equ	0x4f+1		; end address of stack
port	equ     portb		; port on which bits are used
movwt	macro			; move w to tris
        tris	trisb
	endm
bank0	macro
	endm
bank1	macro
	endm
	endif
	ifdef	__16f84	
	endif

	ifdef	__12c509
base	equ	0x07		; first free file register address
stack	equ	0x1f+1		; end address of stack
port	equ	gpio		; port on which bits are used
movwt	macro			; move w to tris
	tris	gpio
	endm
bank0	macro			; select low bank for goto/call
	bcf	status,pa0
	endm
bank1	macro			; select high bank for goto/call
	bsf	status,pa0
	endm
	endif

debug	equ	0		; set to one to enable calculation stack trace
	
	;; port bit allocations

b_ow0	equ	0		; one wire bus to first DS1820
b_ow1	equ	1		; one wire bus to second DS1820
b_tx	equ	2		; serial output 2400 baud to host
b_in	equ	3		; mode flag input
b_ow2	equ	4		; one wire bus to third DS1820
b_ow3	equ	5		; one wire bus to fourth DS1820

	;; r_trism extra allocations

b_noisy	equ	6		; display more data
b_fahre	equ	7		; display in fahrenheit

	;; mask of one wire bus valid bits
	
m_ow	equ	1<<b_ow0|1<<b_ow1|1<<b_ow2|1<<b_ow3
	
	;; data stack macros, to reduce memory use
	;; fsr points to top element of stack
	;; stack grows downward in memory addresses (upward on paper)
	;; initial fsr high in register space
	
popw	macro				; pop to w from stack
	movf	indf,w	
	incf	fsr,f	
	endm

pushw	macro				; push from w to stack
	decf	fsr,f	
	movwf	indf
	endm

popl	macro				; pop literal (aka drop) from stack
	incf	fsr,f	
	endm

pushl	macro	m_literal		; push literal to stack
	movlw	m_literal
	pushw
	endm

popf	macro	m_to			; pop to file addresss (uses w)
	popw
	movwf	m_to
	endm

pushf	macro	m_from			; push from file address (uses w)
	movf	m_from,w	
	pushw
	endm

popf16	macro	m_to			; pop 16-bit value
	popf	m_to+1
	popf	m_to
	endm

pushf16	macro	m_from			; push 16-bit value
	pushf	m_from
	pushf	m_from+1
	endm

pushl16	macro	m_literal		; push literal to stack
	pushl	m_literal
	pushl	0
	endm

popl16	macro				; pop literal (aka 2drop)
	popl
	popl
	endm

mov16	macro	m_from,m_to		; move 16 bit value
	movf	m_from+0,w	
	movwf	m_to+0
	movf	m_from+1,w	
	movwf	m_to+1
	endm
	
clr16	macro	m_from
	clrf	m_from+0
	clrf	m_from+1
	endm

	;; static register allocation
	;; (12c509 has 9 registers addressible regardless of bank select)
		
	cblock	base
		r_ephemeral	; lowest stack level temporary storage

		r_temporary	; middle stack level temporary storage
	
		r_trism		; mirror of tristate output latch
				; used by ow_high, ow_low, main
	
		r_which		; bit mask of which DS1820 to talk to
	
				; data read from device scratchpad
		r_ds_lsb	; temperature lsb
		r_ds_msb	; temperature msb
		r_ds_rem	; remaining count after conversion
		r_ds_count	; counts per degrees c
		r_ds_crc	; cyclic redundancy check byte
	endc

	;; FSR based data stack is from 0x1f to 0x10, 16 bytes.
	
	;; register addresses 0x30 to 0x3f are unallocated due to the
	;; requirement to set bit five of FSR prior to access.

	;; start of executable code
	
	org	0x0
	ifdef	__12c509
	movwf	OSCCAL		; calibrate internal oscillator
	endif
	bank1
	goto	main		; go to main program



;;;
;;; t_gday, table lookup of serial number
;;;
;;; input:	w contains offset in string
;;; output:	w contains table value
;;; 
t_gday
	addwf	pcl,f			; vector to byte in serial #
	;; include "version.inc"	; fetch version from file
	dt	"V1.0"                  ; or use this line
	dt	" "
	;; include "serial.inc"		; fetch serial number from file
	dt	"quozl.netrek.org/ts"   ; or use this line
	retlw	0			; terminating zero for caller


	
;;;
;;; t_which, table lookup of sensor number
;;;
;;; input:	w contains port bit number
;;; output:	w contains ASCII sensor number
;;; 
t_which
	addwf	pcl,f	
	dt	"12  34"
	


;;;
;;; us, delay a number of 10 microsecond cycles
;;;
;;; call stack:	none
;;; data stack: ( cycles -- ) [total 1]
;;; 
us10
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	decfsz	indf,f			; loop until done
	goto	us10
	popl				; drop argument
	retlw	0



;;;
;;; ow_, routines for one wire bus communication
;;;



;;;
;;; ow_command_*, one wire bus command bytes, from ds-1820 data sheet
;;; 
ow_command_skip		equ	0xcc	; skip rom match sequence
ow_command_convert	equ	0x44	; commence temperature conversion
ow_command_read		equ	0xbe	; read the data scratchpad

;;;
;;; ow_high, one wire high, allow output line to float to pull-up
;;;
;;; call stack:	none
;;; data stack:	none
;;; 
ow_high
	movf	r_which,w	; fetch the selection mask
	iorwf	port,f		; set that bit in port latch
				; (a momentary 2uS fast pull-up)
	iorwf	r_trism,w	; set that bit in tris mask
	movwf	r_trism		; save tris mask
	movwt			; set tris
        retlw	0

;;;
;;; ow_low, one wire low, drive output line to ground
;;;
;;; call stack:	none
;;; data stack:	none
;;; 
ow_low
	comf	r_which,w	; fetch inverted selection bits
	andwf	port,f		; reset selected bits of port
	andwf	r_trism,f	; reset selected bits of tris mirror
	movf	r_trism,w	; fetch tris mirror
	movwt			; set tris
        retlw	0

;;;
;;; ow_reset, one wire reset, signal the slave that we want a bus reset
;;;
;;; call stack:	calls others that do not call
;;; data stack:	( -- slave-present-flag ) [total 1]
;;; 
ow_reset
	;; allow the bus to go high
	call	ow_high
	pushl	d'9'		; 60us delay
	call	us10
	
	;; now check to see if the bus is sick (no pull-up)
	movf	port,w		; read port bits once only
	andwf	r_which,w	; keep bits we want
	bz	ow_reset_fail	; bus is being held down, fail

	;; bus is healthy, now issue a reset
	call	ow_low
	pushl	d'50'		; 500us delay (480 to 960us)
	call	us10
	call	ow_high
	pushl	d'9'		; 60us delay
	call	us10
	movf	port,w		; read port bits once only
	andwf	r_which,w	; keep bits we want
	bz	ow_reset_ok
	pushl	d'39'		; total 480us delay
	call	us10
	
ow_reset_fail
	pushl	0		; return failure status
	retlw	0

ow_reset_ok
	pushl	d'39'		; total 480us delay
	call	us10
	pushl	1		; return success status
	retlw	0

;;;
;;; ow_rx, one wire receive, receive a byte from the bus
;;;
;;; call stack:	calls others that do not call
;;; data stack:	( -- received-byte ) [total 1]
;;; 
ow_rx
	pushl	d'0'		; received data
	pushl	d'8'		; count of bits to read in

ow_rx_loop
	call	ow_low		; generate short low for slave
	nop
	call	ow_high
	nop			; delay for 6 microseconds
	nop
	nop
	nop
	nop
        nop
	movf	port,w		; read port bits once only
	andwf	r_which,w	; keep bits we want
	btfsc	status,z	; test if it is zero
	bcf	status,c	; it is, so clear carry
	btfss	status,z	; test if it is one
	bsf	status,c	; it is, so set carry

	incf	fsr,f		; rotate the bit into our copy
	rrf	indf,f	
	decf	fsr,f	
	
	pushl	d'6'		; and wait for sixty microseconds
	call	us10
	decfsz	indf,f		; decrement count of bits to read
	goto	ow_rx_loop	; loop until end of byte
	incf	fsr,f		; drop count of bits
	retlw	0
	
;;;
;;; ow_tx, one wire transmit, output a byte on the bus
;;;
;;; call stack:	calls others that do not call
;;; data stack:	( transmit-byte -- ) [total 3]
;;; 
ow_tx
	pushl	d'8'		; initialise bits in byte counter

ow_tx_loop
	incf	fsr,f		; rotate next bit to carry flag
	rrf	indf,f	
	decf	fsr,f	
        btfsc	status,c	; test the bit
	goto	ow_tx_one
	
ow_tx_zero			; transmit a zero
	call	ow_low		; by generating a long low
	pushl	d'6'		; for sixty microseconds
	call	us10
	call	ow_high		; then return to high
	goto	ow_tx_skip	; and continue to next bit
	
ow_tx_one			; transmit a one
	call	ow_low		; by generating a short low
	call	ow_high		; followed by a long high
	pushl	d'6'		; for sixty microseconds
	call	us10

ow_tx_skip
	decfsz	indf,f		; decrement bits in byte counter
        goto	ow_tx_loop	; loop until counter zero
	incf	fsr,f		; drop bits in byte counter
	incf	fsr,f		; drop transmit byte
	retlw	0		; return to caller



;;;
;;; tx_, routines for serial transmission, asynchronous
;;; 



;;;
;;; tx_byte_hex, transmit a byte in hexadecimal
;;;
;;; call stack:	calls others that do not call
;;; data stack:	( byte-to-send -- ) [total 3]
;;; 
tx_byte_hex
	swapf	indf,w			; send high nibble first
	call	tx_byte_hex_digit
	popw
	
tx_byte_hex_digit
	andlw	0x0f			; keep low nibble only
	pushw				; save to data stack
	movlw	0x0a			; test for greater than nine
	subwf	indf,w			; by subtraction
	movlw	0x30			; assume numeric
	btfsc	status,c		; test result of subtraction
	movlw	0x37			; alphabetic
	addwf	indf,f			; offset to ASCII character
;	goto	tx_byte			; fall-through optimisation

;;;
;;; tx_byte, transmit byte, serially send at 2400 
;;;
;;; call stack:	none
;;; data stack:	( transmit-byte -- ) [total 2]
;;; 
tx_byte
	pushl	9			; bit count
	bcf	status,c		; start bit
	
tx_byte_loop_1
	btfsc	status,c		; echo bit to output
        bcf     port,b_tx
	btfss	status,c
        bsf     port,b_tx

        pushl   d'134'			; set timer for 416us, 2400 baud
	;; [cycles=7]
tx_byte_loop_2
	decfsz	indf,f	
	goto	tx_byte_loop_2
	popl				; drop timer
	
        incf	fsr,f			; rotate byte to send
        rrf     indf,f	
	decf	fsr,f	
	;; [cycles=11+134*3]
	
	decfsz	indf,f			; decrement bit count and loop
	goto	tx_byte_loop_1
	;; [cycles=14+134*3], therefore 2403.8 baud

        bcf     port,b_tx		; send stop bit
	
        movlw   d'134'			; set timer
	pushw
tx_byte_loop_3
	decfsz	indf,f	
	goto	tx_byte_loop_3

	movlw	d'3'			; drop stack (timer, bit count, byte)
	addwf	fsr,f	

	retlw	0
	
;;;
;;; tx_header, transmit packet header, single byte followed by space
;;; 
;;; input:	none
;;; output:	none
;;; stack:	calls others that do not call
;;; data stack:	( byte -- ) [total 1]
;;; 
tx_header
	call	tx_byte

;;;
;;; tx_space, send a single space
;;; 
;;; input:	none
;;; output:	none
;;; stack:	calls others that do not call
;;; data stack:	( -- ) [total 1]
;;; 
tx_space
        pushl	0x20
	goto	tx_byte

;;;
;;; tx_crlf, transmit carriage return line feed
;;; 
;;; input:	none
;;; output:	none
;;; stack:	calls others that do not call
;;; data stack:	( -- ) [total 1]
;;; 
tx_crlf
        pushl	0x0a
	call	tx_byte
        pushl	0x0d
	goto	tx_byte

;;;
;;; tx_put, macro for tx_byte_hex
;;;
;;; input:	address of byte to send
;;; output:	none
;;; stack:	mainline macro
;;; 
tx_put	macro	m_byte
	pushf	m_byte
	bank0
	call	tx_byte_hex
	endm

;;;
;;; arithmetic vector table
;;; (12c509 can't call into upper page of bank 0, 0x100 to 0x1ff)
;;;
sb_bcd		goto	sb_bcd_
sb_bcd_fix	goto	sb_bcd_fix_
sb_add		goto	sb_add_
sb_subtract	goto	sb_subtract_
sb_abs		goto	sb_abs_
sb_negate	goto	sb_negate_
sb_nip		goto	sb_nip_
sb_over		goto	sb_over_
sb_multiply	goto	sb_multiply_
sb_divide	goto	sb_divide_

last0		equ	sb_divide	; define the last call entry in page 0
	
;;;
;;; sb_bcd, convert accumulator to binary coded decimal for output
;;;
;;; data stack:	( lsb msb -- d0 d1 d2 ) [total 5]
;;; stack:	calls others that do not call
;;;
sb_bcd_
	;; ( lsb msb )
	popf	r_ephemeral		; msb
	popf	r_temporary		; lsb
	movlw	0
	pushw				; d0 (lsd)
	pushw				; d1
	pushw				; d2 (msd)
	pushf	r_temporary		; lsb
	pushf	r_ephemeral		; msb
        pushl	d'16'			; count
	
	;; ( d0 d1 d2 msb lsb count )
        bcf     status,c

sb_bcd_loop
	incf	fsr,f			; ( d0 d1 d2 lsb msb  ^  count )
	incf	fsr,f			; ( d0 d1 d2 lsb  ^  msb count )
	rlf	indf,f			; lsb
	decf	fsr,f			; ( d0 d1 d2 lsb msb  ^  count )
	rlf	indf,f			; msb
	incf	fsr,f	
	incf	fsr,f	
	incf	fsr,f	
	incf	fsr,f			; ( d0  ^  d1 d2 lsb msb count )
	rlf	indf,f			; d0 (lsd)
	decf	fsr,f			; ( d0 d1  ^  d2 lsb msb count )
	rlf	indf,f			; d1
	decf	fsr,f			; ( d0 d1 d2  ^  lsb msb count )
	rlf	indf,f			; d2 (msd)

	movlw	d'3'
	subwf	fsr,f			; ( d0 d1 d2 lsb msb count )

	decfsz	indf,f			; count
	goto	sb_bcd_adjust

	movlw	d'3'
	addwf	fsr,f			; drop drop drop
	
	;; ( d0 d1 d2 )
	retlw	0

sb_bcd_adjust
	;; ( d0 d1 d2 lsb msb count )

	movlw	d'3'
	addwf	fsr,f			; ( d0 d1 d2  ^  lsb msb count )
	
	call	sb_bcd_fix		; d2
	call	sb_bcd_fix		; d1
	call	sb_bcd_fix		; d0
					; (  ^  d0 d1 d2 lsb msb count )
	movlw	d'6'
	subwf	fsr,f			; ( d0 d1 d2 lsb msb count )
	
	goto	sb_bcd_loop
	
sb_bcd_fix_
        movlw   3
        addwf   indf,w			; add 3 to low nibble
        movwf   r_ephemeral		; test bits
	btfsc	r_ephemeral,3		; if bit three is set
        movwf   indf			; store the adjusted value
        movlw   0x30
        addwf   indf,w			; add 3 to high nibble
        movwf   r_ephemeral		; test bits
	btfsc	r_ephemeral,7		; if bit seven is set
        movwf   indf			; store the adjusted value
        incf    fsr,f			; move to next digit pair
	retlw	0



;;;
;;; sb_*, sixteen bit math functions
;;;



;;;
;;; sb_add, sixteen bit addition
;;;
;;; data stack:	( a b -- a+b ) [total 4]
;;; stack:	none
;;;
sb_add_					; ( lsb1 msb1 lsb2 msb2 -- lsb3 msb3 )
	movf	indf,w			; msb2
	incf	fsr,f			; ( lsb1 msb1 lsb2 ^ msb2 )
	incf	fsr,f			; ( lsb1 msb1 ^ lsb2 msb2 )
	addwf	indf,f			; msb3 = msb1 + msb2
	decf	fsr,f			; ( lsb1 msb3 lsb2 ^ msb2 )
	movf	indf,w			; lsb2
	incf	fsr,f			; ( lsb1 msb3 ^ lsb2 msb2 )
	incf	fsr,f			; ( lsb1 ^ msb3 lsb2 msb2 )
	addwf	indf,f			; lsb3 = lsb1 + lsb2
	decf	fsr,f			; ( lsb3 msb3 ^ lsb2 msb2 )
	btfsc	status,c		; carry?
	incf	indf,f			; msb3
	retlw	0			; ( lsb3 msb3 )


;;;
;;; sb_subtract, sixteen bit subtraction
;;;
;;; data stack:	( a b -- a-b ) [total 4]
;;; stack:	none
;;;
sb_subtract_				; ( lsb1 msb1 lsb2 msb2 -- lsb3 msb3 )
	movf	indf,w			; msb2
	incf	fsr,f			; ( lsb1 msb1 lsb2 ^ msb2 )
	incf	fsr,f			; ( lsb1 msb1 ^ lsb2 msb2 )
	subwf	indf,f			; msb3 = msb1 - msb2
	decf	fsr,f			; ( lsb1 msb3 lsb2 ^ msb2 )
	movf	indf,w			; lsb2
	incf	fsr,f			; ( lsb1 msb3 ^ lsb2 msb2 )
	incf	fsr,f			; ( lsb1 ^ msb3 lsb2 msb2 )
	subwf	indf,f			; lsb3 = lsb1 - lsb2
	decf	fsr,f			; ( lsb3 msb3 ^ lsb2 msb2 )
	btfss	status,c		; underflow?
	decf	indf,f			; msb3
	retlw	0


;;;
;;; sb_abs, sixteen bit absolute value
;;;
;;; data stack:	( a -- |a| ) [total 2]
;;; stack:	none
;;;
sb_abs_
	btfss   indf,7			; is it negative?
	retlw	0			; no, so return
					; fall through to negate
	
;;;
;;; sb_negate, sixteen bit negate
;;;
;;; data stack:	( a -- 0-a ) [total 2]
;;; stack:	none
;;;
sb_negate_				; ( lsb msb -- lsb msb )
	incf	fsr,f			; ( lsb ^ msb )
	comf    indf,f			; lsb
	incf    indf,f			; lsb
	decfsz	fsr,f			; ( lsb msb ^ )
	nop				; [decrement without losing z]
	btfsc   status,z
	decf    indf,f			; msb
	comf    indf,f			; msb
	retlw	0			; ( lsb msb )


;;;
;;; sb_nip, sixteen bit nip, remove item under current item
;;;
;;; data stack:	( a b -- b ) [total 4]
;;; stack:	none
;;;
sb_nip_					; ( a b c d -- c d )
	movf	indf,w			; d @
	incf	fsr,f			; ->c
	incf	fsr,f			; ->b
	movwf	indf			; b !
	decf	fsr,f			; ->c
	movf	indf,w			; c @
	incf	fsr,f			; ->b
	incf	fsr,f			; ->a
	movwf	indf			; a !
	decf	fsr,f			; ->b
	retlw	0


;;;
;;; sb_over, sixteen bit over, copy item under current item
;;;
;;; data stack:	( a b -- a b a ) [total 6]
;;; stack:	none
;;;
sb_over_				; ( a b c d -- a b c d a b )
	incf	fsr,f			; ( a b c ^ d )
	incf	fsr,f			; ( a b ^ c d )
	incf	fsr,f			; ( a ^ b c d )
	movf	indf,w			; a
	decf	fsr,f			; ( a b ^ c d )
	decf	fsr,f			; ( a b c ^ d )
	decf	fsr,f			; ( a b c d ^ )
	decf	fsr,f			; ( a b c d ? ^ )
	movwf	indf			; a
	incf	fsr,f			; ( a b c d ^ a )
	incf	fsr,f			; ( a b c ^ d a )
	incf	fsr,f			; ( a b ^ c d a )
	movf	indf,w			; b
	decf	fsr,f			; ( a b c ^ d a )
	decf	fsr,f			; ( a b c d ^ a )
	decf	fsr,f			; ( a b c d a ^ )
	decf	fsr,f			; ( a b c d a ? ^ )
	movwf	indf			; b
	retlw	0

		
;;;
;;; sb_multiply, sixteen bit multiply
;;;
;;; data stack:	( multiplicand multiplier -- product ) [total 7]
;;; stack:	calls others that do not call
;;;
sb_multiply_				; ( al ah bl bh -- cl ch )
	pushl16	d'0'			; clear product
	;; ( al ah bl bh cl ch )

	pushl	d'16'			; count of bits to process
	;; ( al ah bl bh cl ch count )

sb_multiply_loop		; for each bit
	;; ( al ah bl bh cl ch count )
	
	;; shift multiplier down by one
	movlw	5
	addwf	fsr,f			; ->ah
	rrf	indf,f			
	incf	fsr,f			; ->al
	rrf	indf,f
	decf	fsr,f			; ->ah
	decf	fsr,f			; ->bl

	;; ( al ah bl ^ bh cl ch count )
	;; if the bit is set ...
	btfss	status,c
	goto	sb_multiply_skip
	
	;; add the multiplicand to the product
	decf	fsr,f			; ->bh
	movf	indf,w
	decf	fsr,f			; ->cl
	decf	fsr,f			; ->ch
	addwf	indf,f
	movlw	3
	addwf	fsr,f			; ->bl
	movf	indf,w
	decf	fsr,f			; ->bh
	decf	fsr,f			; ->cl
	addwf	indf,f
	decf	fsr,f			; ->ch
	btfsc	status,c
	incf	indf,f
	movlw	3
	addwf	fsr,f			; ->bl

sb_multiply_skip
	;; ( al ah bl ^ bh cl ch count )
	;; shift up multiplicand
	bcf	status,c
	rlf	indf,f
	decf	fsr,f			; ->bh
	rlf	indf,f

	;; ( al ah bl bh ^ cl ch count )
	;; and loop for remainder of bits
	movlw	3
	subwf	fsr,f			; ->count
	decfsz	indf,f
	goto	sb_multiply_loop

	;; ( al ah bl bh cl ch count )
	popl				; count
	call	sb_nip			; bl bh
	goto	sb_nip			; al ah


;;;
;;; sb_divide, sixteen bit divide
;;;
;;; data stack:	( numerator denominator -- remainder quotient ) [total 10]
;;; stack:	calls others that do not call
;;;
sb_divide_		; 
	;; ( nl nh dl dh -- rl rh ql qh )
	
	;; prepare stack for results
	;; ( nl nh dl dh )
	call	sb_over			
	;; ( nl nh dl dh nl nh )
	call	sb_over
	;; ( nl nh dl dh nl nh dl dh )
	;; ( rl rh ql qh nl nh dl dh )
	movlw	d'4'
	addwf	fsr,f			; ->qh
	clrf	indf			; qh
	incf	fsr,f			; ->ql
	clrf	indf			; ql
	incf	fsr,f			; ->rh
	clrf	indf			; rh
	incf	fsr,f			; ->rl
	clrf	indf			; rl
	movlw	d'7'
	subwf	fsr,f			; ->dh
	;; ( rl rh ql qh nl nh dl dh )
	
	;; save effective sign difference
	;; sign = xor(nh,dh)
	;; ( nl nh dl dh )
	movf    indf,w			; dh
	incf	fsr,f			; 
	incf	fsr,f			; ->nh
	xorwf	indf,w			; nh
	decf	fsr,f			; 
	decf	fsr,f			; ->dh
	pushw
	;; ( nl nh dl dh sign )

	;; force arguments to positive
	;; n = abs (n)
	;; ( nl nh dl dh sign )
	movlw	d'3'
	addwf	fsr,f			; ->nh
	call	sb_abs
	decf	fsr,f			; ->dl
	decf	fsr,f			; ->dh
	
	;; d = abs (d)
	;; ( nl nh dl dh ^ sign )
	call	sb_abs
	decf	fsr,f			; ->sign

	;; set the bit counter
	pushl	d'16'			; for 16 shifts
	;; ( nl nh dl dh sign count )

	;; ( rl rh ql qh nl nh dl dh sign count )
	
sb_divide_loop
	;; ( rl rh ql qh nl nh dl dh sign count )

	;; shift bits left from numerator to remainder
	movlw	d'5'
	addwf	fsr,f			; ->nl
	bcf     status,c		; clear status.c
	rlf	indf,f			; nl
	decf	fsr,f			; ->nh
	rlf	indf,f			; nh
	incf	fsr,f			; (must keep status.c)
	incf	fsr,f			; 
	incf	fsr,f			; 
	incf	fsr,f			; 
	incf	fsr,f			; ->rl
	rlf	indf,f			; rl
	decf	fsr,f			; ->rh
	rlf	indf,f			; rh
	;; ( rl rh ^ ql qh nl nh dl dh sign count )
	
	;; check if remainder is greater than denominator
	movlw	d'6'
	subwf	fsr,f			; ->dh
	movf	indf,w			; dh
	movwf	r_ephemeral
	movlw	d'6'
	addwf	fsr,f			; ->rh
	movf	r_ephemeral,w
	subwf	indf,w			; rh
	btfss	status,z
	goto	sb_divide_skip_1
	;; ( rl rh ^ ql qh nl nh dl dh sign count )
	
	;; msb equal, so check lsb
	movlw	d'5'
	subwf	fsr,f			; ->dl
	movf	indf,w			; dl
	movwf	r_ephemeral
	movlw	d'6'
	addwf	fsr,f			; ->rl
	movf	r_ephemeral,w
	subwf	indf,w			; rl
	decf	fsr,f			; ->rh
	;; ( rl rh ^ ql qh nl nh dl dh sign count )
	
sb_divide_skip_1
	btfss	status,c
	goto	sb_divide_skip_2	; remainder is less
	;; ( rl rh ^ ql qh nl nh dl dh sign count )
	
	;; carry set, remainder is greater than denominator
	;; subtract denominator from remainder and save in remainder
	movlw	d'5'
	subwf	fsr,f			; ->dl
	movf	indf,w			; dl
	movwf	r_ephemeral
	movlw	d'6'
	addwf	fsr,f			; ->rl
	movf	r_ephemeral,w
	subwf	indf,f			; rl
	decf	fsr,f			; ->rh
	btfss   status,c
	decf	indf,f			; rh

	movlw	d'6'
	subwf	fsr,f			; ->dh
	movf	indf,w			; dh
	movwf	r_ephemeral
	movlw	d'6'
	addwf	fsr,f			; ->rh
	movf	r_ephemeral,w
	subwf	indf,f			; rh
	
	bsf     status,c		; shift a 1 into quotient
	;; ( rl rh ^ ql qh nl nh dl dh sign count )
	
sb_divide_skip_2
	;; shift the quotient left
	decf	fsr,f			; ->ql
	rlf	indf,f			; ql
	decf	fsr,f			; ->qh
	rlf	indf,f			; qh
	
	;; loop until all bits checked
	movlw	d'6'
	subwf	fsr,f			; ->count
	decfsz  indf,f			; count
	goto    sb_divide_loop
	popl				; drop count
	;; ( rl rh ql qh nl nh dl dh sign )

	;; adjust stack
	popf	r_ephemeral
	movlw	d'4'
	addwf	fsr,f			; drop nl nh dl dh
	;; ( rl rh ql qh )
	
	;; check effective sign difference and adjust quotient
	btfss   r_ephemeral,7
	retlw	0			; signs were same
	
	;; signs different, negate quotient
	goto	sb_negate


;;; bank 1 follows
	

	ifdef	__12c509
	org	0x200		; push main to high bank
	endif


;;;
;;; main program
;;; 

main
	;; clear stack
	movlw	stack
	movwf	fsr
	
	;; clear output port
	clrf	port
	
	;; clear tris mirror
	clrf	r_trism
	bsf	r_trism,b_ow0
	bsf	r_trism,b_ow1
	bsf	r_trism,b_in
	bsf	r_trism,b_ow2
	bsf	r_trism,b_ow3
	movf	r_trism,w	
	movwt

	;; set option register to enable pull-ups and enable GP2
	ifdef	__12c509
	movlw	b'10011111'	; NOT_GPPU, T0CS
	option
	endif

	;; set option register
	ifdef	__16f84
	;; enable port b weak pullups (NOT_RBPU=0)
	;; free RA4/TOCKI pin (T0CS=0)
	;; prescaler to WDT (PSA=1)
	;; prescaler at 1:64, 18ms x 64 = 1.152sec
	movlw	b'01011110'
	movwf	OPTION_REG
	endif

	;; pause for 0.5s to let supply stabilise before chatting
	pushl	d'195'
pause_loop
	pushl	d'0'			; 2.65ms
	bank0
	call	us10
	bank1
	decfsz	indf,f
	goto	pause_loop
	popl

	;; test for fahrenheit mode, gp3 low
	;; (if left open, it is pulled up to high)
	btfss	port,b_in
	bsf	r_trism,b_fahre
	
	ifdef	notdef
noisy
	;; need to account for gp0 being input now
	;; test for noisy mode; gp0 connected to gp3
	bank1
	bsf	port,gp0	; turn on gp0
	btfss	port,gp3	; see if gp3 
	goto	noisy_skip
	bcf	port,gp0
	btfsc	port,gp3
	goto	noisy_skip
	bsf	b_noisy,r_trism
	goto	noisy_done

noisy_skip
	
noisy_done
	endif
		
	;; say g'day
gday
	pushl	'R'			; reset
	bank0
	call	tx_header

	pushl	0			; start at beginning of table
gday_loop
	movf	indf,w			; pull down index
	bank0
	call	t_gday			; get the byte from the string
	iorlw	0			; is zero?
	bank1
	bz	gday_exit		; if so, exit loop
	pushw
	bank0
	call	tx_byte			; transmit the character
	incf	indf,f			; increment index
	bank1
	goto	gday_loop
gday_exit

	;; print C or F according to b_fahre
	movlw	'C'			; assume low
	btfsc	r_trism,b_fahre
	movlw	'F'			; set high if non zero
	pushw

	bank0
	call	tx_space
	call	tx_byte			; display the F or C
	call	tx_crlf			; followed by a new line pair
	
	;; select starting sensor
	movlw	1<<b_ow0
	movwf	r_which

	;; main loop begins here
loop
	
	;; determine displayable sensor number
	movlw	0		; initialise sensor counter
	movwf	r_ephemeral
	movf	r_which,w	; copy selection mask to temporary
	movwf	r_temporary
	bank1
	
which_loop
	btfsc	r_temporary,0	; have we shifted the bit down yet?
	goto	which_exit	; yes, exit the loop
	rrf	r_temporary,f	; shift the bit down
	incf	r_ephemeral,f	; increment the sensor number
	goto	which_loop
	
which_exit
	movf	r_ephemeral,w
	bank0
	call	t_which		; translate to printable
	pushw			; save on stack
	
	;; reset bus to determine if sensor is present
	;; bank0
	call	ow_reset		; ( -- flag )
	popw
	iorlw	0x0			; set zero flag if sensor absent
	bank1
	bnz	present

	;; if non-verbose, don't report pins without sensors
	;; bank1
	;; btfss	port,b_in		; verbose mode on?
	goto	hello_skip		; no, skip it

	;; report a sensorless pin
	bank0
	call	tx_header		; transmit the sensor number

	movf	r_which,w		; fetch the selection mask
	andwf	port,w			; keep the port bit we want
	movlw	'L'			; assume low
	skpz
	movlw	'H'			; set high if non zero
	pushw

	;; bank0
	call	tx_byte			; display the L or H
	call	tx_crlf
	bank1
	goto	absent
	
hello_skip
	popl				; drop the sensor number
	goto	absent

present
	bank0
	call	tx_header		; transmit the sensor number
	
	;; skip rom check sequence
	pushl	ow_command_skip
	bank0
	call	ow_tx

	;; commence conversion
	pushl	ow_command_convert
	call	ow_tx

	;; wait until sensor not busy
	
	;; ??? we wait until we get eight bits of 1's, which could take
	;; longer than just waiting for one bit of 1.  try waiting for 1?

	;; ds1820 specification says 200ms typical, 500ms maximum
busy
	bank0
	call	ow_rx			; fetch a byte
	popw				; pop the byte from stack
	xorlw	0xff			; test for 0xff
	bank1
	bnz	busy			; loop if not equal

	;; ask sensor for data

	;; reset bus
	bank0
	call	ow_reset		; ( -- flag )
	popw
	iorlw	0x0			; set zero flag if sensor absent
	bank1
	bz	absent

	;; skip rom check sequence
	pushl	ow_command_skip
	bank0
	call	ow_tx

	;; request data
	pushl	ow_command_read
	call	ow_tx
	
	;; read data from sensor
ow_get	macro	m_byte		; get byte to address
	;; bank0
	call	ow_rx
	popf	m_byte
	endm
	
	ow_get	r_ds_lsb
	ow_get	r_ds_msb
	ow_get	r_ds_rem	; ignore low alarm byte
	ow_get	r_ds_rem	; ignore high alarm byte
	ow_get	r_ds_rem	; ignore reserved byte
	ow_get	r_ds_rem	; ignore reserved byte
	ow_get	r_ds_rem
	ow_get	r_ds_count
	ow_get	r_ds_crc

	;; ??? check the crc some day!

	;; in verbose mode, dump the data in hex
data
	bank1
	;; btfss	port,b_in		; verbose mode on?
	goto	data_skip		; no, skip it
	
	tx_put	r_ds_lsb
	tx_put	r_ds_msb
	tx_put	r_ds_rem
	tx_put	r_ds_count
	tx_put	r_ds_crc
	call	tx_space
data_skip

;;;
;;; dump stack pointer at top value
;;; 
	if	debug == 1
dump	macro	m_label
	pushl	m_label			; print the label byte
	bank0
	call	tx_byte
	tx_put	fsr			; ??? stack check
	call	tx_space
	endm
	else
dump	macro	m_label			; null macro
	endm
	endif

;;;
;;; Calculation of high resolution temperature from the count per degrees
;;; c and remaining count values fetched from the DS1820 scratchpad.
;;;
;;; high-res-temp = ( temperature >> 1 ) - 0.25
;;;                 + (count-per-c - count-remain) / (count-per-c)
;;; 
;;; Problem:	Result is truncated to two decimal places rather than being
;;;		rounded.  Solution not yet planned.
;;; 

	;; move temperature as read to accumulator
	pushf16	r_ds_lsb

	;; shift right to truncate 0.5C LSB
	bcf	status,c	; start with a clear carry flag
	rrf	indf,f
	incf	fsr,f
	rrf	indf,f
	decf	fsr,f

	;; multiply by 100 to scale to hundredths of degrees
	pushl16	d'100'
	bank0
	call	sb_multiply
	dump	'A'

	;; subtract 25 from accumulator
	pushl16	d'25'
	;; bank0
	call	sb_subtract
	dump	'B'

	;; keep on stack for later use

	;; take eight bit count per degrees c
	;; subtract count remaining
	movf	r_ds_rem,w	; get count remaining
	subwf	r_ds_count,w	; subtract it from count per degrees c
	pushw			; place on stack
	pushl	d'0'		; extend to sixteen bits

	;; multiply by 100 to scale
	pushl16	d'100'
	;; bank0
	call	sb_multiply
	dump	'C'

	;; divide by count per degrees c
	pushf	r_ds_count
	pushl	d'0'			; extend to sixteen bits
	;; bank0
	call	sb_divide
	call	sb_nip			; drop remainder
	dump	'D'

	;; we now have offset from temperature in hundredths of degrees
	;; add to previouosly saved value

	;; bank0
	call	sb_add
	dump	'E'

	;; we now have the measurement in degrees centigrade, scaled by
	;; 100, such that 23.54 degrees is binary value for decimal 2354
	
	;; convert to fahrenheit
	bank1
	btfss	r_trism,b_fahre		; if bit zero, degrees C
	goto	fahrenheit_skip		; yes, skip it
	
	bank0
	pushl16	d'9'		; multiply by nine
	call	sb_multiply

	pushl16	d'5'		; divide by five (therefore multiply by 1.8)
	call	sb_divide
	call	sb_nip		; drop remainder

	pushl	d'128'		; add thirty two (3200)
	pushl	d'12'
	call	sb_add
	
fahrenheit_skip
	;; check sign of result
	bank1
	btfss	indf,7		; sign bit of signed integer result
	goto	skip		; bit cleared, it is positive

	;; issue negative sign to output
	pushl	'-'
	bank0
	call	tx_byte

	;; subtract from zero
	call	sb_negate

skip
	
	;; convert value to bcd
	bank0
	call	sb_bcd
	dump	'F'

	;; transmit the bcd output buffer
	call	tx_byte_hex		; d2 (msd)
	call	tx_byte_hex		; d1
	pushl	'.'			; interject the decimal place '0655.35'
	call	tx_byte
	call	tx_byte_hex		; d0 (lsd)

	;; finish the output off with a newline
	call	tx_crlf

	
	;; delay for a while between outputs, try to get 500ms
	bank1
	;; btfsc	port,b_in		; verbose mode on?
	;; goto	snooze_skip		; yes, skip it
	
	;; (above takes 279ms, so we need 221ms)
	pushl	d'85'			; ( outer ) [experimental figure]
snooze_loop
	pushl	d'250'			; ( outer inner )
	bank0
	call	us10
	bank1
	decfsz	indf,f			; ( outer )
	goto	snooze_loop
	popl				; ( )
snooze_skip
	
absent
	;; switch to next sensor by spinning the mask around to next combo
	
	btfss	r_which,7		; propogate bit 7 to carry flag
	bcf	status,c
	btfsc	r_which,7
	bsf	status,c

	rlf	r_which,f		; logical left rotate (spin bits)

	movlw	m_ow			; is it a bit we can use?
	andwf	r_which,w
	bz	absent			; no, so spin it again
	
	;; clear watchdog timer!
	clrwdt
	
	;; and do it all again
	goto	loop

last2	retlw	0		; define the last byte used in page 2/3

	;; ease determination of last used data address
	cblock
		r_zzz
	endc

	;; calibration for 12C509-JW part
	ifdef	__12c509
	ifdef	jw
	org	0x3ff
	movlw	0x50			; 12c509 #1
	; movlw	0x30			; 12c509 #2
	endif
	endif

	;; cross-checks
	ifdef	__12c509
	if	r_zzz > 0x1f
	messg	"error:	last allocated file register is not in bank 0"
	endif
	if	last0 > 0xff
	messg	"error: last function in page 0 is beyond call limit"
	endif
	if	last2 > 0x3ff
	messg	"error:	code in page 3 runs over to page 4 (>0x3ff)"
	endif
	endif
	end
