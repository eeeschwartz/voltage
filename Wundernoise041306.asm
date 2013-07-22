; --WunderNoise--
; TMB
; Original: 10/31/05
; Rebuilt to be more better for Spring 2006 Tour-
; 4/2/06
; Still EEpromless but tour ready with five programs that drop bombs. 
4/13/06 -TB

;  The following is the code for the 12f683 which drives the "Who Got A Big 
Ol' Butt" kit made for the Voltage
;tour, fall 2005.  Fundamentally it is a ghetto dsp device which makes a lot 
of "guesses" about an incoming
;analog signal using the a/d and comparator and outputs different gibberish, 
waveforms and whatnot.
;It reminds me of a little accountant, hiring and firing.  Cutting checks on 
the spot.

;The idea is that with the one button you can engage / disengage the circuit 
from the world (turn the output into an input)
;But if you hold said magic button the circuit will change to a different 
program.  This program gets stored in EEPROM so that
;each particular circuit remembers what it's supposed to be after power off. 
Maybe the PWM to the LED is different for each one
;in some artsy way.

;The concepts for some of the routines:

;-------See notes in MoreWunderNoise.txt



;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Compiler Setup
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------


WunderNoise_setup

		processor		12f683
		include			<p12f683.inc>
		__config		_CP_OFF & _WDT_OFF & _PWRTE_ON & _BOD_OFF & _MCLRE_OFF & 
_INTRC_OSC_NOCLKOUT
		org			0
		cblock			h'20'
			randomval4
			randomval3
			randomval2
			randomval1
			debounce_counter
			switch_held_counter
			switch_register
			program_selector
			output_state
			ADresult
			LEDchase
			currentDelay
			delayCounter
			loopCounter
			comparatorReg
			IIRL
			IIRH
			timer_start_valueL
			timer_start_valueH
			times_through_loop
			current_timer_readingL
			current_timer_readingH
			tempL
			tempH
			temp1L
			temp1H
			step_timer_counter
			debugCounter
		endc


;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Device Pinout:
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Device =
;12F683

;			-----------------
;			|  *			|
;		  VDD				VSS
;			|				|
;		Output				Comparator In
;			|				|
;	Analog In				Comparator Reference
;			|				|
;	   Switch				LED Drive
;			|				|
;			-----------------

;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Defines:
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------

		#define		SWITCH_INPUT		GPIO,3
		#define		OUTPUT_PIN			GPIO,5

		#define		DEBOUNCE_TIME		d'255'
		#define		HOLD_TIME			d'255'
		#define		ALREADY_PRESSED		switch_register,0
		#define		VALID_SWITCH		switch_register,1
		#define		TIMER_FLIPPED_FLAG	switch_register,2
		#define		RESET_PROGRAM_FLAG	switch_register,3
		#define		CHANGE_PROGRAM_FLAG switch_register,4

		#define		OUTPUT_STATE_FLAG	output_state,0
		#define		NOISE_BIT			randomval1,0
		#define		NOISE_THRESHOLD		d'175'
		#define		DELAY_CONSTANT		d'8'				; This is the loop multiplier to 
determine noise freq.  Greater = lower f.
		#define		STEP_TIME_CONSTANT	d'30'				; This value determines the speed of 
the stepped tone generator.  Lower = faster.
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Initialization on Powerup.
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------

;Get your options, input interrupts, tris, A/D setup and the like going.
;Clear your I/Os and get your banks straight.

power_on_routine

		bcf			STATUS, RP0		;bank 0
		clrf		INTCON			;Turns interrupts off.
		movlw		b'00000010'
		movwf		CMCON0			;Comparator is on, taking input from the pins, and 
non-inverted.  No external O/P.
		movlw		b'0001101'
		movwf		ADCON0			;A/D on, left justified, referenced to VDD, channel 3 (pin 
3).
		clrf		CMCON1			;Comparator runs asyncronously.
		clrf		GPIO			;clears outputs.
		bsf			STATUS, RP0		;bank1

		movlw		b'00111011'
		movwf		TRISIO			;GP0,1,3,4,5 = inputs, GP2 = outputs.
								;Remember to turn 5 back into an output when it's time to flip it!
		movlw		b'01110111'		;Sets the system clock to the internal oscillator and
									;ratchets it up to 8MHz.
		movwf		OSCCON			;Osccon is in bank 1.

		movlw		b'01011000'
		movwf		ANSEL			;AN3 to analog input, conversion clock = Fosc / 16.
		clrf		VRCON			;Voltage reference powered down.
		movlw		b'10000000'
		movwf		OPTION_REG		;sets pullups off.

		bcf			STATUS, RP0		;bank 0
		clrf		GPIO			;clear O/Ps again because I'm paranoid.

		call		init_PWM		;Get the LED poppin.

		movf		TMR0,W			;Seed the random number generator.
		movwf		randomval4
		movwf		randomval3
		movwf		randomval2
		movwf		randomval1

;Initialize your variables

		movlw		DEBOUNCE_TIME
		movwf		debounce_counter
		clrf		switch_held_counter
		clrf		switch_register
		clrf		program_selector
;		movlw		b'11111111'			;Why did I do this?
;		movwf		output_state
		bcf			ALREADY_PRESSED		;Switch isn't already pressed.
		bcf			VALID_SWITCH		;Switch isn't valid.
		bcf			RESET_PROGRAM_FLAG
		bcf			CHANGE_PROGRAM_FLAG
		clrf		output_state
		movlw		d'128'
		movwf		ADresult
		movwf		ADRESH
		clrf		LEDchase
		movlw		d'127'			;initialize to low frequency.
		movwf		currentDelay
		movlw		DELAY_CONSTANT
		movwf		delayCounter
		clrf		comparatorReg

;		call		readprom		;Get the hot shit from EEPROM.

		goto		program_selector_tree


;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Program Selector Loop
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------

next_program
		incf		program_selector
		bcf			CHANGE_PROGRAM_FLAG
		bcf			RESET_PROGRAM_FLAG
		bsf			OUTPUT_STATE_FLAG		;Always start with the shit killin'.
											;This is also set in the "switch held" routine immediately 
previously.
program_selector_tree
;The main loop just shuttles us off to whichever program we dug out of 
eeprom in Setup,
;or puts us in a new program after the button is held.

		movlw		b'00000000'				;Was it program 0?
		clrz
		xorwf		program_selector,W
		skpnz
		goto		program_0_setup

		movlw		b'00000001'				;Was it program 1?
		clrz
		xorwf		program_selector,W
		skpnz
		goto		program_1_setup

		movlw		b'00000010'				;Was it program 2?
		clrz
		xorwf		program_selector,W
		skpnz
		goto		program_2_setup

		movlw		b'00000011'				;Was it program 3?
		clrz
		xorwf		program_selector,W
		skpnz
		goto		program_3_setup

		movlw		b'00000100'				;Was it stepped tone?
		clrz
		xorwf		program_selector,W
		skpnz
		goto		stepped_tone_program

;And so on and so forth....

		clrf		program_selector
		goto		program_0_setup				;In case some weird number gets in here, or you 
increment over the number of programs,
												;just go to program 0.

;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Synth Programs:
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------

program_0_setup

	bsf		STATUS,RP0			;Bank 1
	bsf		TRISIO,5			;Turn the OP into a high impedance input.
	movlw	b'01110111'			;Sets the system clock to the internal oscillator and
								;ratchets it up to 8MHz.
	movwf	OSCCON				;Osccon is in bank 1.
	movlw	b'01011000'
	movwf	ANSEL				;AN3 to analog input, conversion clock = Fosc / 16.
	bcf		STATUS,RP0			;Bank 0

	movlw	b'00001100'			;CCP set to PWM.
	movwf	CCP1CON
	bcf		RESET_PROGRAM_FLAG		;If we've comeback from an idle time this is the 
flag that we reset.

;(45 commands long when the button is not pressed, so the loop time is based 
on the A/D
;cycle, and therefore around 56 commands or so.)

;The first of the shit-kickers:
;This program reads the A/D and if the level in is outside of a certain 
range (ie, the signal's amplitude is
;greater than a threshold) it turns on the output pin and sends it the PSRB. 
   It must read lower than the threshold for
;a millisecond (or so, about 36 complete A/D cycles) before it turns off the 
PSRB -- since there's no averaging on the signal this
;is just a sort of hysteresis or attempt to make sure that a chance read 
near the zero point doesn't turn off the signal.

program_0

	btfss	SWITCH_INPUT
	goto	un_pressed

	call 	debounce
	call	on_off_switch_handler
	goto	switch_hooey_finished

un_pressed

	movlw	DEBOUNCE_TIME
	movwf	debounce_counter
	bcf		ALREADY_PRESSED
	clrf	switch_held_counter

switch_hooey_finished

	call	random32			;Keep it random while you're waiting.
	btfss	OUTPUT_STATE_FLAG		;If the thing is supposed to be inactive, go to 
another loop which just turns off the OP, diddles the LED and waits on 
switches.
	call	bide_your_time
	btfsc	RESET_PROGRAM_FLAG		;If we return from a bide_your_time call to this 
point, this flag tells us to re-init everything.
	goto	program_0_setup
	btfsc	CHANGE_PROGRAM_FLAG		;If the Change flag got tripped, break out of 
this program to the next_program section.
	goto	next_program

	;Wait for the conversion now, since eveything is done.

	btfsc	ADCON0,GO
	goto	$-1

	;In 7 cycles, start the A/D again.

	movf	ADRESH,W
	movwf	ADresult

						; If the AD reading is less than 128, complement it -- a software 
absolute value above the reference voltage.
	movlw	d'128'
	clrc
	subwf	ADresult,W
	skpc
	comf	ADresult

	bsf		ADCON0,GO
	;And they're off!  The A/D should be done roughly 48 commands from now, and 
ready to
	;go again in 8 after that.  So it'd be nice to catch the end of the 
conversion so I know
	;where to count 8 from -- it'll be fastest that way. (see above)

	;(the switch checking and RNG at the top of the loop take 12-20 cycles.  
Try and be done in another 28 from here)

	;Next is the compare section.  If the result is higher than a threshold, 
bring tha noise.


	movlw	NOISE_THRESHOLD
	bsf	STATUS,C			; Set the C bit so that subtracting through zero causes it 
to clear.
	subwf	ADresult,W			; ADresult minus W.
	skpc					; If the ADresult is bigger, there's no borrow, nothing changes.  
C remains set.
	goto 	input_less_than_threshold	; Therefore, if the C bit is still set, 
make some noise.  If it is now clear, the reading was lower than the 
threshold -- don't do shit.

input_greater_than_threshold
	;Turn on the OP port and LED.  If you have the cycles, debounce, but 
whatever.

	bsf	STATUS,RP0			;Bank 1
	bcf	TRISIO,5			;Turn the OP into an OP.
	bcf	STATUS,RP0			;Bank 0
	btfss	NOISE_BIT			;Check the bit on the RNG and put it on the output pin.
	bcf	OUTPUT_PIN
	btfsc	NOISE_BIT
	bsf	OUTPUT_PIN
	movlw	b'11111111'			;Turn up the LED all the way.
	movwf	CCPR1L
	goto 	program_0			;You're done, start the loop again.

input_less_than_threshold

	bsf	STATUS,RP0			;Bank 1
	bsf	TRISIO,5			;Turn the OP into a high impedance input.
	bcf	STATUS,RP0			;Bank 0

	rrf	ADresult,W			;LED shit:
	clrf	CCPR1L				; A test to see if the input always stays less than the 
threshold.
;	movwf	CCPR1L				; If the noise is off, make the led correspond to input 
amplitude, but half max brightness.
	goto	program_0			;You're done, start the loop again.


;------------------------------
program_1_setup

	bsf		STATUS,RP0			;Bank 1
	movlw	b'01110111'			;Sets the system clock to the internal oscillator and
								;ratchets it up to 8MHz.
	movwf	OSCCON				;Osccon is in bank 1.
	movlw	b'01011000'
	movwf	ANSEL				;AN3 to analog input, conversion clock = Fosc / 16.
	bcf		STATUS,RP0			;Bank 0

	movlw	b'00001100'			;CCP set to PWM.
	movwf	CCP1CON

;2.)  Amplitude-dependent filtered white noise
;Lil' Blood looks at the A/D value and adjusts the frequency of the PSRB, 
which is always
;on.  With small or no signal in, There are a lot of DC pops.  As the 
amplitude pins out,
;the shit gets hissy.

;Check the button, like before.
;When the program is engaged, keep the OP an OP.

;Take the AD reading, figure absolute value via complement-
;if A/D - 128 flips the Carry, complement.
;So then a reading of: 0 = 255 and 255 = 255 and 127=128 and 128=128.
;Complement this again.
;Now: 255 = 0 and 128=127.
;This means that we'll get a number back from 0-127 or so where 0 equals the 
greatest input amplitude, and
;127 equals the smallest input amplitude.
;Now use this to delay moving the Noise bit to the O/P.

;Basically, this program shoots out random bits.
;It changes the time until the next random bit based on the input magnitude.
;The greater the input magnitude (amplitude), the faster the bits go out.

;So:

;Run the A/D full speed.
;Turn the a/d into a delay time. Something like ((0-127)*32) = cycles 
through a 56 command program per noise bit check.
;Compare the Delay calculated from the A/D reading to the current delay 
counter.
;If the calculated delay is shorter, reload the delay counter with the new 
delay.
;If the calculated delay is longer, (ie, the amplitude is smaller than it 
was) keep the old delay counter and
;re-calculate at the next A/D reading.

	;First thing, turn on the OP so you don't waste these cycles every loop -- 
this stays on as long as the
	;program is active.

	bsf		STATUS,RP0			;Bank 1
	bcf		TRISIO,5			;Turn the OP into an OP.
	bcf		STATUS,RP0			;Bank 0
	bcf		RESET_PROGRAM_FLAG

program_1_loop
	decf	currentDelay		; A carryover from the OP state / freq evaluation at the 
end of the loop.

	btfss	SWITCH_INPUT
	goto	un_pressed_a

	call 	debounce
	call	on_off_switch_handler
	goto	switch_hooey_finished_a

un_pressed_a

	movlw	DEBOUNCE_TIME
	movwf	debounce_counter
	bcf		ALREADY_PRESSED
	clrf	switch_held_counter


switch_hooey_finished_a

	btfss	OUTPUT_STATE_FLAG		;If the thing is supposed to be inactive, go to 
another loop which just turns off the OP, diddles the LED and waits on 
switches.
	call	bide_your_time
	btfsc	RESET_PROGRAM_FLAG		;If we return from a bide_your_time call to this 
point, this flag tells us to re-init everything.
	goto	program_1_setup
	btfsc	CHANGE_PROGRAM_FLAG		;If the Change flag got tripped, break out of 
this program to the next_program section.
	goto	next_program
	btfsc	ADCON0,GO				;Are we done yet?
	goto	$-1

	;Get the data.  In 7 cycles, you're clear to start the A/D again.
	; If the AD reading is less than 128, complement it -- a software absolute 
value above the reference voltage.

	movf	ADRESH,W
	movwf	ADresult
	movlw	d'128'

	bsf		STATUS,C
	subwf	ADresult,W
	skpc
	comf	ADresult
	comf	ADresult

;	clrc
;	subwf	ADresult,W
;	skpc
;	comf	ADresult
						;ADresult should = 128-255 now.
;	comf	ADresult
						;Adresult should = 0-127 now.
						;ADresult will = 127 ADresultwith no signal in.
						;ADresult is basically a seven bit number now, inversely proportional 
to the signal amplitude.

	bsf		ADCON0,GO	;Restart that ish.  Get a new reading.

	;Now compare the AD result to the currentDelay.  If the ADresult is smaller 
(meaning that amplitude at the input is is
	;greater than it was last read, or greater than the equivalent current 
delay frequency) then load the ADresult into the
	;current delay value.  If the ADresult is greater, keep the currentDelay.

	movf	currentDelay,W
	bsf		STATUS,C
	subwf	ADresult,W					; This command is ADresult (f) minus currentDelay (W).
	skpnc			;(NB:skpc Locks the OP high.)		; If the C bit is still set, W was 
less than f, so the ADresult was greater.  So if C is set don't change the 
delay register.

currentDelayLessThanAD
	; If we're here, the ADresult (ie, the inverse of the signal magnitude) is 
greater than the value of the decay as it stands
	; Don't update the delay, just keep it moving.

	goto	calculationDone

currentDelayGreaterThanAD
	;This means the frequency of the output should increase, because the delay 
period is greater (longer) than what the
	;magnitude in would dictate.  Move ADresult into currentDelay and reset the 
delayCounter as well.

	movf	ADresult,W
	movwf	currentDelay
;	movlw	DELAY_CONSTANT			;Don't reset the delay counter here, because if the 
input stays at the same value, (like 0)
;	movwf	delayCounter			;OR if the AD value steadily increases, this keeps 
getting reloaded and never getting to the OP.

calculationDone
;Okay, now decrement the delay counter.  When it gets to zero, decrement the 
currentDelay.  When that gets to zero,
;update the bitstream and output.  Reset currentDelay to the default value 
of 127 or whatever it was.

;Keep it random:
	call	random32

	clrz
	decf	delayCounter
	skpz						;When the delay counter hits zero, reset it and worry about 
currentDelay.  Otherwise, don't mess -- return to the beginning of the loop.
	goto	program_1_loop

	movlw	DELAY_CONSTANT
	movwf	delayCounter

	clrw
	clrz
	iorwf	currentDelay		; Inclusive or 0 with currentDelay.  A result of zero 
means that currentDelay was zero.  Move to the OP section.
	skpz						; Otherwise, return to the loop.  IOR used because currentDelay 
can be loaded with 0 if the AD returns a 255,
								; in which case using DEC then SKPZ would lead to a LONG delay with 
the input pinned.
	goto	program_1_loop		; Decrement currentDelay 1st thing in the loop now, to 
save another case evaluation here.

	;If we're here, the counters have all gotten to zero.  Now worry about 
putting the random bit on the OP.

	btfss	NOISE_BIT			;Check the bit on the RNG and put it on the output pin.
	goto	bitLow

bitHigh

	bsf		OUTPUT_PIN
	movlw	b'11111111'					;Turn up the LED all the way.
	movwf	CCPR1L
	goto	program_1_loop				;You're done, start the loop again.

bitLow

	bcf		OUTPUT_PIN
	clrf	CCPR1L						;Turn down the LED all the way.
	goto	program_1_loop				;You're done, start the loop again.


;---------------------------------------
;---------------------------------------

program_2_setup
;This program is easy.  It looks at the comparator, and if the O/P of the 
comparator is high, it sets the O/P bit
;high.  If the comparator is low, the O/P goes low.  Easy.
;***** -- In here now is the comp XORED with TMR0.  It's not bad.
;***** -- See if you can't make the button (or something) somehow select the 
TMR0 freq.

	;First thing, turn on the OP so you don't waste these cycles every loop -- 
this stays on as long as the
	;program is active.

	bsf		STATUS,RP0		;Bank 1
	bcf		TRISIO,5		;Turn the OP into an OP.
	bcf		STATUS,RP0		;Bank 0

	bsf		STATUS,RP0		;Bank 1
	movlw	b'11000111'		;Pullups off, timer0 driven by internal clock, prescaler 
to timer0, prescaler = 1:256.
	movwf	OPTION_REG
	bcf		STATUS,RP0		;Bank 0


	movlw	b'00110101'			;Free run Timer 1 with max prescale (1/8) -- this gives 
us a frequency = (2Mhz) / 8 = 250kHz.  Hmmm.
	movwf	T1CON				;Both of these registers are in Bank 0.
	bsf		CMCON1,CMSYNC		;Sync the comparator with the falling edge of Timer1 
Clock.

	bcf		RESET_PROGRAM_FLAG

	;I think that's all there is to it.  But I don't think this'll matter much, 
audio-wise, as the T1 Clock is so fast.  You could ratchet down the CPU 
freq...

program_2_loop

	btfss	SWITCH_INPUT
	goto	un_pressed_b

	call 	debounce
	call	on_off_switch_handler
	goto	switch_hooey_finished_b

un_pressed_b

	movlw	DEBOUNCE_TIME
	movwf	debounce_counter
	bcf		ALREADY_PRESSED
	clrf	switch_held_counter


switch_hooey_finished_b

	btfss	OUTPUT_STATE_FLAG		;If the thing is supposed to be inactive, go to 
another loop which just turns off the OP, diddles the LED and waits on 
switches.
	call	bide_your_time
	btfsc	RESET_PROGRAM_FLAG		;If we return from a bide_your_time call to this 
point, this flag tells us to re-init everything.
	goto	program_2_setup
	btfsc	CHANGE_PROGRAM_FLAG		;If the Change flag got tripped, break out of 
this program to the next_program section.
	goto	next_program

	clrf	comparatorReg		;Set a bit based on the comparator OP.
	btfsc	CMCON0,COUT
	bsf		comparatorReg,4		;Change this bit to change the "frequency" of the 
"oscillator" you're logically XORing with the comparator.

	movf	comparatorReg,W
	xorwf	TMR0,W			;Effectively just XORing the comparator output with (bit 4*) 
of TMR0 -- ***MAKE SURE NOT TO WRITE TO TMR0 HERE***
						;The (bit 4*) of W is now what we want on the O/P.
						;* If we've anded the comparator w/ a different bit in TMR0, make sure 
to check that bit for the O/P.
						;NB -- try using a different logical operator than XOR.  Or, try just 
doing a logical operation on the result of
						;the A/D instead of TMR0 -- don't hang up the operation waiting on the 
A/D, just free run that nasty nas.
						;You could use the noise source instead of TMR0 or the A/D, too.

	movwf	comparatorReg			;You didn't need that register, did you?


	btfss	comparatorReg,4			;Test the bit and rock it.
	goto	XORLow
	bsf		OUTPUT_PIN
	movlw	b'11111111'
	movwf	CCPR1L
	goto	program_2_loop

XORLow

	bcf		OUTPUT_PIN
	clrf	CCPR1L
	goto	program_2_loop


;The loop below totally works, and sounds like a RAT pedal or something.  
Fun for shredding, but not winning any art awards.

;	movlw	d'10'					;Run this part of the loop this many times before 
bothering to check the switches again.  Make sure your debouncing works okay 
with this program.
;	movwf	loopCounter
;
;square_wave_loop
;
;	btfss	CMCON0,COUT				;Test the comparator and rock it.
;	bcf		OUTPUT_PIN
;	btfsc	CMCON0,COUT
;	bsf		OUTPUT_PIN
;	clrz
;	decf	loopCounter
;	skpz
;	goto	square_wave_loop
;	goto	program_2_loop

;--------------------------------


;---------------------------------------
;---------------------------------------

program_3_setup
;Program 3 examines square wave generation in a different way, by taking an 
A/D reading and using the most significant bit to
;set the square wave.  Mess with conversion speed to get this to sound 
different (aliasing vs max conversion rate violation)
;Right now the clock is scaled down to 4MHz and the A/D conversion speed is 
way slowed down.

	bsf		STATUS,RP0		;Bank 1
	bcf		TRISIO,5		;Turn the OP into an OP.

	movlw	b'01100111'		;Slows down the internal oscillator like screwed and 
chopped.
	movwf	OSCCON			;Osccon is in bank 1. (4Mhz sounds pretty good)
							;The switches in this program are all kind of fucked up -- run the 
oscillator back up for the
							;waiting period.

	movlw	b'01101000'		; 0110 is = 1/64, 0000 (MSBs)= 1/2.
	movwf	ANSEL			;AN3 to analog input, conversion clock optimal =  Fosc / 16. 
(now f/64)

	bcf		STATUS,RP0		;Bank 0
	bcf		RESET_PROGRAM_FLAG

program_3_loop

	btfss	SWITCH_INPUT
	goto	un_pressed_c

	call 	debounce
	call	on_off_switch_handler
	goto	switch_hooey_finished_c

un_pressed_c

	movlw	DEBOUNCE_TIME
	movwf	debounce_counter
	bcf		ALREADY_PRESSED
	clrf	switch_held_counter


switch_hooey_finished_c

	btfss	OUTPUT_STATE_FLAG		;If the thing is supposed to be inactive, go to 
another loop which just turns off the OP, diddles the LED and waits on 
switches.
	call	bide_your_time
	btfsc	RESET_PROGRAM_FLAG		;If we return from a bide_your_time call to this 
point, this flag tells us to re-init everything.
	goto	program_3_setup
	btfsc	CHANGE_PROGRAM_FLAG		;If the Change flag got tripped, break out of 
this program to the next_program section.
	goto	next_program

	;Wait for the conversion now, since eveything is done.

	btfsc	ADCON0,GO
	goto	$-1

	;In 7 cycles, start the A/D again.

	movf	ADRESH,W
	movwf	ADresult

	btfss	ADresult,7			;Test the bit and rock it.
	goto	goLow
	bsf		OUTPUT_PIN
	movlw	b'11111111'
	movwf	CCPR1L
	bsf		ADCON0,GO
	goto	program_3_loop

goLow

	bcf		OUTPUT_PIN
	clrf	CCPR1L
	bsf		ADCON0,GO
	goto	program_3_loop



;------------------------------
stepped_tone_program
;See notes.  This one's complicated.
;Initialize TMR1 and TMR0.  Set tmr0 and its counters to get the time 
between the steps (1/8th second, say)
;Initialize CCP to Compare -- You won't be able to PWM the LED in this 
program.

;Start the first routine and run it once:
;
;Note_one:
;Start TMR0 and any counters associated with it.
;Wait for the comparator to flip.
;Start TMR1.
;Wait for the comparator to flip again.
;When it does, get the value of TMR1, and reset the timer.
;Load this value into an IIR averaging filter like so:  newAverage = 
(average + currentValue) / 2
;Keep waiting for comparator flips, and averaging the time between them 
UNTIL
;TMR0 counts off the step length.  Now:
;Take the average from the IIR and put it into the CCP module.  Keep 
IIR_average.
;
;Now run the following loop forever:
;
;N_notes:
;Clear TMR0.  Clear TMR1.
;clear a variable "timer_start_value" to zero.
;clear a variable "times_through_loop" to zero.
;clear a "tmr_flipped" flag.
;Wait for TMR0 to flip again.  While you are:
;
;Run TMR1.  When TMR1 matches CCP, flip the OP and reset TMR1.
;  Check the TMR_flipped flag.  If it is clear, set the "tmr_flipped_flag".
;  If it is set, increment times_through_loop.
;
;Tracking the incoming frequency--
;We know TMR1 is resetting is dependent on the frequency being output, and 
independent of the input frequency.
;When the comparator flips, take the value of TMR1.  Call it 
current_tmr1_reading or something.  Reset TMR1 and keep it moving.
;Now operate on the results.
;Check the tmrflipped flag:
;  If it is clear, tmr1 hasn't flipped since the last comparator change.
;     Get the new period by simply taking (currentReading - 
timer_start_value).
;  If it is set, then tmr1 has reset to zero while the new frequency was 
being counted.
;     The new period is then (times_through_loop * (CCP_value)) + (CCP_value 
- timer_start_value) + (current_timer_reading)
;From this we get the new period between comparator changes.
;Now:
;Clear the times_through_loop variable and tmr_flipped flag.
;set timer_start_value to current_tmr1_reading.
;Run the IIR averaging filter on the new period.
;Wait for the next comparator flip or TMR1 flip and handle it as above.
;
;When TMR0 flips, take the Average from the IIR and put it in the CCP.  
Leave IIR loaded.  Restart the loop.

;Registers
;IIRL, IIRH
;timer_start_valueL
;timer_start_valueH
;times_through_loop		(I think you only need 8 bits here)
;TIMER_FLIPPED_FLAG     (just one bit in some other register)
;current_timer_readingL
;current_timer_readingH
;tempL
;tempH
;step_timer_counter

init_stepped_tone
;Setup TMR0, TMR1, and the CCP.
	bsf		STATUS,RP0		;Bank 1

	movlw	b'01110111'		;Sets the system clock to the internal oscillator and
							;ratchets it up to 8MHz.
	movwf	OSCCON			;Osccon is in bank 1.
	movlw	b'11000111'		;Pullups off, timer0 driven by internal clock, prescaler 
to timer0, prescaler = 1:256.
	movwf	OPTION_REG		;This means that TMR0 rolls over 30.5 times a second, 
with internal clock = 8Mhz.
	bcf		TRISIO,5		;Turn the OP into an OP.
	bcf		STATUS,RP0		;Bank 0
	movlw	b'00110100'		;Set Timer 1 with max prescale (1/8) -- this gives us a 
frequency = (2Mhz) / 8 = 250kHz.
	movwf	T1CON			;All the TMR1 stuff is in Bank 0.
							;NOTE: TMR1 isn't on yet until we tell it to be.

	clrf	CMCON1			;The comparator won't start unless it's unsynced with TMR1.
							;The CCP stuff is in bank 0 also.
	movlw	b'00001010'		;Set the CCP to compare mode, and have it generate a 
software interrupt when the match happens.
	movwf	CCP1CON			;The TMR1 registers should be unaffected.
							;You know, you could use this to do A/D timing in the background 
elsewhere...

	bsf		GPIO,2		;turn on the LED.

	bcf		RESET_PROGRAM_FLAG

	clrf	IIRL
	clrf	IIRH
	clrf	timer_start_valueL
	clrf	timer_start_valueH
	clrf	times_through_loop
	bcf		TIMER_FLIPPED_FLAG
	clrf	current_timer_readingL
	clrf	current_timer_readingH
	clrf	tempL
	clrf	tempH
	clrf	temp1L
	clrf	temp1H
	movlw	STEP_TIME_CONSTANT
	movwf	step_timer_counter
	bcf		PIR1,CMIF

;Now that the timers and interrupts are reset, wait for the first comparator 
flip.  When it happens, start your
;routines and start TMR1.  Then do the real loop.
;This'll make sure the first reading into the IIR isn't wildly off.  
Hopefully.

	btfss	PIR1,CMIF			;The Peripheral Interrupt Register and comparator 
interrupt flag are in Bank0.
	goto	$-1

	; If here we know that the comparator has flipped.  Start TMR1.
	bcf		PIR1,CMIF			;Clear the comparator interrupt flag.
	bsf		T1CON,TMR1ON		;Start the timer.
	clrf	TMR0
	bcf		INTCON,T0IF

first_note

	btfss	INTCON,T0IF
	goto	first_note_TMR0_handling_done
	bcf		INTCON,T0IF
	clrz
	decfsz	step_timer_counter
	goto	first_note_TMR0_handling_done
	goto	first_note_done
	; If we've gotten here then TMR0 has rolled STEP_TIME_CONSTANT times. We're 
done with the first note.
	; Is there no decfsnz command?

first_note_TMR0_handling_done
;Not time to go to the next note.  Now what's up with the comparator?

	btfss	PIR1,CMIF			;The Peripheral Interrupt Register and comparator 
interrupt flag are in Bank0.
	goto	first_note

	; If here we know the comparator has flipped again.  Stop TMR1, get the 
value, and restart the timer.

	bcf		T1CON,TMR1ON		;Stop the timer.
	bcf		PIR1,CMIF			;Clear the Comparator interrupt flag.
	movf	TMR1L,W
	movwf	current_timer_readingL
	movf	TMR1H,W
	movwf	current_timer_readingH		;Move the timer value into the fiddle-ready 
registers.
	clrf	TMR1L
	clrf	TMR1H
	bsf		T1CON,TMR1ON			;Start that shit again.

	;Now that we've got the info, make it into the average.

	call	doIIR
	goto	first_note
	;Once we've gotten the  IIR updated, go back to the start of the loop.

first_note_done

	movf	IIRL,W		;Load the compare registers.
	movwf	CCPR1L
	movf	IIRH,W
	movwf	CCPR1H

						;Clear timers, clear their interrupts.  Clear CCP interrupt.  Reload 
the step counter with the constant.
						; ? Start the  timers again?
	bcf		PIR1,CCP1IF
	bcf		T1CON,TMR1ON
	clrf	TMR1L
	clrf	TMR1H
	bcf		INTCON,T0IF
	movlw	STEP_TIME_CONSTANT
	movwf	step_timer_counter
	bcf		PIR1,CMIF
	clrf	TMR0
	bsf		T1CON,TMR1ON
;--------
n_note
;All the tough shit happens here.
;See the notes that start this program.
;The first section is just like note 1 and checks to see if TMR0 flipped.

	btfss	SWITCH_INPUT
	goto	un_pressed_d

	call 	debounce
	call	on_off_switch_handler
	goto	switch_hooey_finished_d

un_pressed_d

	movlw	DEBOUNCE_TIME
	movwf	debounce_counter
	bcf		ALREADY_PRESSED
	clrf	switch_held_counter


switch_hooey_finished_d

	btfss	OUTPUT_STATE_FLAG		;If the thing is supposed to be inactive, go to 
another loop which just turns off the OP, diddles the LED and waits on 
switches.
	call	bide_your_time
	btfsc	RESET_PROGRAM_FLAG		;If we return from a bide_your_time call to this 
point, this flag tells us to re-init everything.
	goto	init_stepped_tone
	btfsc	CHANGE_PROGRAM_FLAG		;If the Change flag got tripped, break out of 
this program to the next_program section.
	goto	next_program

	btfss	INTCON,T0IF				;And the business starts here.
	goto	n_note_TMR0_handling_done
	bcf		INTCON,T0IF
	clrz
	decfsz	step_timer_counter
	goto	n_note_TMR0_handling_done
	goto	n_note_done

n_note_TMR0_handling_done
; You need to check for comparator flips and CCP matches.

	btfsc	PIR1,CCP1IF
	call	handle_CCP_match
	btfsc	PIR1,CMIF
	call	handle_comparator_flip
	goto	n_note

handle_CCP_match
;Here we need to flip the OP bit and reset TMR1.
;Check flipped flag, if it's clear, set it.
;If set, increment times_through_loop.
;Use comparatorReg to keep track of the OP since it doesn't get used 
anywhere else in this program.

	comf	comparatorReg
	btfss	comparatorReg,0
	bcf		OUTPUT_PIN
	btfsc	comparatorReg,0
	bsf		OUTPUT_PIN
	btfsc	TIMER_FLIPPED_FLAG
	goto	increment_times_through_loop
	bsf		TIMER_FLIPPED_FLAG
	return
increment_times_through_loop
	incf	times_through_loop
	return

handle_comparator_flip
; This is the beating heart of the confusing assembly, as undertaken at 2am.
; Get the TMR1L first and hope it hasn't incremented since the comparator 
flipped.  If it has, no big deal, get it anyway.
; Get the other timer value, too (put in current_timer_reading)
; Check the TIMER_FLIPPED_FLAG.  If it hasn't flipped, handle this case by 
saying:
; (temp = current - timer_start_value) and adjust this process for 16 bits.
; If the TIMER_FLIPPED_FLAG has flipped, then:
; (times_through_loop * (CCP_value)) + (CCP_value - timer_start_value) + 
(current_timer_reading)
; so xor times through loop with zero.  If the Z flag is set, go on to 
subtraction.  Else:
; temp = temp + CCP value.  decf times through loop.  XOR check again.
; Once temp accounts for times through loop, then subwf CCP and timer start. 
  Add it to temp.
; Add current timer reading to temp.
; The math is done.  Now:
; Make current timer reading into timer_start_value.
; Make temp into currentTimerReading.
; Do the IIR.
; Reset the comparator flag.
; Clear times_through_loop and TIMER_FLIPPED_FLAG.
; Return to the main loop.

	movf	TMR1L,W
	movwf	current_timer_readingL
	movf	TMR1H,W
	movwf	current_timer_readingH
	btfsc	TIMER_FLIPPED_FLAG
	goto	handle_timer_flipped_math
timer_not_flipped_math
	;16 bit subtract is like:
	setc
	movf	timer_start_valueL,W
	subwf	current_timer_readingL,W
	movwf	tempL
	btfss	STATUS,C
	incf	timer_start_valueH
	movf	timer_start_valueH,W
	subwf	current_timer_readingH,W
	movwf	tempH
	goto	mathDone

handle_timer_flipped_math
	;Are the times through loop == zero?
	clrw
	clrz
	xorwf	times_through_loop,W
	skpnz
	goto	CCP_minus_timer_start_value

	;If not, add the CCP registers to the temp registers and dec times through 
loop.  When you get to zero, you're done.
	clrc
	movf	CCPR1L,W
	addwf	tempL,F
	btfsc	STATUS,C
	incf	tempH
	movf	CCPR1H,W
	addwf	tempH,F
	decf	times_through_loop
	goto	handle_timer_flipped_math

CCP_minus_timer_start_value
;now take the (CCP value) - (timer start value) and call that temp1.

	setc
	movf	timer_start_valueL,W
	subwf	CCPR1L,W
	movwf	temp1L
	btfss	STATUS,C
	incf	timer_start_valueH
	movf	timer_start_valueH,W
	subwf	CCPR1H,W
	movwf	temp1H

;add temp1 to temp.

	clrc
	movf	temp1L,W
	addwf	tempL,F
	btfsc	STATUS,C
	incf	tempH
	movf	temp1H,W
	addwf	tempH,F

;the last thing we do is add the currentTimerReading to Temp.

	clrc
	movf	current_timer_readingL,W
	addwf	tempL,F
	btfsc	STATUS,C
	incf	tempH
	movf	current_timer_readingH,W
	addwf	tempH,F

mathDone
;Phew.  Bet that section will give trouble at runtime.
;Finish up by calling the current read the start for the next read, then the 
temp to the current.
;Do the IIR.
; Reset the comparator flag.
; Clear times_through_loop and TIMER_FLIPPED_FLAG.
; Return to the main loop.

	movf	current_timer_readingL,W
	movwf	timer_start_valueL
	movf	current_timer_readingH,W
	movwf	timer_start_valueH
	movf	tempL,W
	movwf	current_timer_readingL
	movf	tempH,W
	movwf	current_timer_readingH
	call	doIIR
	bcf		PIR1,CMIF
	clrf	times_through_loop
	bcf		TIMER_FLIPPED_FLAG
	return

n_note_done
;make sure to reinitialize the start value as well as loop counters.

	movf	IIRL,W		;Load the compare registers.
	movwf	CCPR1L
	movf	IIRH,W
	movwf	CCPR1H

	clrf	timer_start_valueL
	clrf	timer_start_valueH
	clrf	times_through_loop
	bcf		TIMER_FLIPPED_FLAG

						;Clear timers, clear their interrupts.  Clear CCP interrupt.  Reload 
the step counter with the constant.
	bcf		PIR1,CCP1IF
	bcf		T1CON,TMR1ON
	clrf	TMR1L
	clrf	TMR1H
	bcf		INTCON,T0IF
	movlw	STEP_TIME_CONSTANT
	movwf	step_timer_counter
	bcf		PIR1,CMIF
	clrf	TMR0
	bsf		T1CON,TMR1ON
	goto	n_note

;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
;Subroutines:
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------
doIIR
;Performs an really simple IIR filtering (averaging) algorithm of the form:
;	IIRvalue = (IIRvalue + currentSample) / 2
;All this shit is 16-bit.
;Below is the add:
	clrc
	movf	current_timer_readingL,W
	addwf	IIRL,F
	btfsc	STATUS,C
	incf	IIRH
	movf	current_timer_readingH,W
	addwf	IIRH,F
;At this point IIR has been replaced with the (current reading + the old 
average).  current_timer_readings are unchanged.
;Roll the IIR registers right to divide by two.
	clrc
	rrf		IIRH
	rrf		IIRL
;Simplicity itself.
	return

;--------------------------------
random32
;This is a 32-bit Galois LFSR (Linear Feedback Shift Register)
;which seems like it ought to give a nice long random sequence.
;Probably the "randomness" is less important than having it run fast.
;This must be seeded with non-zero values.
;(7 or 13 tcy)

	bcf		STATUS,C
   	rrf		randomval4,F
	rrf		randomval3,F
	rrf		randomval2,F
	rrf		randomval1,F
	btfss		STATUS,C
	return
	movlw		0xA6
	xorwf		randomval4
	xorwf		randomval3
	xorwf		randomval2
	xorwf		randomval1
	return

;--------------------------------
readprom
;This routine reads a byte in from eeprom address 0 to the program select 
register.

	bsf		STATUS,RP0			;Bank 1.
	clrf	EEADR				;Sets the eeprom address to 00h.
	bsf		EECON1,RD			;Execute the read.
	movf	EEDAT,W				;Put the data into the W register.
	bcf		STATUS,RP0			;Go back to bank 0.
	movwf	program_selector		;Move the data to the program selector register.
	return

writeprom
;This routine writes a byte from the W register into address 0 in eeprom.
;Note: this routine will just kick it here until the write is finished, so 
the program will
;hang for up to 6ms.  This will probably fuck up any timing specific stuff 
going on (sampling, software PWM
;cycle counting) so be aware of that when you call this.

	bsf		STATUS,RP0			;Bank 1.
	clrf	EEADR				;eeprom addy to 00h.
	movwf	EEDAT				;Put that data up in there.
	bsf 	EECON1,WREN			;Set the Write Enable bit.
	movlw	0x55				;Microchip Magic Unlocking Spell.
	movwf	EECON2				; " "
	movlw	0xAA	 			; " "
	movwf	EECON2				; " "
	bsf		EECON1,WR			;Start writing.  Bring a book...
	btfsc	EECON1,WR			;Keep testing to see if the write is done.
	goto	$-1					;If it isn't, keep testing.
	bcf		EECON1,WREN			;Once it is, make the eeprom safe again.
	bcf		STATUS,RP0			;Go home to bank 0 where simple programmers feel safe.
	return

;-------------------------
debounce
;This routine debounces the one switch.

;NOTE:  I changed this so the code only gets here if a switch is touched.

;If the switch reads pressed (high) decrement a counter (debounce).  If the 
counter gets to
;xero, set a flag (VALID).  Clear this flag elsewhere after you've done 
whatever it is you wanted to happen when the button
;got pressed.
;If the switch ever reads low, reset that counter (and make ready a new 
VALID reading by clearing the ALREADY PRESSED flag).
;The VALID flag only gets set once per switch hit even if the switch is 
held, thanks to the ALREADY PRESSED flag.
;If the switch is held for a very long time (ie, the debounce counter rolls 
over HOLD_TIME times)
;do something I haven't figured out yet.
;(15, 18 or 21 tcy)

	movlw	HOLD_TIME
	clrz
	xorwf	switch_held_counter,W
	skpnz
	goto	switch_held

	clrz
	decf	debounce_counter
	skpz
	return

	incf	switch_held_counter
	btfsc	ALREADY_PRESSED
	return
	bsf		ALREADY_PRESSED
	bsf		VALID_SWITCH
	return

switch_held

	bsf		CHANGE_PROGRAM_FLAG
	bcf		ALREADY_PRESSED					;Is this making trouble?
	bcf		VALID_SWITCH					;Ditto?
	;Handle changing programs in each loop, because a goto at this point would 
pre-empt the return from
	;debounce and put something on the stack that would never get popped back 
off.
	clrf	switch_held_counter		;reset your counters.
	movlw	DEBOUNCE_TIME
	movwf	debounce_counter

	;hang up here until genius lets off the switch.

	btfsc	SWITCH_INPUT
	goto	$-1

	bsf		OUTPUT_STATE_FLAG		;Cheat these guys such that the only flag to get 
hit is the
	bcf		RESET_PROGRAM_FLAG		;Next program flag.

	return

;-----------------------------------------------------------------------------
on_off_switch_handler
;This routine uses switch information to toggle the output -- get this --
;on and off.  Revolutionary!
;(2 or 5 tcy)

	btfss	VALID_SWITCH
	return
	bcf		VALID_SWITCH
	comf	output_state
	return

;-----------------------------------------------------------------------------
init_PWM
;Here the PWM stuff is all initialized.  The TMR2 shit is set up and the 
CCP1.
;This will turn the LED on and (almost) all the way up.
;To vary the brightness over an 8-bit range, change the value in CCPR1L.  
The two LSBs are in CCP1CON
;and I'm not worrying about them here.

	bcf		STATUS,RP0			;Bank 0
	movlw	b'00000100'			;TMR2 on, scaling to 1:1.
	movwf	T2CON
	movlw	b'00001100'			;CCP set to PWM.
	movwf	CCP1CON
	movlw	b'11111111'			;Set the Duty cycle to max.
	movwf	CCPR1L
	bsf		STATUS,RP1			;bank1
	movlw	b'11111111'			;Set the period to max, to make the numbers easier.
	movwf	PR2
	bcf		STATUS,RP0			;bank0
	return

;-----------------------------------------------------------------------------
;Here's the surrogate loop that happens when the device is in the off state.
;In the biding time loop, make sure to get the oscillator to 8Mhz.
;Reset the CCP to PWM mode.
;Make a RESET_PROGRAM_FLAG which is set when the program gets to bide your 
time, so that it can re-initialize when it returns to whichever program it 
was in.
;
;Remember to re-init any A/D weirdness in programs 0 and 1, since it gets 
changed in program 3.
;So change OSCON, ANSEL, CCP1CON back to normal.


bide_your_time

	bsf		STATUS,RP0		;Bank 1
	bsf		TRISIO,5		;Turn the OP into a high impedance input.
	movlw	b'01110111'		;Sets the system clock to the internal oscillator and
							;ratchets it up to 8MHz.
	movwf	OSCCON			;Osccon is in bank 1.
	movlw	b'01011000'
	movwf	ANSEL			;AN3 to analog input, conversion clock = Fosc / 16.
	bcf		STATUS,RP0		;Bank 0

	movlw	b'00001100'		;CCP set to PWM.
	movwf	CCP1CON

	bsf		RESET_PROGRAM_FLAG	;This is checked after the return from 
bide_your_time in any given program.  If it's set, the program vectors back 
to it's setup routine.

	btfss	SWITCH_INPUT
	goto	un_pressed2

	call 	debounce
	call	on_off_switch_handler
	goto	switch_hooey_finished2

un_pressed2
	movlw	DEBOUNCE_TIME
	movwf	debounce_counter
	bcf		ALREADY_PRESSED
	clrf	switch_held_counter

switch_hooey_finished2
;Check the OP state to return to the program.
;Do the LED.

	btfsc	OUTPUT_STATE_FLAG
	return
	btfsc	CHANGE_PROGRAM_FLAG		;If we get a Change flag, return to the main 
program so we can exit it to the program changer.
	return

	nop		;Here are 31 nops to make this biding loop the same length as the "on" 
loop, to keep the debouncing the same.
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	clrz
	incf	LEDchase		;Every time this counter passes through zero, bump the PWM 
up by one.
	skpnz
	incf	CCPR1L
	goto	bide_your_time

;----------------------------
debug
;This just blinks and LED forever if the program gets to this point.

	bcf		STATUS,RP0		;Bank0
	movlw	b'00001010'		;Set the CCP to compare mode, and have it generate a 
software interrupt when the match happens.
	movwf	CCP1CON			;The TMR1 registers should be unaffected.
							;You know, you could use this to do A/D timing in the background 
elsewhere...

	bsf		GPIO,2			;turn on the LED.
	btfss	INTCON,T0IF
	goto	$-1
	bcf		INTCON,T0IF
	clrz
	incf	debugCounter
	skpz
	goto	$-6
	bcf		GPIO,2
	btfss	INTCON,T0IF
	goto	$-1
	bcf		INTCON,T0IF
	clrz
	incf	debugCounter
	skpz
	goto	$-6
	goto	debug
;-------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------

	end


