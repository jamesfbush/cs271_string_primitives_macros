; TITLE Project 6 - Designing low-level I/O procedures  (Proj6.asm)

; Author:					James Bush
; Last Modified:	2021-03-16
; Course/section:	CS271 Section 400
; Project Number:	6
; Due Date:				3/14/21
; Description:		Program will implement and test two macros for string processing 
;			and two procedures for signed integers which use string primitive
;			instructions. Test program will get 10 valid integers from user,
;			store the inputs in an array, display the integers, their sum,
;			and their average.

INCLUDE Irvine32.inc


;----------------------------------------------------------------------
; MACROS
;----------------------------------------------------------------------
; Name: mGetString
; Display prompt, get user input of number string.
; Preconditions: promptAddr and count set to address, size of string
; Receives: 	promptAddr = prompt string address
;		count = size of string
; Returns:	userInput = string taken from user
;		bytesRead = byte count of string taken
;----------------------------------------------------------------------

mGetString MACRO promptAddr:REQ, userInput:REQ, count:REQ, bytesRead:REQ

	; Display prompt (input parameter, by reference) with WriteString

	PUSH EDX
	MOV EDX, promptAddr
	CALL WriteString
	POP EDX

	; Preserve registers

	PUSH EAX
	PUSH ECX
	PUSH EDX

	; Get user input with ReadString

	MOV	EDX, userInput		; userInput - point to the buffer (output, by reference)
	MOV	ECX, count		; count - specify max characters (input, by value)
	CALL ReadString			; get string from input
	MOV [bytesRead], EAX		; bytesRead - number of characters (output parameter, by reference)

	; Restore registers

	POP EDX
	POP ECX
	POP EDX

ENDM

;----------------------------------------------------------------------
; Name: mDisplayString
; Print the string which is stored in a specified memory location.
; Preconditions:	String array address passed by stringAddr
; Receives:		stringAddr = string address
; Returns:		None
;----------------------------------------------------------------------
mDisplayString MACRO stringAddr:REQ

	PUSH EDX
	MOV EDX, stringAddr
	CALL WriteString
	POP EDX

ENDM

;----------------------------------------------------------------------
; Constants

NUM_HI_LMT =	2147483647	; Upper bound of SDWORD
NUM_LO_LMT =	-2147483648	; Lower bound of SDWORD
ARRAYSIZE =		10	; Number of array elements requested

;----------------------------------------------------------------------
.data

intro1		BYTE	"PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures",13,10,
			"Written by: James Bush.",13,10,0
intro2		BYTE	"Please provide 10 signed decimal integers.",13,10,
			"Each number needs to be small enough to fit inside a 32 bit register. After you have",13,10,
			"finished inputting the raw numbers I will display a list of the integers, their sum,",13,10,
			"and their average value. ",13,10,0
inputPrompt	BYTE	"Please enter a signed number:",32,0
inputError1	BYTE	"ERROR: You did not enter a signed number or your number was too big.",13,10,0
inputError2	BYTE	"Please try again: ",0
arrayTitle	BYTE	13,10,"You entered the following numbers:",13,10,0
sumTitle	BYTE	13,10,13,10,"The sum of these numbers is: ",0
averageTitle	BYTE	13,10,13,10,"The rounded average is: ",0
commaSpc	BYTE	44,32,0
farewell	BYTE	13,10,"Thanks for playing!",13,10,0
buffer		BYTE 21 DUP(0)	; input buffer for ReadString take user input
byteCount	DWORD ?	; holds counter

numConverted	SDWORD		0		; Set by ASCII-SWORD conversions made by ReadVal proc
charConverted	SDWORD		0		; Set by SDWORD-ASCII conversions made by WriteVal proc
testArray	SDWORD	ARRAYSIZE DUP(?)	; Array for testing conversions, filled in Main proc
testArraySum	SDWORD		0
testArrayAvg	SDWORD		0



.code

;----------------------------------------------------------------------
; MAIN PROCEDURE
;----------------------------------------------------------------------

main PROC

; Greeting title, author, and program description/instructions

_Introduction:

	mDisplayString OFFSET intro1

; Set up stack to obtain 10 signed values from user

_SetupToGetArray:
	MOV EDI, OFFSET testArray		; Reference blank array
	PUSH ECX				; Save registers
	PUSH EAX
	MOV ECX, ARRAYSIZE			; Number of values to obtain

	PUSH OFFSET numConverted		; [EBP + 36]
	PUSH OFFSET	inputError1		; [EBP + 32]
	PUSH OFFSET byteCount			; [EBP + 28]
	PUSH OFFSET inputError2			; [EBP + 24]
	PUSH OFFSET byteCount			; [EBP + 20]
	PUSH SIZEOF buffer			; [EBP + 16]
	PUSH OFFSET buffer			; [EBP + 12]
	PUSH OFFSET inputPrompt			; [EBP + 8]

; Obtain 10 SDWORD digits from user

_GetArray:
	CALL ReadVal				; Get user input, validate
	MOV EAX, numConverted			; Value obtained from user
	MOV [EDI], EAX				; Place user value into array
	MOV numConverted, 0
	ADD EDI, 4
	LOOP _GetArray
	POP EAX					; Restore registers
	POP ECX

; Set up to calculate sum of array values for display

_SetupSumTestArray:				; Set up registers
	PUSH ESI
	PUSH ECX
	PUSH EAX
	MOV ESI, OFFSET testArray		; Point to beginning of array
	MOV ECX, LENGTHOF testArray

; Add each array value to obtain sum

_SumTestArray:
	MOV EAX, [ESI]
	ADD testArraySum, EAX
	ADD ESI, 4
	LOOP _SumTestArray
	POP EAX					; Restore used registers
	POP ECX
	POP ESI

; Set up to calculate average of array values for display

_SetupAvgTestArray:
	PUSH EAX
	PUSH EBX

; Perform mean/average calculation on array

_AvgTestArray:
	MOV EBX, LENGTHOF testArray
	MOV EAX, testArraySum
	CDQ
	IDIV EBX
	MOV testArrayAvg, EAX

; Show array of entered numbers

_PrintTestArrayTitle:

	mDisplayString OFFSET arrayTitle

; Set up to print array values

_PrintTestArraySetup:
	MOV numConverted, 0
	PUSH ESI
	PUSH ECX

	MOV ECX, LENGTHOF testArray
	MOV ESI, OFFSET testArray

; Print array values

_PrintArray:

	MOV EAX, [ESI]
	PUSH OFFSET numConverted			; Will be [EBP + 12]
	PUSH EAX					; Will be [EBP + 8]

	CALL WriteVal					; This should take the SDWORD, print it
	mDisplayString OFFSET charConverted

	mDisplayString OFFSET commaSpc

	ADD ESI, 4
	MOV charConverted, 0
	LOOP _PrintArray


_PrintArraySum:

	mDisplayString OFFSET sumTitle

	; Setup
	MOV numConverted, 0
	PUSH ESI
	PUSH ECX

	MOV ECX, LENGTHOF testArraySum
	MOV ESI, OFFSET testArraySum

	MOV EAX, [ESI]
	PUSH OFFSET numConverted				; Will be [EBP + 12]
	PUSH EAX						; Will be [EBP + 8]

	CALL WriteVal						; This should take the SDWORD, print it
	mDisplayString OFFSET charConverted


_PrintArrayAvg:

	mDisplayString OFFSET averageTitle

	; Setup
	MOV numConverted, 0
	PUSH ESI
	PUSH ECX
	MOV ECX, LENGTHOF testArrayAvg
	MOV ESI, OFFSET testArrayAvg
	MOV EAX, [ESI]
	PUSH OFFSET numConverted				; Will be [EBP + 12]
	PUSH EAX						; Will be [EBP + 8]
	CALL WriteVal						; This should take the SDWORD, print it
	mDisplayString OFFSET charConverted


_Farewell:
	mDisplayString OFFSET farewell

; Exit

	Invoke ExitProcess,0					; exit to operating system
main ENDP

;----------------------------------------------------------------------
; ADDITIONAL PROCEDURES
;----------------------------------------------------------------------

;----------------------------------------------------------------------
; Name: ReadVal
; Get user input in form of string of digits, validate each input,
; convert valid string to numeric SDWORD, and store in memory variable.
; Preconditions:
; Postconditions:
; Receives:
; Returns:
;----------------------------------------------------------------------
ReadVal PROC

	; WHAT DO WE NEED TO PUSH PRIOR?

	; Build stack frame

	_BuildWriteValFrame:
		PUSH EBP
		MOV EBP, ESP
		PUSH EDX				; Preserve used registers
		PUSH EAX
		PUSH ESI
		PUSH EBX
		PUSH ECX

	_InitialGetString:

		mGetString [EBP + 8], [EBP + 12], [EBP + 16], OFFSET byteCount 
		JMP _Setup

	_ErrorGetString:
		POP EBX					; For stack alignment
		mGetString [EBP + 24], [EBP + 12], [EBP + 16], OFFSET byteCount	

	; Set pointers to inputted string and output

	_Setup:

		CLD
		PUSH ESI
		MOV ESI, [EBP + 20]
		MOV ECX, [ESI]				; Length of buffer
		POP ESI
		MOV ESI, [EBP + 12]			; Point ESI to beginning of inputted string

	; Begin byte-by-byte comparison for validation / storage

	_ValidateAndStoreLoop:
		LODSB					; Put byte into AL

	_CheckSign:
		CMP ECX, byteCount
		JL _CheckDigit				; If not first char, check if it's a digit

		CMP AL, 43				; 43 is plus sign "+"
		JE	_PosNum

		CMP AL, 45				; 45 is negative sign "-"
		JE _NegNum

		PUSH EBX				; Correct stack alignment
		JMP _CheckDigit				; If first char neither positive or negative, check if it is a digit

	_PosNum:

		MOV EBX, 0				; "+" is positive number
		PUSH EBX				; save EBX for later conversion

		JMP _EndConvertCharVal			; Go to next char

	_NegNum:

		MOV EBX, 1				; "-" sign is negative number
		PUSH EBX				; Preserve numConverted value

		JMP _EndConvertCharVal

	_CheckDigit:

		CMP AL, 48				; Not a digit (< "0")
		JL _InvalidInputError
		CMP AL, 57				; Not a digit (> "9")
		JG _InvalidInputError
		JMP _ConvertCharVal			; Else, is a digit

	_InvalidInputError:

		mDisplayString [EBP + 32]
		MOV numConverted, 0
		JMP _ErrorGetString

	_ConvertCharVal:
		SUB AL, 48				; This provides actual digit
		PUSH EAX				; Save the digit
		PUSH EBX				; Save EBX
		MOV EBX, 10
		MOV EAX, numConverted			
		MUL EBX					; Multiply by ten for each char
		MOV numConverted, EAX			
		POP EBX
		POP EAX
		CMP EBX, 1				; EBX = 1 indiciative of "-" sign, else positive
		JE _SubDig

	_AddDig:
		ADD numConverted, EAX
		JO _InvalidInputError			; > 2147483647 will set OF
		JMP _EndConvertCharVal

	_SubDig:

		SUB numConverted, EAX
		JO _InvalidInputError			; < -2147483648 will set OF

	_EndConvertCharVal:

		LOOP   _ValidateAndStoreLoop

	_EndReadVal:					; Restore registers
		POP EBX
		POP ECX
		POP EBX
		POP ESI
		POP EAX
		POP EDX
		POP EBP
		RET

ReadVal ENDP

;----------------------------------------------------------------------
; Name: WriteVal
; Convert numeric SDWORD value to a string of ASCII digits, print
; ASCII digits to console.
; Preconditions: numConverted set by ReadVal proc, EAX contains SDWORD to write
; Postconditions: None
; Receives:	offset of numConverted = place to write num
;		EAX = SDWORD representation of num to write
; Returns:	charConverted = new ASCII representation of converted number
;----------------------------------------------------------------------
WriteVal PROC

	; Build stack frame

	_BuildWriteValFrame:
		PUSH EBP
		MOV EBP, ESP
		PUSH EDX				; Preserve used registers
		PUSH EBX
		PUSH EAX
		PUSH EDI
		PUSH ECX
		PUSH ESI

	; Set up registers for conversion of numbers to characters

	_SetupNumToChar:
		MOV EDI, [EBP + 12]			; Offset of numConverted
		CLD					; Clear direction flag to increment through EDI with STOSB
		MOV EAX, [EBP + 8]			; SDWORD digit to convert
		MOV ECX, 0

	; For numbers <10 that do not require factoring

	_TenCannotFactor:
		CMP EAX, 10
		JGE _SeqDivide
		CMP EAX, -10
		JL _SeqDivide
		MOV DL, AL
		DEC ECX					; **NOTE problem with single digits?
		JMP _SaveDigit

	; Sequentially divide numbers > 10 or < -10

	_SeqDivide:					; Sequentially divide
		MOV EBX, 10
		MOV EAX, EAX				; EAX holds the value
		CDQ
		IDIV EBX				; EAX holds quotient, remainder to EDX

	; Save each consecutive digit to string

	_SaveDigit:
		PUSH EAX				; Save value
		MOV AL, DL
		CMP AL, 0
		JL _ConvertNegDig

	; Entry point after conversion of negative digits

	_BackFromConvertNegDig:
		ADD AL, 48
		STOSB					; Put the char in the array
		INC ECX
		POP EAX					; Remaining quotient
		CMP EAX, 0				; Any digit < 0 checked to see if it's last
		JL _CheckLastCharNeg
		JMP _CheckLastCharPos

	; Negative digits are converted to absolute value

	_ConvertNegDig:
		PUSH BX
		MOV BX, -1
		IMUL AX, BX
		POP BX
		JMP _BackFromConvertNegDig

	; For negative integers, check last character before print

	_CheckLastCharNeg:
		CMP EAX, -10
		JG _AddMinusSign
		JMP _SeqDivide				; Else keep sequentially dividing by 10

	; For positive integers, check last character before print

	_CheckLastCharPos:
		CMP EAX, 10
		JL _NoMinusSign
		JMP _SeqDivide

	; If a minus sign is required for the number

	_AddMinusSign:
		PUSH BX
		MOV BX, -1
		IMUL AX, BX
		POP BX
		ADD AL, 48
		STOSB					; Add the last digit char
		INC ECX
		INC ECX
		PUSH EBX				; Add minus sign as final char
		MOV EBX, 45
		MOV [EDI], EBX				; This will be first el of EDI
		ADD EDI, 4
		POP EBX
		JMP _ReverseSting			; Jump to final step, reversal

	; Positive numbers receive no "-"

	_NoMinusSign:
		ADD AL, 48
		STOSB
		INC ECX

	; Set up loop counter and indices to reverse string

	_ReverseSting:
	  mov    ESI, OFFSET numConverted
	  add    ESI, ECX
	  dec    ESI
	  mov    EDI, OFFSET charConverted

	; Reverse string

	_revLoop:
		STD
		LODSB
		CLD
		STOSB
		LOOP   _revLoop

	; Conclude procedure, restore registers and return

	_EndWriteVal:
		POP ESI
		POP ECX
		POP EDI
		POP EAX
		POP EBX
		POP EDX
		POP EBP
		RET  8			; 32-bit reg and an address pushed previous

WriteVal ENDP

;----------------------------------------------------------------------
;EXIT PROGRAM
;----------------------------------------------------------------------

END main
