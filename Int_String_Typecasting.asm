TITLE Int-String Typecasting     (Int_String_Typecasting.asm)

; Author: Cale Coffman
; Last Modified: 11/29/2023
; OSU email address: coffmaca@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: Project 6        Due Date: 12/10/2023
; Description: This program prompts the user for 10 integers, validates the input (and re-prompts, if necessary),
;				stores the values in an array, and then displays those values, their sum, and their truncated average.

INCLUDE Irvine32.inc

; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Prompts user to enter a signed integer.
;
; Preconditions: Do not use ECX or EDX as arguments.
;
; Registers Used: EAX, ECX, EDX
;
; Receives:
;	promptString		=	Address of prompt string
;	userString			=	Address where user string to be stored
;	userStringLength	=	Address where length of user string to be stored
;
; Returns:
;	User's input string stored at memory location userString.
;	Length ofuser's input string stored at memory location userStringLength.
; ---------------------------------------------------------------------------------

mGetString MACRO promptString:REQ, userString:REQ, userStringLength
	PUSH	EAX
	PUSH	ECX
	PUSH	EDX
	PUSH	EDI
	MOV		EDX, promptString
	CALL	WriteString
	MOV		EDX, userString
	MOV		ECX, MAX_INPUT_LENGTH
	CALL	ReadString
	MOV		EDI, userStringLength
	MOV		[EDI], EAX
	POP		EDI
	POP		EDX
	POP		ECX
	POP		EAX
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Prints a string.
;
; Registers Used: EDX
;
; Receives:
;	printString		=	Address of string to be printed
; ---------------------------------------------------------------------------------

mDisplayString MACRO printString:REQ
	PUSH	EDX
	MOV		EDX, printString
	CALL	WriteString
	POP		EDX
ENDM

MAX_INPUT_LENGTH = 50

.data

introStr1			BYTE	"Integer-String Typecasting, by Cale Coffman",13,10,13,10,
							"Welcome to Integer-String Typecasting! Today we will ask",13,10,
							"you to provide 10 signed integers, which will be input",13,10,
							"as strings.",13,10,13,10,0
introStr2			BYTE	"We will then convert each string to a signed integer and",13,10,
							"store in memory. The signed integers will then be converted",13,10,
							"back into strings and listed for you, along with the sum and",13,10,
							"truncated average. Note that numbers must be small enough to",13,10,
							"fit within a 32-bit register.",13,10,13,10,0
introStr3			BYTE	"Let's begin!",13,10,13,10,0
commaSpace			BYTE	", ",0
lineBreak			BYTE	13,10,0
goodbyeStr			BYTE	"Thank you for playing!",13,10,0
promptStr			BYTE	"Please enter a signed integer: ",0
errorStr			BYTE	"ERROR: You either did not enter a signed integer, or the value",13,10,
							"of your signed integer was either too high a positive value or",13,10,
							"too low a negative value.",13,10,0
outputIntTitleStr	BYTE	"You entered the following numbers: ",0
outputSumTitleStr	BYTE	"The sum of these numbers is: ",0
outputAvgTitleStr	BYTE	"The truncated average is: ",0
userIntStr			BYTE	MAX_INPUT_LENGTH DUP(0)
userIntStrLen		DWORD	?
userIntNum			SDWORD	?
userIntNumArray		SDWORD	10 DUP(?)
userIntSum			SDWORD	?
userIntAvg			SDWORD	?
outputIntStr		BYTE	MAX_INPUT_LENGTH DUP(0)
outputIntStrTemp	BYTE	MAX_INPUT_LENGTH DUP(0)

.code
main PROC

	; Print program title and introduction
	mDisplayString OFFSET introStr1
	mDisplayString OFFSET introStr2
	MDisplayString OFFSET introStr3

	; ---------------------------------------------------------
	; Call ReadVal 10 times to obtain 10 signed integers from user,
	;	passing the following variables (by reference):
	;	(i)		promptStr (Input Variable)
	;	(ii)	errorStr (Input Variable)
	;	(iii)	userIntStr (Output Variable)
	;	(iv)	userIntStrLen (Output Variable)
	;	(v)		userIntNum (Output Variable)
	; ---------------------------------------------------------
	MOV		ECX, 10
	MOV		EDI, OFFSET userIntNumArray
_InputLoop:
	PUSH	OFFSET promptStr
	PUSH	OFFSET errorStr
	PUSH	OFFSET userIntStr
	PUSH	OFFSET userIntStrLen
	PUSH	OFFSET userIntNum
	CALL	ReadVal
	PUSH	EAX
	MOV		EAX, userIntNum
	MOV		SDWORD PTR [EDI], EAX
	ADD		EDI, 4
	POP		EAX
	LOOP	_InputLoop

	; ---------------------------------------------------------
	; Call WriteVal 10 times to display all 10 user provided signed integers,
	;	passing the following variables:
	;	(i)		userIntNumArray[index] (by value)
	;	(ii)	outputIntStr (by reference)
	;	(iii)	outputIntStrTemp (by reference)
	; ---------------------------------------------------------
	mDisplayString OFFSET outputIntTitleStr
	MOV		ECX, 10
	MOV		ESI, OFFSET userIntNumArray
_OutputLoop:
	PUSH	[ESI]
	PUSH	OFFSET outputIntStr
	PUSH	OFFSET outputIntStrTemp
	CALL	WriteVal
	CMP		ECX, 2
	JB		_NoCommaSpace								; Prints ", " after each number, excep the last
	mDisplayString OFFSET commaSpace
	ADD		ESI, 4
	LOOP	_OutputLoop
_NoCommaSpace:
	mDisplayString OFFSET lineBreak
	ADD		ESI, 4
	LOOP	_OutputLoop

	; ---------------------------------------------------------
	; Calculate sum of user provided signed integers, and call WriteVal to display,
	;	passing the following variables:
	;	(i)		userIntSum (by value)
	;	(ii)	outputIntStr (by reference)
	;	(iii)	outputIntStrTemp (by reference)
	; ---------------------------------------------------------
	MOV		ECX, 10
	MOV		EAX, 0
	MOV		ESI, OFFSET userIntNumArray
_SumLoop:
	ADD		EAX, [ESI]
	ADD		ESI, 4
	LOOP	_SumLoop
	MOV		userIntSum, EAX
	
	; Print title string
	mDisplayString OFFSET outputSumTitleStr

	; Print sum and line break
	PUSH	userIntSum
	PUSH	OFFSET outputIntStr
	PUSH	OFFSET outputIntStrTemp
	CALL	WriteVal
	mDisplayString OFFSET lineBreak

	; ---------------------------------------------------------
	; Calculate average of user provided signed integers, and call WriteVal to display,
	;	passing the following variables:
	;	(i)		userIntAvg (by value)
	;	(ii)	outputIntStr (by reference)
	;	(iii)	outputIntStrTemp (by reference)
	; ---------------------------------------------------------
	; Calculate truncated average
	XOR		EDX, EDX
	MOV		EAX, userIntSum
	MOV		EBX, 10
	CDQ
	IDIV	EBX
	MOV		userIntAvg, EAX

	; Print title string
	mDisplayString OFFSET outputAvgTitleStr

	; Print average and line breaks
	PUSH	userIntAvg
	PUSH	OFFSET outputIntStr
	PUSH	OFFSET outputIntStrTemp
	CALL	WriteVal
	mDisplayString OFFSET lineBreak
	mDisplayString OFFSET lineBreak

	; Print goodbye string
	mDisplayString OFFSET goodbyeStr
	
	Invoke ExitProcess,0	; exit to operating system
main ENDP

; ---------------------------------------------------------------------------------
; Name: ReadVal
;
; Prompts the user for and receives a signed integer string, validates that the input
;	is a signed integer that will fit within a 32-bit register, converts the signed
;	integer string to a signed integer literal, and saves the signed integer literal
;	to memory.
;
; Receives:
;	[EBP+24]	=	Address of prompt string
;	[EBP+20]	=	Address of error string
;	[EBP+16]	=	Address of user input string variable
;	[EBP+12]	=	Address of user input string length variable
;	[EBP+8]		=	Address of converted integer variable
;
; Returns:
;	userIntStr		-	Updated to user's input integer string
;	userIntStrLen	-	Updated to length of input integer string
;	userIntNum		-	Updated to converted integer literal
; ---------------------------------------------------------------------------------

ReadVal PROC USES EAX EBX ECX EDI EDX ESI
	LOCAL		minusSign:DWORD, pointerEDI:DWORD, strLenMinusSym:DWORD

_GetString:
	; Get user string
	mGetString	[EBP+24], [EBP+16], [EBP+12]

	; ---------------------------------------------------------
	; Initialize strLenMinusSym to string length, EDI / pointerEDI to string address,
	;	and minusSign to 0 (i.e., not negative).
	; ---------------------------------------------------------
	MOV			ESI, [EBP+12]
	MOV			ESI, [ESI]
	MOV			strLenMinusSym, ESI									; strLenMinusSym tracks length of string without any leading +/- symbol
	MOV			EDI, [EBP+16]
	MOV			pointerEDI, EDI										; pointerEDI variable used to reference first numerical digit symbol and omit +/- symbol
	MOV			minusSign, 0										; minusSign tracks whether the input value is negative
	
	; ---------------------------------------------------------
	; Check if first character is a plus (+) or minus (-) symbol.
	;	If either, increments pointerEDI, notes minus sign (if applicable)
	;		and moves to the number validation.
	;	If neither, jumps to the number validation without noting minus sign
	;		or incrementing pointerEDI (to test if first character is a valid number).
	; ---------------------------------------------------------

	; Check if first character is a minus (-) symbol.
	MOV			EAX, 0
	MOV			AL, 45											
	CMP			[EDI], AL		
	JE			_ValidateWithLeadMinus

	; Check if first character is a plus (+) symbol.
	MOV			AL, 43
	CMP			[EDI], AL
	JE			_ValidateWithLeadSym

	; ---------------------------------------------------------
	; If first character is neither a minus (-) or plus (+) symbol, jump to number validation
	;	without incrementing, to test if it is a valid number character.
	; ---------------------------------------------------------
	JMP			_NumberValidation

	; ---------------------------------------------------------
	; To account for the presence of a minus (-) symbol, set minusSign (local variable) equal to 1.
	; ---------------------------------------------------------
_ValidateWithLeadMinus:
	MOV			DWORD PTR minusSign, 1
	
	; ---------------------------------------------------------
	; To account for leading plus (+) or minus (-) symbol, increment pointerEDI to next character
	;	in string and decrement strLenMinusSym by 1 before validating remainder of characters.
	; ---------------------------------------------------------
_ValidateWithLeadSym:
	ADD			pointerEDI, 1
	SUB			strLenMinusSym, 1

	; ---------------------------------------------------------
	; Check if characters will create integer value outside the range of a 32-bit register.
	; ---------------------------------------------------------
_NumberValidation:
	; ---------------------------------------------------------
	; Check if number is too large by checking length of string.
	;	If above 10 characters, string is too long, and number will be too large.
	;	If below, number will not be too large.  If equal, need to test
	;	actual value of user integer, so proceed to test below.
	; ---------------------------------------------------------
	CMP			strLenMinusSym, 10
	JA			_ErrorMessage
	JNE			_CharValidation

	; ---------------------------------------------------------
	; Check values of characters in order to see if they exceed boundary integers
	;	(i.e., -2,147,483,648 and 2,147,483,647), starting with the most significant place value.
	;	For each digit, if the user integer digit is above the max digit, print error
	;	and re-prompt.  If below, number is within range.  If equal, need to test the next
	;	most significant place value, etc.
	; ---------------------------------------------------------
	MOV			EDI, pointerEDI
	MOV			AL, 50
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 49
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 52
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 55
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 52
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 56
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 51
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 54
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 52
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	ADD			EDI, 1
	MOV			AL, 56
	CMP			[EDI], AL
	JA			_ErrorMessage
	JB			_CharValidation
	CMP			minusSign, 0					; "56" above uses the "8" test for one's digit, which only works for negative values.
	JE			_ErrorMessage

	; ---------------------------------------------------------
	; Check if characters are all above 47 and below 58 (i.e., are numerical digit symbols).
	;	If not, jump to error message.
	;	If so, jump to string to number conversion.
	; ---------------------------------------------------------
_CharValidation:
	; Check if string characters are below numerical digit symbol range.
	MOV			EDI, pointerEDI
	MOV			ECX, strLenMinusSym
	MOV			AL, 47
_CharValidationLow:
	CMP			[EDI], AL
	JBE			_ErrorMessage
	ADD			EDI, 1
	LOOP		_CharValidationLow

	; Check if string characters are above numerical digit symbol range.
	MOV			EDI, pointerEDI
	MOV			ECX, strLenMinusSym
	MOV			AL, 58
_CharValidationHigh:
	CMP			[EDI], AL
	JAE			_ErrorMessage
	ADD			EDI, 1
	LOOP		_CharValidationHigh

	; If within required range, jump to number conversion.
	JMP			_ConvertToInt


	; ---------------------------------------------------------
	; Display error message and re-prompt user.
	; ---------------------------------------------------------
_ErrorMessage:
	MOV			EDX, [EBP+20]
	CALL		WriteString
	JMP			_GetString

	; ---------------------------------------------------------
	; Initialize ESI, EDI, and ECX for string-to-number conversion.
	;	Convert each numberical digit symbol to its number literal
	;	value and accumulate total of all digits in EDI.
	; ---------------------------------------------------------
_ConvertToInt:
	MOV			ESI, pointerEDI
	MOV			EDI, [EBP+8]
	MOV			SDWORD PTR [EDI], 0
	MOV			ECX, strLenMinusSym					

	; ---------------------------------------------------------
	; AL holds literal value of current numerical digit symbol.
	;	Subtract 48 to convert to number literal.  Multiple value
	;	at EDI by 10 to acount for place value and add AL to EDI
	;	to accumulate, then repeat for all characters.
	; ---------------------------------------------------------
	_ConvertToIntLoop:
		MOV			EBX, 10
		MOV			EAX, [EDI]
		IMUL		EBX
		MOV			EBX, EAX
		XOR			EAX, EAX
		LODSB
		SUB			AL, 48
		ADD			EBX, EAX
		MOV			[EDI], EBX
		LOOP		_ConvertToIntLoop

	; ---------------------------------------------------------
	; Check if minus (-) symbol was present.  If so, convert
	;	positive value in EDI to a negative by subtracting
	;	from 0.
	; ---------------------------------------------------------
	CMP			minusSign, 0
	JE			_Return
	MOV			EAX, [EDI]
	XOR			EBX, EBX
	SUB			EBX, EAX
	MOV			[EDI], EBX
	
_Return:
	RET			16
ReadVal ENDP

; ---------------------------------------------------------------------------------
; Name: WriteVal
;
; Converts a signed integer literal to a string and prints.
;
; Receives:
;	[EBP+16]	=	Value of signed integer literal
;	[EBP+12]	=	Address of output string variable
;	[EBP+8]		=	Address of temporary string variable (used to un-reverse the output string)
;
; Returns:
;	outputIntStr		-	Updated to converted signed integer string
;	outputIntStrTemp	-	Updated to unsigned verion of converted signed integer string
; ---------------------------------------------------------------------------------

WriteVal PROC USES EAX EBX ECX EDX EDI ESI
	LOCAL	isNegative:DWORD, intLength:DWORD

	; Initialize isNegative to 0
	MOV		isNegative, 0

	; ---------------------------------------------------------
	; Initialize source and destination registers and check if
	;	input signed integer is negative.
	; ---------------------------------------------------------
	MOV		ESI, [EBP+16]											; User int value
	MOV		EDI, [EBP+12]											; Address of output string variable
	MOV		ECX, 0													; Counter for number of digits
	CMP		ESI, 0
	JL		_IsNegative

	; ---------------------------------------------------------
	; Convert positive/unsigned integer to string.  Divide integer
	;	by 10. Remainder, which is the least significant digit in the 
	;	integer string, is incremented by 48 to convert to its ASCII value
	;	and is stored in the output string variable. Process
	;	repeated on quotient, which includes only the remaining
	;	digits in the integer, until the quotient equals 0.
	; ---------------------------------------------------------
_ConvertToStrLoop:
	XOR		EDX, EDX
	MOV		EAX, ESI
	MOV		EBX, 10
	IDIV	EBX
	MOV		ESI, EAX
	MOV		AL, DL
	ADD		AL, 48
	STOSB
	INC		ECX
	CMP		ESI, 0
	JNE		_ConvertToStrLoop

	; ---------------------------------------------------------
	; Store length of positive/unsigned integer in intLength.
	;	Check if user integer was negative and jump to respective
	;	reverse string algorithm.  If string is only 1 character
	;	long and no string reversal is required, jumps directly
	;	to adding null terminator and printing.
	; ---------------------------------------------------------
	MOV		intLength, ECX
	CMP		isNegative, 1
	JE		_ReverseCheckNegative
	CMP		intLength, 1
	JNE		_ReverseStringPositive
	JMP		_NullTerminateAndPrint
_ReverseCheckNegative:
	CMP		intLength, 1
	JNE		_ReverseStringNegative
	JMP		_NullTerminateAndPrint

	; ---------------------------------------------------------
	; For negative user integer values, sets isNegative to 1 for
	;	later reference. Then adds an ASCII minus (-) as the first
	;	element of the output string, multiplies the negative integer
	;	by -1 to convert to a positive/unsigned integer, and returns
	;	to convert the positive/unsigned integer to a string.
	; ---------------------------------------------------------
_IsNegative:
	MOV		isNegative, 1
	MOV		BYTE PTR AL, 45
	STOSB
	MOV		EAX, ESI
	XOR		EDX, EDX
	MOV		EBX, -1
	MUL		EBX
	MOV		ESI, EAX
	JMP		_ConvertToStrLoop

	; ---------------------------------------------------------
	; Initializes EDI and ESI for reversal of the integer string.
	;	EDI initialized to address of the final element in the
	;	temporary string array,and ESI initialized to address of
	;	the first element in the output string array. For negative
	;	integers, ESI is incremented by one element to account for the
	;	minus (-) symbol that is already present in the output string.
	; ---------------------------------------------------------
_ReverseStringPositive:
	MOV		ECX, intLength
	SUB		ECX, 1
	MOV		EDI, [EBP+8]
	ADD		EDI, ECX
	MOV		ESI, [EBP+12]
	XOR		EAX, EAX
	MOV		ECX, intLength
	JMP		_ReverseLoop
_ReverseStringNegative:
	MOV		ECX, intLength
	SUB		ECX, 1
	MOV		EDI, [EBP+8]
	ADD		EDI, ECX
	MOV		ESI, [EBP+12]
	INC		ESI
	XOR		EAX, EAX
	MOV		ECX, intLength

	; ---------------------------------------------------------
	; Elements of ESI are copied into EDI, with the first element of
	;	ESI added as the final element of EDI, ESI incremented after
	;	each move, and EDI decremented after each move for the number
	;	of characters stored in intLength.
	; ---------------------------------------------------------
_ReverseLoop:
	CLD
	LODSB
	STD
	STOSB
	LOOP	_ReverseLoop

	; ---------------------------------------------------------
	; Copy the reverse string stored in the temporary string variable
	;	back to the output string variable.  For negative integers,
	;	EDI is incremented by one to account for the minus (-) symbol
	;	that is already present in the output string.
	; ---------------------------------------------------------
	MOV		ESI, [EBP+8]
	MOV		EDI, [EBP+12]
	XOR		EAX, EAX
	MOV		ECX, intLength
	CMP		isNegative, 1
	JNE		_CopyLoop
	ADD		EDI, 1
_CopyLoop:
	CLD
	REP	MOVSB
	
	; ---------------------------------------------------------
	; Add a null terminator (value of 0) after the final element
	;	of the output string.  For negative integers, address is
	;	incremented by one byte to account for additional element--
	;	the minus (-) symbol--present in the output string. Then,
	;	call mDisplayString to print the integer string.
	; ---------------------------------------------------------
_NullTerminateAndPrint:
	MOV		ECX, intLength
	CMP		isNegative, 1
	JNE		_NullTerminateAndPrintPositive
	INC		ECX
_NullTerminateAndPrintPositive:
	MOV		EDI, [EBP+12]
	ADD		EDI, ECX
	MOV		BYTE PTR [EDI], 0
	mDisplayString [EBP+12]

	RET		12
WriteVal ENDP

END main
