; Verify decimal mode behaviour (6502).
;
; Original work 2009, Bruce Clark
; Modifications 2014, Daniel England
;
; PUBLIC DOMAIN.  SEE BELOW.
;
;
;-------------------------------------------------------------------------------
;
;   This is free and unencumbered software released into the public domain.
;
;   Anyone is free to copy, modify, publish, use, compile, sell, or
;   distribute this software, either in source code form or as a compiled
;   binary, for any purpose, commercial or non-commercial, and by any
;   means.
;
;   In jurisdictions that recognize copyright laws, the author or authors
;   of this software dedicate any and all copyright interest in the
;   software to the public domain. We make this dedication for the benefit
;   of the public at large and instead of and in preference to, those of our 
;   own interests, estates and holdings.  We intend this dedication to be an 
;   overt act of relinquishment in perpetuity of all present and future rights 
;   to this software under copyright law.
;   
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;   IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;   OTHER DEALINGS IN THE SOFTWARE.
;   
;-------------------------------------------------------------------------------
;
;
; This is a modification of Bruce's routine to allow for full characterisation 
; via a test harness by driving it through calls to TESTIN$, TESTCF$, TESTI2$ 
; and TESTMN$.
;
; The original code was sourced from: 
;   http://www.6502.org/tutorials/decimal_mode.html
;
; The original routine was developed after discusion which can be viewed at:
;   http://forum.6502.org/viewtopic.php?f=1&t=254
;
;
; Entry points:
;
;   TESTIN$     Call to initialise the testing routines
;   TESTCF$     Load .A with 0-2 to configure the testing routines for matching
;               a particular level of CPU compatibilty (0 - 6502, 1 - 65C02, 
;               2 - 65816)
;   TESTI2$     This routine should be called after TESTIN$ and TESTCF$ when 
;               wanting to perform a specific test where N1 is not 0.
;   TESTMN$     Call (repeatedly) to perform the tests.  It will return after
;               each test is done in order to collect results.  When called 
;               again, it will perform the next test until TC = 1.
;
;
; Variables:
;
;   TC          all tests complete
;   RS          interation return result
;   TC          flags testing complete
;   NS          0 for next call is ADD, 1 for next call is SUB
;   NC          carry testing phase flag
;   N1          are the two numbers to be added or subtracted 
;   N2 
;
;   N1H, N1L, N2H, and N2L are the upper 4 bits and lower 4 bits of N1 and N2
;   DA and DNVZC are the actual accumulator and flag results in decimal mode
;   HA and HNVZC are the accumulator and flag results when N1 and N2 are
;     added or subtracted using binary arithmetic
;   AR, NF, VF, ZF, and CF are the predicted decimal mode accumulator and
;     flag results, calculated using binary arithmetic
;
;
; NOTE:  These tests could take a very long time to process to completion!
;

TC$     BYTE    0
RS$     BYTE    0
NC$     BYTE    0
N2$     BYTE    0
N1$     BYTE    0
NS$     BYTE    0

AR$     BYTE    0
CF$     BYTE    0
DA$     BYTE    0
DNVZC$  BYTE    0
HA$     BYTE    0
HNVZC$  BYTE    0
N1H$    BYTE    0
N1L$    BYTE    0
N2L$    BYTE    0
NF$     BYTE    0
VF$     BYTE    0
ZF$     BYTE    0
N2H$    WORD    0


TSTACL  BYTE    <A6502, <A65C02, <A65816
TSTACH  BYTE    >A6502, >A65C02, >A65816
TSTSCL  BYTE    <S6502, <S65C02, <S65816
TSTSCH  BYTE    >S6502, >S65C02, >S65816


;Initialisation routine.  Call at the start
TESTIN$ LDY     #1    ; initialize Y (used to loop through carry flag values)
        STY     NC$

        LDA     #$00    ; LEVEL
        JSR     TESTCF$

        LDA     #0    ; initialize N1 and N2
        STA     RS$ 
        STA     N1$
        STA     N2$
        STA     NS$
        STA     TC$
        RTS

TESTCF$ CMP     #$04
        BPL     TSTCFX

        TAX

        LDA     TSTACL,X
        STA     TESTAP+1
        LDA     TSTACH,X
        STA     TESTAP+2
        
        LDA     TSTSCL,X
        STA     TESTSP+1
        LDA     TSTSCH,X
        STA     TESTSP+2

TSTCFX  RTS
        

TESTI2$ LDA     N2$          ; N2L = N2 & $0F
        AND     #$0F        ; [1] see text
        STA     N2L$
        LDA     N2$          ; N2H = N2 & $F0
        AND     #$F0        ; [2] see text
        STA     N2H$
        ORA     #$0F        ; N2H+1 = (N2 & $F0) + $0F
        STA     N2H$+1
        RTS


;Test main routine.  
TESTMN$ SEI             ; Disable interrupts (just in case)
        LDA     #1         ; Set state to error (for safety)
        STA     RS$ 
        LDY     NC$

        LDX     NS$          ; Check if its add or subtract this iteration
        BEQ     M0

        JSR     TESTS
        JMP     M3

M0      LDA     N1$          ; We need to figure out which interation of the test
        BEQ     M2          ;     that we need to go to next

M1      JSR     TEST2
        JMP     M3

M2      JSR     TEST1

M3      ;LDA     NC$             ; Check if the testing is complete
        ;BNE     M4
        ;LDA     N1$          
        ;BNE     M4
        ;LDA     N2$          
        ;BNE     M4

        BCC     M4              

        LDA     #$01
        STA     TC$

M4      CLI
        RTS


;       JSR     TESTI2$     ; DON'T USE A SUBROUTINE 'CAUSE ITS SLOWER
TEST1   LDA     N2$          ; N2L = N2 & $0F
        AND     #$0F        ; [1] see text
        STA     N2L$
        LDA     N2$          ; N2H = N2 & $F0
        AND     #$F0        ; [2] see text
        STA     N2H$
        ORA     #$0F        ; N2H+1 = (N2 & $F0) + $0F
        STA     N2H$+1

TEST2   LDA     N1$          ; N1L = N1 & $0F
        AND     #$0F        ; [3] see text
        STA     N1L$
        LDA     N1$          ; N1H = N1 & $F0
        AND     #$F0        ; [4] see text
        STA     N1H$

        LDX     #$01            ; NEXT TEST WILL BE SUB
        STX     NS$

TESTA   JSR     ADD
TESTAP  JSR     A6502
        JSR     COMPARE
        BNE     TESTMD
        LDA     #$00        ; test passed, so store 0 in RS
        STA     RS$
        JMP     TESTMD

TESTS   JSR     SUB
TESTSP  JSR     S6502

        JSR     COMPARE
        BNE     TESTNX
        LDA     #$00       ; test passed, so store 0 in RS
        STA     RS$

TESTNX  LDX     #$00        ; NEXT TEST WILL BE ADD
        STX     NS$

        INC     N1$         ; [5] see text
        BNE     TESTMD       ; loop through all 256 values of N1

        INC     N2$         ; [6] see text
        BNE     TESTMD       ; loop through all 256 values of N2

        CPY     #$00
        BNE     TESTM1

        SEC
        RTS

TESTM1  DEY
        STY     NC$

TESTMD  CLC
        RTS


; Calculate the actual decimal mode accumulator and flags, the accumulator
; and flag results when N1 is added to N2 using binary arithmetic, the
; predicted accumulator result, the predicted carry flag, and the predicted
; V flag
;
ADD     SED       ; decimal mode
        CPY     #1    ; set carry if Y = 1, clear carry if Y = 0
        LDA     N1$
        ADC     N2$
        STA     DA$    ; actual accumulator result in decimal mode
        PHP
        PLA
        STA     DNVZC$ ; actual flags result in decimal mode
        CLD       ; binary mode
        CPY     #1    ; set carry if Y = 1, clear carry if Y = 0
        LDA     N1$
        ADC     N2$
        STA     HA$    ; accumulator result of N1+N2 using binary arithmetic

        PHP
        PLA
        STA     HNVZC$ ; flags result of N1+N2 using binary arithmetic
        CPY     #1
        LDA     N1L$
        ADC     N2L$
        CMP     #$0A
        LDX     #$00
        BCC     A1
        INX
        ADC     #5    ; add 6 (carry is set)
        AND     #$0F
        SEC

A1      ORA     N1H$
;
; if N1L + N2L <  $0A, then add N2 & $F0
; if N1L + N2L >= $0A, then add (N2 & $F0) + $0F + 1 (carry is set)
;
        ADC     N2H$,X
        PHP
        BCS     A2
        CMP     #$A0
        BCC     A3
A2      ADC     #$5F  ; add $60 (carry is set)
        SEC
A3      STA     AR$    ; predicted accumulator result
        PHP
        PLA
        STA     CF$    ; predicted carry result
        PLA
;
; note that all 8 bits of the P register are stored in VF
;
        STA     VF$    ; predicted V flags
        RTS



; Calculate the actual decimal mode accumulator and flags, and the
; accumulator and flag results when N2 is subtracted from N1 using binary
; arithmetic
;
SUB     SED       ; decimal mode
        CPY     #1    ; set carry if Y = 1, clear carry if Y = 0
        LDA     N1$
        SBC     N2$
        STA     DA$    ; actual accumulator result in decimal mode
        PHP
        PLA
        STA     DNVZC$ ; actual flags result in decimal mode
        CLD       ; binary mode
        CPY     #1    ; set carry if Y = 1, clear carry if Y = 0
        LDA     N1$
        SBC     N2$
        STA     HA$    ; accumulator result of N1-N2 using binary arithmetic

        PHP
        PLA
        STA     HNVZC$ ; flags result of N1-N2 using binary arithmetic
        RTS



; Calculate the predicted SBC accumulator result for the 6502 and 65816

;
SUB1    CPY     #1    ; set carry if Y = 1, clear carry if Y = 0
        LDA     N1L$
        SBC     N2L$
        LDX     #0
        BCS     S11
        INX
        SBC     #5    ; subtract 6 (carry is clear)
        AND     #$0F
        CLC

S11     ORA     N1H$
;
; if N1L - N2L >= 0, then subtract N2 & $F0
; if N1L - N2L <  0, then subtract (N2 & $F0) + $0F + 1 (carry is clear)
;
        SBC     N2H$,X
        BCS     S12
        SBC     #$5F  ; subtract $60 (carry is clear)
S12     STA     AR$
        RTS

; Calculate the predicted SBC accumulator result for the 6502 and 65C02

;
SUB2    CPY     #1    ; set carry if Y = 1, clear carry if Y = 0
        LDA     N1L$
        SBC     N2L$
        LDX     #0
        BCS     S21
        INX
        AND     #$0F
        CLC
S21     ORA     N1H$
;
; if N1L - N2L >= 0, then subtract N2 & $F0
; if N1L - N2L <  0, then subtract (N2 & $F0) + $0F + 1 (carry is clear)
;
        SBC     N2H$,X
        BCS     S22
        SBC     #$5F   ; subtract $60 (carry is clear)
S22     CPX     #0
        BEQ     S23
        SBC     #6
S23     STA     AR$     ; predicted accumulator result
        RTS

; Compare accumulator actual results to predicted results
;
; Return:
;   Z flag = 1 (BEQ branch) if same
;   Z flag = 0 (BNE branch) if different
;
COMPARE LDA     DA$
        CMP     AR$
        BNE     C1
        LDA     DNVZC$ ; [7] see text
        EOR     NF$
        AND     #$80  ; mask off N flag
        BNE     C1
        LDA     DNVZC$ ; [8] see text
        EOR     VF$
        AND     #$40  ; mask off V flag
        BNE     C1    ; [9] see text
        LDA     DNVZC$
        EOR     ZF$    ; mask off Z flag
        AND     #2
        BNE     C1    ; [10] see text
        LDA     DNVZC$
        EOR     CF$
        AND     #1    ; mask off C flag
C1      RTS

; These routines store the predicted values for ADC and SBC for the 6502,
; 65C02, and 65816 in AR, CF, NF, VF, and ZF

A6502   LDA     VF$
;
; since all 8 bits of the P register were stored in VF, bit 7 of VF contains
; the N flag for NF
;
        STA     NF$
        LDA     HNVZC$
        STA     ZF$
        RTS

S6502   JSR     SUB1
        LDA     HNVZC$
        STA     NF$
        STA     VF$
        STA     ZF$
        STA     CF$
        RTS

A65C02  LDA     AR$
        PHP
        PLA
        STA     NF$
        STA     ZF$
        RTS

S65C02  JSR     SUB2
        LDA     AR$
        PHP
        PLA
        STA     NF$
        STA     ZF$
        LDA     HNVZC$
        STA     VF$
        STA     CF$
        RTS

A65816  LDA     AR$
        PHP
        PLA
        STA     NF$
        STA     ZF$
        RTS

S65816  JSR     SUB1
        LDA     AR$
        PHP
        PLA
        STA     NF$
        STA     ZF$
        LDA     HNVZC$
        STA     VF$
        STA     CF$
        RTS
