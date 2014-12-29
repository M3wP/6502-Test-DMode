; TEST DECIMAL MODE
;
; COPYRIGHT (C) 2014, DANIEL ENGLAND.  
; ALL RIGHTS RESERVED.  LGPL LICENSE, SEE BELOW.
;
;
;-------------------------------------------------------------------------------
; 
;   This program is free software: you can redistribute it and/or modify it 
;   under the terms of the GNU Lesser General Public License as published by the 
;   Free Software Foundation, either version 3 of the License, or (at your 
;   option) any later version.
;
;   This program is distributed in the hope that it will be useful, but 
;   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
;   or FITNESS FOR A PARTICULAR PURPOSE.  
;
;   See the GNU Lesser General Public License for more details.
;
;   You should have received a copy of the GNU Lesser General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;
;   Additional permission under GNU GPL version 3 section 7
;
;       Exceptions are granted to the standard "Appropriate Legal Notices"
;       requirements in recognition of the constraints of the target platforms.
;
;       The requirements for the display of "Appropriate Legal Notices" shall be
;       deemed sufficiently met if the compiled executable simply contains the
;       copyright information ("COPYRIGHT (C) 2014, DANIEL ENGLAND") in clear
;       and readable text data (ASCII/PETSCII).  Display of this information 
;       according to the original constraints would be appreciated when and if 
;       possible.
;
;       In cases where the warranty cannot be displayed, the requirements shall 
;       be deemed sufficient met if the user is directed by the application to a 
;       source from which this information is available (on a "splash" screen,
;       for example), so long as this source is distributed with the compiled 
;       execuatable (such as an included "readme" text file which includes the 
;       warranty details).
;
;-------------------------------------------------------------------------------
;
;
; TESTS THE COMPLIANCE OF THE 6502 FAMILY PROCESSOR INSTALLED IN THE MACHINE.
; ALLOWS SELECTION OF 6502, 65C02 AND 65816 COMPLIANCE LEVELS.
;
; A STANDARD COMPLIANCE TEST PERFORMS TESTING OF THE CPU UNTIL A FAILURE IS
; DETECTED OR UNTIL ALL CASES ARE COMPLETE.  A SUMMARY IS PRODUCED.
;
; A COMPREHENSIVE TEST TESTS ALL POSSIBLE CASES AND RECORD FAILURE RESULTS TO 
; DISK.  THIS TEST TYPE MAY REQUIRE UP TO 31 DISKS AND 10 HOURS TO RUN.
;
; A SPECIFIC CASE TEST ALLOWS TESTING OF ONE SPECIFIC TEST.  A SUMMARY IS
; PRODUCED.  CURRENTLY UNIMPLEMENTED FOR VIC20.
;
; THE PROGRAM PROMPTS THE USER FOR INFORMATION AS IT IS REQUIRED.
;
; THIS VERSION OF THE PROGRAM IS FOR USE WITH VIC20 COMPUTERS.  IT MUST BE 
; LOADED AT ITS ORIGINAL ADDRESS (NON RELOCATING).  IT WILL WORK WHEN THERE IS 
; NO EXPANSION RAM (OR ONLY THE +3KB EXPANSION) BUT IN THIS CASE, THE "INIT" 
; PROGRAM (SANEMEVIC20 LOADER) MUST BE USED FIRST TO CONFIGURE THE MACHINE AS IT 
; WOULD BE WHEN A RAM EXPANSION CART (8/16/24KB) IS PRESENT.
;
; MODIFICATION FOR OTHER 6502 BASED PLATFORMS SHOULD BE POSSIBLE.
;
; SPECIAL THANKS TO BRUCE CLARK FOR HIS INFORMATIVE WEB PAGE AND THE ORIGINAL
; TEST ROUTINE.
; 
; 
; TO USE:
;
;       EXPANDED VIC20 (+8/16/24KB):
;
;               LOAD "MAIN", 8, 1
;               RUN
;
;
;       STOCK VIC20 OR +3KB ONLY (SANEMEVIC20 LOADER _MUST_ BE RELOCATED UNLESS
;       THE MACHINE IS ENTIRELY STOCK):
;
;               LOAD "INIT", 8
;               RUN
;
;
; NOTE:  UNFORTUNATELY, THIS CODE USES MORE RAM AND IS LARGER THAN I HAD 
;        ORIGINALLY ANTICIPATED/HOPED.  THERE ARE POTENTIAL OPTIMISATIONS BUT
;        I WILL HAVE TO GET TO THAT AT SOME OTHER POINT.  IT ONLY JUST FITS INTO
;        THE AVAILABLE RAM ON A STOCK VIC20.
;
;        I AM USING SELF MODIFING CODE FAR MORE THAN I LIKE.  UNFORTUNATELY,
;        THERE IS NO WAY TO INDEX INDIRECT A WORD MEMORY POINTER...  OR EVEN
;        TO COMBINE ABSOLUTE AND INDIRECT ADDRESSING ON WORD POINTERS...  IS 
;        THAT WHAT I'M LOOKING FOR?
;
;        YOU MAY BE A LITTLE FRUSTRATED WITH THE ASSEMBLY SYNTAX.  I'M 
;        PRESENTLY USING SOME TOOLS I DON'T EVEN WANT TO DISCLOSE 
;        (CBM PRG STUDIO) BECAUSE IT IS AN IDE AND HAS BASIC COMPILATION, 
;        AS WELL.  *TSK*  ITS BITING ME 'CAUSE IT HAS A REALLY FRUSTRATING BUG 
;        HANDLING STRINGS WITH BYTE DIRECTIVES (KEEPS INTERPRETTING THINGS AS 
;        NUMERICS AND EXPRESSIONS) AND SOME OTHER ODDITIES.  IT IS NICE IN THAT 
;        I'M USED TO WORKING WITH IDE ENVIRONMENTS.  IF I HAVE TO DELETE A DAMN
;        "'" OR '"''"'"'" WHEN I'M COMMENTING AGAIN, I'LL...!!!  BLIMEY!
;
;        I NEED TO COMMENT THIS CODE MORE!  *EEP*
;


        INCASM  "platform.asm"


; BOOT STRAP BASIC LOADER
*       =       $1201
        INCBIN  "strap.bin"

BASSTRP JMP     MAIN0


; "ENUMS" FOR BORDER COLOUR STATUS FEEDBACK
CLRNRM  =       $1B             
CLRERR  =       $1A


; "ENUMS" FOR NOTIFICATION MESSAGES
NTFINIT =       $00             
NTFEXEC =       $01
NTFWAIT =       $02
NTFPREP =       $03
NTFABRT =       $04
NTFUNDF =       $05
NTFTSTF =       $06
NTFTSTC =       $07


; "ENUMS" FOR SCREENS
SCNLEVL =       $00             
SCNTYPE =       $01
SCNSPEC =       $02
SCNNOTE =       $03
SCNCOMP =       $04
SCNDISK =       $05
SCNPREP =       $06
SCNSING =       $07
SCNQUIT =       $08
SCNSPLS =       $09
SCNINIT =       $0A
SCNRSLT =       $0B
SCNTERM =       $FF

; MAXIMUM RECORDS TO WRITE TO A DISK
DSKRECC =       $2198


; SPLASH SCREEN STRINGS
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS STRING DATA AS NUMERIC FNS
TSTSPL0 BYTE    $1F,"VER",$3A," 2",$2E,"01",$2D,"SANVC20",$2D,"BETA",$00

; THESE STRINGS MUST NOT BE MODIFIED OR REMOVED (FOR  COPYRIGHT AND OWNERSHIP 
; RIGHTS)
TSTSPL3 BYTE    $90," COPYRIGHT ",$28,"C",$29,$2C," 2014",$0D,$00
TSTSPL2 BYTE    $90,"    DANIEL ENGLAND",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "(" AND "," AS NUMERIC FNS
TSTSPL4 BYTE    $90," ALL RIGHTS RESERVED",$0D,$00

TSTSPL7 BYTE    $1F,"THANKS TO: BRUCE CLARK",$00
TSTSPLA BYTE    $9F,"PLEASE SEE README FOR",$0D,$00
TSTSPLB BYTE    $9F," FURTHER INFORMATION ",$0D,$00


; A BLANK LINE STRING.  OOOH...
TSTBLNK BYTE    $1F," ",$0D,$00


; PROCESSING DATA
TSTSCRN BYTE    $00             
TSTCOMP BYTE    $FF
TSTTYPE BYTE    $FF

; GENERAL SCREEN DISPLAY STRINGS
TSTHDRS BYTE    $93,$9C,"DECIMAL MODE TEST",$0D,$00
TSTHDC0 BYTE    $9F,"6502",$0D,$00
TSTHDC1 BYTE    $9F,"65C02",$0D,$00
TSTHDC2 BYTE    $9F,"65816",$0D,$00
TSTHDT0 BYTE    $9F,"COMPLIANCE",$0D,$00
TSTHDT1 BYTE    $9F,"COMPREHENSIVE",$0D,$00
TSTHDT2 BYTE    $9F,"SPECIFIC CASE",$0D,$00

;TSTHDD0 BYTE    $1F," ",$0D,$00
TSTHDD1 BYTE    $1F,"DONE.",$0D,$00

; NOTIFICATION MESSAGE STRINGS
TSTFNIS BYTE    $1F,"INITIALISING... ",$0D,$00
TSTFNES BYTE    $1F,"EXECUTING... ",$0D,$00
TSTFNWS BYTE    $1F,"WAITING... ",$0D,$00
TSTFNPS BYTE    $1F,"PREPARING... ",$0D,$00
TSTFNQS BYTE    $1F,"USER ABORTED.",$0D,$00
TSTFNUS BYTE    $1F,"? UNIMPLEMENTED FN.",$0D,$00
TSTFNFS BYTE    $1C,"TEST FAILED.",$0D,$00
TSTFNCS BYTE    $1F,"TEST COMPLETE.",$0D,$00

; INPUT KEYS FOR MENUS

KEYOPT1 =       $00
KEYOPT2 =       $01
KEYOPT3 =       $02
KEYOPTQ =       $03
KEYOPTB =       $04
KEYOPTU =       $FF

TSTMNIS BYTE    "123QB",$00

; SCREEN NAVIGATION STRINGS
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "> - " AS NUMERIC FN
TSTPCPS BYTE    $1F,"  <",$9F,"B",$1F,"> ",$2D," BACK",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "> - " AS NUMERIC FN
TSTPCQS BYTE    $1F,"  <",$9F,"Q",$1F,"> ",$2D," QUIT",$0D,$00

; COMPLIANCE LEVEL SCREEN STRINGS
TSTCSHS BYTE    $1F,"LEVEL SELECT: ",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "- 6502" AS NUMERIC
TSTCSO0 BYTE    $1F,"  <",$9F,"1",$1F,"> ",$2D," 6502",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "> - " AS NUMERIC FN
TSTCSO1 BYTE    $1F,"  <",$9F,"2",$1F,"> ",$2D," 65C02",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "- 65816" AS NUMERIC
TSTCSO2 BYTE    $1F,"  <",$9F,"3",$1F,"> ",$2D," 65816",$0D,$00

; TEST TYPE SCREEN STRINGS
TSTTSHS BYTE    $1F,"TYPE SELECT: ",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "> - " AS NUMERIC FN
TSTTSO0 BYTE    $1F,"  <",$9F,"1",$1F,"> ",$2D," COMPLIANCE",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "> - " AS NUMERIC FN
TSTTSO1 BYTE    $1F,"  <",$9F,"2",$1F,"> ",$2D," COMPREHENSIVE",$0D,$00
;FIXME WORK AROUND BUGS, ASSEMBLER INTERPRETS "> - " AS NUMERIC FN
TSTTSO2 BYTE    $1F,"  <",$9F,"3",$1F,"> ",$2D," SPECIFIC CASE",$0D,$00

; NEXT SCREEN REQUIREMENT FOR TYPE
TSTTSIN BYTE    SCNCOMP,SCNNOTE,SCNSPEC

; COMPREHENSIVE NOTICATION SCREEN STRINGS
TSTCHN0 BYTE    $90,"NOTE:  ",$1F,$0D,$00

TSTCHN2 BYTE    "THIS TEST MAY REQUIRE",$00
TSTCHN3 BYTE    " UP TO 31 BLANK DISKS",$0D,$00
TSTCHN4 BYTE    "   AND 10 HOURS TO",$0D,$00
TSTCHN5 BYTE    "      COMPLETE.",$00

;TEST RESULTS DATA STRING
TSTRSDS BYTE    $90
TSTRSD0 BYTE    "00 "  
TSTRSD1 BYTE    "00 "
TSTRSD2 BYTE    "00 "
TSTRSD3 BYTE    "00 ",$0D,$00

; DISK SWAP SCREEN STRINGS
TSTDSKP BYTE    $90,"INSERT DISK #"
TSTDSK0 BYTE    "00",$00

; FILENAME STRINGS
TSTFILS BYTE    "S"
TSTFILW BYTE    "0:TESTDMODE"
TSTFILT BYTE    "00",$2E,"DAT"
TSTFILI BYTE    $2C,"S",$2C,"W"
TSTFILD BYTE    $00


; THESE TABLES USE TOO MUCH MEMORY

; SCREEN HEADER TABLES
TSTHDCL BYTE    <TSTHDC0,<TSTHDC1,<TSTHDC2
TSTHDCH BYTE    >TSTHDC0,>TSTHDC1,>TSTHDC2
TSTHDTL BYTE    <TSTHDT0,<TSTHDT1,<TSTHDT2
TSTHDTH BYTE    >TSTHDT0,>TSTHDT1,>TSTHDT2

; PROGRAM DONE FEEDBACK TABLE
TSTHDLL BYTE    <TSTBLNK,<TSTHDD1,<TSTBLNK
TSTHDLH BYTE    >TSTBLNK,>TSTHDD1,>TSTBLNK

; NOTIFICATION MESSAGE TABLE
TSTNTFL BYTE    <TSTFNIS,<TSTFNES,<TSTFNWS,<TSTFNPS,<TSTFNQS,<TSTFNUS,<TSTFNFS
        BYTE    <TSTFNCS
TSTNTFH BYTE    >TSTFNIS,>TSTFNES,>TSTFNWS,>TSTFNPS,>TSTFNQS,>TSTFNUS,>TSTFNFS
        BYTE    >TSTFNCS

; SPLASH SCREEN TABLE
TSTSPLL BYTE    <TSTSPL0,<TSTBLNK,<TSTSPL2,<TSTSPL3,<TSTSPL4,<TSTBLNK,<TSTBLNK
        BYTE    <TSTSPL7,<TSTBLNK,<TSTBLNK,<TSTSPLA,<TSTSPLB
TSTSPLH BYTE    >TSTSPL0,>TSTBLNK,>TSTSPL2,>TSTSPL3,>TSTSPL4,>TSTBLNK,>TSTBLNK
        BYTE    >TSTSPL7,>TSTBLNK,>TSTBLNK,>TSTSPLA,>TSTSPLB

; LEVEL SCREEN TABLE
TSTCSTL BYTE    <TSTCSHS,<TSTCSO0,<TSTCSO1,<TSTCSO2,<TSTBLNK,<TSTBLNK,<TSTPCQS
TSTCSTH BYTE    >TSTCSHS,>TSTCSO0,>TSTCSO1,>TSTCSO2,>TSTBLNK,>TSTBLNK,>TSTPCQS

; TYPE SCREEN TABLE
TSTTSTL BYTE    <TSTTSHS,<TSTTSO0,<TSTTSO1,<TSTTSO2,<TSTBLNK,<TSTPCPS,<TSTPCQS
TSTTSTH BYTE    >TSTTSHS,>TSTTSO0,>TSTTSO1,>TSTTSO2,>TSTBLNK,>TSTPCPS,>TSTPCQS

; NOTICE SCREEN TABLE
TSTCHNL BYTE    <TSTBLNK,<TSTCHN0,<TSTBLNK,<TSTCHN2,<TSTCHN3,<TSTCHN4,<TSTCHN5
TSTCHNH BYTE    >TSTBLNK,>TSTCHN0,>TSTBLNK,>TSTCHN2,>TSTCHN3,>TSTCHN4,>TSTCHN5


; "ENUMS" FOR SCREEN DISPLAY
DSPSPLS =       $00
DSPLEVL =       $01
DSPTYPE =       $02
DSPNOTE =       $03

; TABLES OF TABLES FOR SCREEN DISPLAY
TSTSCLL BYTE    <TSTSPLL,<TSTCSTL,<TSTTSTL,<TSTCHNL
TSTSCLH BYTE    >TSTSPLL,>TSTCSTL,>TSTTSTL,>TSTCHNL
TSTSCHL BYTE    <TSTSPLH,<TSTCSTH,<TSTTSTH,<TSTCHNH
TSTSCHH BYTE    >TSTSPLH,>TSTCSTH,>TSTTSTH,>TSTCHNH
TSTSCTN BYTE    $0C,$07,$07,$07


; SCREEN HANDLER FUNCTION TABLE
TSTSCJL BYTE    <SHOWSC0,<SHOWSC1,<SHOWSC2,<SHOWSC3,<SHOWSC4,<SHOWSC5,<SHOWSC6
        BYTE    <SHOWSC7,<SHOWSC8,<SHOWSC9,<SHWINIT,<SHOWSCA
TSTSCJH BYTE    >SHOWSC0,>SHOWSC1,>SHOWSC2,>SHOWSC3,>SHOWSC4,>SHOWSC5,>SHOWSC6
        BYTE    >SHOWSC7,>SHOWSC8,>SHOWSC9,>SHWINIT,>SHOWSCA


; BUFFER
TSTBUFA BYTE    0

TSTBFRS BYTE    0
TSTBFNC BYTE    0
TSTBFN2 BYTE    0
TSTBFN1 BYTE    0
TSTBFNS BYTE    0

TSTBFDL BYTE    0
TSTBFDH BYTE    0

TSTBFRL BYTE    0
TSTBFRH BYTE    0


        INCASM  "throb.asm"
        INCASM  "testdmode.asm"


        ZPOPT   OFF

; REAL PROGRAM ENTRY POINT
MAIN0   LDA     #SCNINIT
        STA     TSTSCRN

MAFSC   LDX     TSTSCRN
        CPX     #$FF
        BEQ     MADONE
        
        LDA     TSTSCJL,X               ; COULD OPTIMISE TO USE RTS CALL TRICK
        STA     MAJSR+1                 ; AND SAVE A FEW BYTES?
        LDA     TSTSCJH,X
        STA     MAJSR+2
        
MAJSR   JSR     SHWINIT
        JMP     MAFSC
        
MADONE  LDX     #$00
MATRMI  STX     TSTBUFA
        LDA     TSTHDLL,X
        LDY     TSTHDLH,X
        JSR     BASPST$
        LDX     TSTBUFA
        INX
        CPX     #$03
        BNE     MATRMI

        RTS


; THESE POSITION ROUTINES SAVE ME ONLY 1 BYTE OF MEMORY APPARENTLY BUT THEY ARE
; USEFUL FOR STANDARDISATION PURPOSES (AND I MAY USE THEM MORE IN THE FUTURE
; MEANING MORE MEMORY SAVED)
MPOSNTF CLC
        LDY     #$00                    ; THESE ARE REVERSED IN RESPECT TO THE DOCS.         
        LDX     #$04
        JSR     KNLPLT$
        RTS

;TODO   I MAY NEED TO RECONSIDER THIS ONE BECAUSE IT IS CALLED ON EVERY 
;       ITERATION ON THE TEST ROUTINE CALL...
MPOSINF CLC
        LDY     #$00            
        LDX     #$07
        JSR     KNLPLT$
        RTS


; DISPLAY SCREEN COMMON HEADER INFO
DISPHDR LDA     #<TSTHDRS
        LDY     #>TSTHDRS
        JSR     BASPST$

        LDX     TSTCOMP
        CPX     #$FF
        BEQ     DISPHD
        LDA     TSTHDCL,X
        LDY     TSTHDCH,X
        JSR     BASPST$

        LDX     TSTTYPE
        CPX     #$FF
        BEQ     DISPHD
        LDA     TSTHDTL,X
        LDY     TSTHDTH,X
        JSR     BASPST$

DISPHD  RTS


; DISPLAY A SCREEN'S CONTENTS
; THIS HAS HELPED REDUCE THE MEMORY FOOTPRINT BY QUITE A LARGE AMOUNT.  I CAN
; NOW AFFORD THE SPLASH SCREEN!  *WHOO!*
DISPSCR STA     TSTBUFA
        JSR     DISPHDR

        JSR     MPOSNTF

        LDX     TSTBUFA

        LDA     TSTSCLL,X
        STA     DSPSCL+1     
        LDA     TSTSCLH,X
        STA     DSPSCL+2     

        LDA     TSTSCHL,X
        STA     DSPSCH+1     
        LDA     TSTSCHH,X
        STA     DSPSCH+2     

        LDA     TSTSCTN,X
        STA     DSPSCN+1     
        
        LDX     #$00
DSPSC1  STX     TSTBUFA
DSPSCL  LDA     TSTCSTL,X
DSPSCH  LDY     TSTCSTH,X
        JSR     BASPST$
        LDX     TSTBUFA
        INX
DSPSCN  CPX     #$00
        BNE     DSPSC1

        RTS


; DISPLAY A NOTIFICATION MESSAGE
DISPNTF PHA
        
        JSR     MPOSNTF

        PLA
        TAX
        LDA     TSTNTFL,X
        LDY     TSTNTFH,X
        JSR     BASPST$

        RTS

; THROBBER TYPE TOGGLE ROUTINES.  I USE THEM OFTEN SO THIS MAY SAVE MEMORY BUT
; ITS USEFUL FOR READABILITY AND STANDARDISATION
THRBKON SEC
        LDA     #THRBTK$
        JSR     THRBCF$
        RTS

THRBKOF CLC
        LDA     #THRBTK$
        JSR     THRBCF$
        RTS

THRBBP1 SEC
        LDY     #$00
        LDA     #THRBTB$
        JSR     THRBCF$
        RTS

THRBBP2 SEC
        LDY     #$01
        LDA     #THRBTB$
        JSR     THRBCF$
        RTS

THRBION SEC
        LDA     #THRBTI$
;       LDX     #$0E
        LDY     #$04
        JSR     THRBCF$
        RTS

THRBIOF CLC
        LDA     #THRBTI$
        JSR     THRBCF$
        RTS

THRBPON SEC
        LDA     #THRBTP$
;       LDX     #$0D
        LDY     #$04
        JSR     THRBCF$
        RTS

THRBPOF CLC
        LDA     #THRBTP$
        JSR     THRBCF$
        RTS

THRBWON SEC
        LDA     #THRBTW$
;       LDX     #$0D
        LDY     #$04
        JSR     THRBCF$
        RTS

THRBWOF CLC
        LDA     #THRBTW$
        JSR     THRBCF$
        RTS

THRBGON SEC
        LDA     #THRBTG$
        JSR     THRBCF$
        RTS

THRBGOF CLC
        LDA     #THRBTG$
        JSR     THRBCF$
        RTS


; SIMPLIFYING THE KEYBOARD INPUT HANDLING HAS REDUCED THE MEMORY FOOTPRINT A 
; FAIR AMOUNT AND MADE THE CODE MUCH EASIER TO READ
KEYGETI JSR     KNLGET$
        CMP     #$00
        BEQ     KEYGETI
        STA     TSTBUFA

        LDX     #$00
KYGTI1  LDA     TSTMNIS,X
        CMP     #$00
        BEQ     KYGTI2
        CMP     TSTBUFA
        BEQ     KYGTI3
        INX
        BNE     KYGTI1

KYGTI2  LDX     #KEYOPTU
KYGTI3  RTS


UTIL2HX CMP     #$0A
        BCS     @1
        ADC     #$30
        RTS

@1      ADC     #$36
        RTS

; SETUP THE FILE NAME STRINGS
UTILSFS PHP
        CLD

        LDA     TSTBFDH
        JSR     UTIL2HX
        STA     TSTDSK0
        STA     TSTFILT
        
        LDA     TSTBFDL
        JSR     UTIL2HX
        STA     TSTDSK0+1
        STA     TSTFILT+1

        LDA     #<DSKRECC
        STA     TSTBFRL
        LDA     #>DSKRECC
        STA     TSTBFRH

        PLP
        RTS

; SELECT THE NEXT DISK
UTILSND LDA     #$01
        JSR     KNLCLS$

        LDA     #$0A
        INC     TSTBFDL
        CMP     TSTBFDL
        BNE     UTNDD
        LDX     #$00
        STX     TSTBFDL
        INC     TSTBFDH

UTNDD   JSR     UTILSFS
        RTS

; DECREMENT THE RECORD COUNTER, SETTING CARRY IF RAN OUT
UTILDCR LDA     TSTBFRL
        CMP     #$00
        BNE     UTDRL
        CMP     TSTBFRH
        BNE     UTDRH

        SEC
        BCS     UTDRX

UTDRH   DEC     TSTBFRH
UTDRL   DEC     TSTBFRL
        CLC
        
UTDRX   RTS

; WRITE DATA RECORD TO FILE
UTILWRD SEI
        LDX     #$01
        JSR     KNLCKO$

        LDX     #$00
UTWRD1  LDA     TSTBFRS,X
        JSR     KNLCHO$
        INX
        CPX     #$05
        BNE     UTWRD1

        LDX     #$00
UTWRD2  LDA     AR$,X
        JSR     KNLCHO$
        INX
        CPX     #$0E
        BNE     UTWRD2

        JSR     KNLCLC$
        CLI
        RTS

; CACHE THE SIGNIFICANT TEST DATA TO LOCAL STORAGE (THIS TEST INST CACHE)
UTILLCL LDX     #$03                    ; COPY THIS TEST DATA TO LOCAL BUFFER
UTLCL1  LDA     NC$,X
        STA     TSTBFNC,X
        DEX
        BPL     UTLCL1
        RTS

; IN .A = BYTE TO CONV, OUT .A = LOW CHAR, .Y = HIGH CHAR
UTILCVB 
        RTS

; SCREEN HANDLER FUNCTIONS


; INITIALISATION "SCREEN"
SHWINIT LDA     #$FF
        STA     TSTCOMP
        STA     TSTTYPE

        LDA     #CLRNRM
        STA     VICSCL$

        JSR     DISPHDR

        LDA     #NTFINIT
        JSR     DISPNTF

        LDA     #$00
        STA     TSTBFDH
        LDA     #$01
        STA     TSTBFDL
        JSR     UTILSFS

        JSR     THRBIN$
        JSR     TESTIN$

        JSR     UTILLCL

        LDA     #SCNSPLS
        STA     TSTSCRN
        RTS

; LEVEL SELECT SCREEN
SHOWSC0 LDA     #DSPLEVL
        JSR     DISPSCR

        LDX     #$0E
        JSR     THRBION

SHWS0G  JSR     KEYGETI
        BMI     SHWS0R                  ; KEYGETI LOADING $FF INTO .X WILL     
                                        ; PRODUCE THIS FLAG
        CPX     #KEYOPTQ
        BEQ     SHWS0Q
        
        CPX     #KEYOPTB                ; WE DON'T WANT THEM HERE
        BNE     SHWS0D  

SHWS0R  JSR     THRBBP1
        JMP     SHWS0G
        
SHWS0D  STX     TSTCOMP
        TXA
        JSR     TESTCF$

        LDA     #SCNTYPE
        STA     TSTSCRN
        JMP     SHWS0X

SHWS0Q  LDA     #SCNQUIT
        STA     TSTSCRN

SHWS0X  JSR     THRBIOF
        RTS


; TYPE SELECT SCREEN
SHOWSC1 LDA     #DSPTYPE
        JSR     DISPSCR

        LDX     #$0D
        JSR     THRBION

SHWS1G  JSR     KEYGETI
        BMI     SHWS1R                  ; KEYGETI LOADING $FF INTO .X WILL     
                                        ; PRODUCE THIS FLAG
        CPX     #KEYOPTQ
        BEQ     SHWS1Q

        CPX     #KEYOPTB
        BNE     SHWS1D
        BEQ     SHWS1P

SHWS1R  JSR     THRBBP1
        JMP     SHWS1G

SHWS1P  LDA     #$FF
        STA     TSTCOMP
        LDA     #SCNLEVL
        STA     TSTSCRN
        JMP     SHWS1X
  
SHWS1D  STX     TSTTYPE
        LDA     TSTTSIN,X
        STA     TSTSCRN
        JMP     SHWS1X

SHWS1Q  LDA     #SCNQUIT
        STA     TSTSCRN

SHWS1X  JSR     THRBIOF
        RTS


; SPECIFIC CASE ENTRY SCREEN
SHOWSC2 JSR     DISPHDR

; TODO  IMPLEMENT INPUT

        LDA     #NTFUNDF
        JSR     DISPNTF

        LDA     #SCNTERM
        STA     TSTSCRN
        RTS


; COMPREHENSIVE TEST NOTIFICATION SCREEN
SHOWSC3 LDA     #DSPNOTE
        JSR     DISPSCR

        JSR     THRBKON

SHWS3G  JSR     KEYGETI

        CPX     #KEYOPTQ
        BEQ     SHWS3Q

        CPX     #KEYOPTB
        BEQ     SHWS3P
        BNE     SHWS3D

SHWS3P  LDA     #$FF
        STA     TSTTYPE
        LDA     #SCNTYPE
        STA     TSTSCRN
        JMP     SHWS3X
        
SHWS3Q  LDA     #SCNQUIT
        STA     TSTSCRN
        JMP     SHWS3X

SHWS3D  LDA     #$00
        STA     TSTBUFA

        LDA     #SCNDISK
        STA     TSTSCRN

SHWS3X  JSR     THRBKOF
        RTS


; COMPLIANCE TESTING SCREEN
SHOWSC4 JSR     DISPHDR

        LDA     #NTFEXEC
        JSR     DISPNTF

        LDX     #$0D
        JSR     THRBPON

        LDA     TSTBFNC                 ; INIT GRAPH 
        EOR     #$01
        TAX
        LDY     TSTBFN2
        JSR     THRBGON
        
SHWS4E  JSR     UTILLCL

SHWS42  JSR     TESTMN$                 ; THE ACTUAL TEST DRIVER CALL IS HERE
        
        LDY     TSTBFN2
        LDA     TSTBFN1
        JSR     THRBSG$                 ; UPDATE THE GRAPH

        LDA     RS$
        STA     TSTBFRS

        LDA     #$00
        STA     TSTBUFA

        LDA     TSTTYPE                 ; IT GETS REALLY INEFFICIENT FROM HERE
        CMP     #$01
        BNE     SHWS43

        LDA     RS$
        BEQ     SHWS43

        JSR     UTILWRD
        LDA     TSTBFNS

        JSR     UTILDCR
        BCC     SHWS43

        LDA     #$01
        STA     TSTBUFA

SHWS43  LDA     TC$
        CMP     #$01
        BEQ     SHWS4D

        LDA     TSTTYPE
        CMP     #$01
        BNE     SHWS4T

        LDA     TSTBUFA
        CMP     #$01
        BNE     SHWS44

        JSR     THRBPOF
        JSR     THRBGOF
        LDA     #SCNDISK
        STA     TSTSCRN
        RTS

SHWS4T  LDA     TSTBFRS
        CMP     #$01
        BEQ     SHWS4F

SHWS44  LDA     TSTBFNC                 ; CHECK IF WE NEED A NEW GRAPH
        CMP     NC$
        BEQ     SHWS45

        JSR     THRBGOF

        LDA     NC$                     ; RE-INIT GRAPH 
        EOR     #$01
        TAX
        LDY     N2$
        JSR     THRBGON

SHWS45  JMP     SHWS4E
        
SHWS4F  LDA     #CLRERR                 
        STA     VICSCL$

SHWS4D  LDA     TSTTYPE
        CMP     #$01
        BNE     SHWS4X

        LDA     #$01
        JSR     KNLCLS$

SHWS4X  JSR     THRBPOF
        JSR     THRBGOF
        LDA     #SCNRSLT
        STA     TSTSCRN
        RTS


; DISK SWAP SCREEN
SHOWSC5 JSR     DISPHDR

        LDA     #NTFWAIT
        JSR     DISPNTF

        LDA     TSTBUFA
        CMP     #$01
        BNE     SHWS5C

        JSR     UTILSND

SHWS5C  JSR     THRBBP2
        JSR     THRBKON
        LDX     #$0B
        JSR     THRBION
        
        JSR     MPOSINF

        LDA     #<TSTDSKP
        LDY     #>TSTDSKP
        JSR     BASPST$

        JSR     KEYGETI

        JSR     THRBIOF
        JSR     THRBKOF

        LDA     #SCNPREP
        STA     TSTSCRN
        RTS


; PREPARE DISK SCREEN
SHOWSC6 JSR     DISPHDR

        LDA     #NTFPREP
        JSR     DISPNTF

        LDX     #$0D
        JSR     THRBWON
        
        LDA     #TSTFILI-TSTFILS
        LDX     #<TSTFILS
        LDY     #>TSTFILS
        JSR     KNLSNM$

        LDA     #$0F
        LDX     #$08
        LDY     #$0F
        JSR     KNLSLF$

        JSR     KNLOPN$
        
        LDA     #$0F
        JSR     KNLCLS$

        LDA     #TSTFILD-TSTFILW
        LDX     #<TSTFILW
        LDY     #>TSTFILW
        JSR     KNLSNM$

        LDA     #$01
        LDX     #$08
        LDY     #$01
        JSR     KNLSLF$

        JSR     KNLOPN$

        JSR     THRBWOF

        LDA     #SCNCOMP
        STA     TSTSCRN
        RTS


; SINGLE CASE TESTING SCREEN
SHOWSC7 JSR     DISPHDR
        LDA     #SCNTERM
        STA     TSTSCRN
        RTS

; QUIT SCREEN
SHOWSC8 LDA     #SCNTERM
        STA     TSTCOMP
        STA     TSTTYPE
        STA     TSTSCRN
        
        JSR     DISPHDR

        LDA     #NTFABRT
        JSR     DISPNTF
        RTS

; SPLASH SCREEN
SHOWSC9 LDA     #DSPSPLS
        JSR     DISPSCR

        JSR     THRBKON

        JSR     KEYGETI

        JSR     THRBKOF

        LDA     #SCNLEVL
        STA     TSTSCRN

        RTS

; RESULTS SCREEN
SHOWSCA JSR     DISPHDR

        LDA     TSTTYPE
        CMP     #$01
        BEQ     SHWS6C
        
        LDA     TSTBFRS
        CMP     #$00
        BEQ     SHWS6C

        LDA     #NTFTSTF
        JSR     DISPNTF

;       JSR     MPOSINF

;       LDA     #<TSTRSDS
;       LDY     #>TSTRSDS
;       JSR     BASPST$
        
        JMP     SHWS6D 

SHWS6C  LDA     #NTFTSTC
        JSR     DISPNTF


SHWS6D  LDA     #SCNTERM
        STA     TSTSCRN      
        RTS
