; SIMPLE THROBBER DRIVER
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
; FOR USE WITH APPLICATIONS ON A VIC20.  COULD BE MODIFIED FOR OTHER 6502
; PLATFORMS.
;
; USE TO INSTALL A THROBBER DRIVER IN YOUR APPLICATION.  SUPPORTS SIX DIFFERENT
; THROBBER TYPES (SEE BELOW).  EACH THROBBER TYPE CAN BE ENABLED SIMULTANEOUSLY
; EXCEPT FOR PROCESSING AND THE WAITING/INPUT (ENABLING ONE WILL DISABLE THE 
; OTHERS).  THE BEEP THROBBER (WITH TWO DIFFERENT TONES) AUTOMATICALLY DISABLES 
; ITSELF AFTER A SHORT DURATION.  THE PROCESSING AND WAITING/INPUT THROBBERS CAN 
; BE POSITIONED AT ANY POINT ON THE SCREEN WHEN ENABLED.  THE GRAPH THROBBER HAS
; AN UPDATE ROUTINE.  
;
;
; CALL THRBIN$ TO INSTALL THE IRQ HANDLER.
; CALL THRBCF$ TO CONFIGURE (ENABLE/DISABLE) A THROBBER
; CALL THRBSG$ TO UPDATE THE GRAPH THROBBER PROGRESS
;
;
;
; REQUIRES PRIOR DEFINTION OF THE FOLLOWING VALUES:
;
;       SA_SCN$ - ADDRESS OF SCREEN MEMORY
;       SA_CLM$ - ADDRESS OF COLOUR MEMORY
;
;       VICO2F$ - OSCILLATOR 2 FREQUENCY REGISTER
;       VICO3F$ - OSCILLATOR 3 FREQUENCY REGISTER
;       VICSGV$ - VOLUME CONTROL REGISTER
;
;
;
; NOTE:  UNFORTUNATELY, THIS CODE USES MORE RAM AND IS LARGER THAN I HAD 
;        ORIGINALLY ANTICIPATED/HOPED.  THERE ARE POTENTIAL OPTIMISATIONS BUT
;        I WILL HAVE TO GET TO THAT AT SOME OTHER POINT.
;
;        YOU MAY BE A LITTLE FRUSTRATED WITH THE ASSEMBLY SYNTAX.  I'M PRESENTLY
;        USING SOME SOMEWHAT OBSCURE TOOLS, CBM PRG STUDIO, BECAUSE IT HAS BASIC 
;        COMPILATION, AS WELL.  *TSK*
;


;GLOBAL DEFINES
;-------------------------------------------------------------------------------

; THROBBER TYPES.  THESE ARE THE ACCEPTED "ENUM" VALUES FOR CALLING IN THRBCF$
; .A SHOULD BE SET TO ONE OF THESE VALUES.
;       THRBTP$ - PROCESSING THROBBER
;       THRBTW$ - WAITING THROBBER
;       THRBTK$ - PRESS A KEY THROBBER
;       THRBTB$ - BEEP THROBBER
;       THRBTI$ - INPUT THROBBERS, ALIAS OF THRBTW$ BUT USES DIFFERENT CHARACTER
;       THRBTG$ - GRAPH THROBBER
THRBTP$ =       $01
THRBTW$ =       $02
THRBTK$ =       $03
THRBTB$ =       $04
THRBTG$ =       $05
; ALIASES MUST BE LAST
THRBTI$ =       $06

THRBTTP = THRBTI$ + 1

;PRIVATE DEFINES
;-------------------------------------------------------------------------------

GPHCOF  = 22 * 7
GPHSSA  = SA_SCN$ + GPHCOF
GPHCSA  = SA_CLM$ + GPHCOF

GPHTOF  = GPHCOF + 256
GPHTSA  = SA_SCN$ + GPHTOF
GPHTCA  = SA_CLM$ + GPHTOF
GPHTLN  = $08

GPHFCH  = $A0

; I AUGHT TO MAKE A VIC20 LIBRARY WITH THESE DEFINES.  SO MUCH MUCKING ABOUT 
; GETTING THEM RIGHT.

;PURPLE
GPHCL0  = $06
;BLUE
GPHCL1  = $04
;BLUE
GPHCL2  = $04
;BLACK
GPHCL3  = $00           


;PRIVATE STORAGE
;-------------------------------------------------------------------------------

THRBVC  WORD    0
THRBTY  BYTE    0

THRBPC  BYTE    0
THRBWC  BYTE    0
THRBKC  BYTE    0
THRBBC  BYTE    0

THRBPP  BYTE    0
THRBWP  BYTE    0
THRBKP  BYTE    0
THRBKD  BYTE    0

THRBTF  BYTE    $01, $02, $04, $08, $10
THRBPT  BYTE    $7C, $6C, $7B, $7E
THRBWT  BYTE    $7F, $7F, $FF, $FF
THRBIT  BYTE    $3F, $3F, $BF, $BF
THRBKT  BYTE    $A0,$A0,$A0,$A0,$90,$92,$85,$93,$93,$A0,$A0,$BC,$93,$90,$81,$83
        BYTE    $85,$BE,$A0,$A0,$A0,$A0
THRBKS  BYTE    $00, $06, $04, $03

GRHPBF  BYTE    GPHCL0,GPHCL1,GPHCL2,GPHCL0
;
;PALETTE IDX    .X = 0/1
;
;FG =           TXA
;               ASL
;               INC
;               TAX
;               LDA     GRHPBF,X       
;
;BG =           TXA
;               ASL
;               TAX
;               LDA     GRHPBF,X


;                 0   1   2   3   4   5   6   7   8
;                00  1F  3F  5F  7F  9F  BF  DF  FF
;GRHPCS  BYTE   $20,$65,$74,$75,$61,$F6,$EA,$E7,$A0
GRHPCS  BYTE    $A0,$E5,$F4,$F5,$E1,$76,$6A,$67,$A0
;                 0   1   1   1   1   1   1   1   1             
;                 0   1   1   1   1   1   1   1   1           
;                 0   1   1   1   1   1   1   1   1           
;                 0   1   1   1   1   1   1   1   1           
;                 0   1   1   1   1   1   1   1   1           
;                 0   0   1   0   1   0   1   0   1           
;                 0   0   0   1   1   0   0   1   1           
;                 0   0   0   0   0   1   1   1   1           

;INDEX =        LDY #$00
;               LSR
;               LSR
;               LSR
;               LSR
;               LSR
;               TAX
;               TYA
;               ROL
;               CMP #$01
;               BNE NOINX
;               INX
;         NOINX ...


THRBIL  BYTE    <THRBINP, <THRBINW, <THRBINK, <THRBINB, <THRBING
THRBIH  BYTE    >THRBINP, >THRBINW, >THRBINK, >THRBINB, >THRBING
THRBUL  BYTE    <THRBUNP, <THRBUNW, <THRBUNK, <THRBUNB, <THRBUNG
THRBUH  BYTE    >THRBUNP, >THRBUNW, >THRBUNK, >THRBUNB, >THRBUNG

THRBWN  BYTE    0
THRBCA  BYTE    0
THRBCX  BYTE    0
THRBCY  BYTE    0

GRPPAL  BYTE    0
GRPHNL  BYTE    0
GRPHNH  BYTE    0
GRPHLH  BYTE    0


;PRIVATE DEFINES
;-------------------------------------------------------------------------------

TRBDUR  =       $06
TRBBDR  =       $0C
THRBBF  =       $C0
THRBCC  =       $03
THRBKL  =       22

THRBKM  =       SA_CLM$ + $01B8
THRBKU  =       SA_SCN$ + $01B8



;ROUTINES
;-------------------------------------------------------------------------------

        ZPOPT   OFF



;INSTALL THROBBER IRQ HANDLER
;-------------------------------------------------------------------------------
; CALL TO INSTALL THE IRQ HANDLER FOR THROBBER DRIVER
;
;       EXPORTED
;
;       INPUTS          NO ARGS
;       OUTPUTS         NONE
;       DESTROYS        .A, .P
THRBIN$ SEI                     

        LDA     VC_IRQ$         ;CHECK NOT ALREADY INSTALLED
        CMP     <THRBIRQ
        BNE     THRBI1
        LDA     VC_IRQ$ + 1
        CMP     >THRBIRQ
        BEQ     THRBI2

THRBI1  LDA     VC_IRQ$         ;NEED INSTALL, COPY EXISTING ADDRESS
        STA     THRBVC
        LDA     VC_IRQ$ + 1
        STA     THRBVC + 1

        LDA     #<THRBIRQ       ;INSTALL OUR IRQ HANDLER INTO VECTOR
        STA     VC_IRQ$
        LDA     #>THRBIRQ
        STA     VC_IRQ$ + 1

THRBI2  LDA     #$00            ;INIT
        STA     VICO2F$
        STA     VICSGV$
        STA     THRBTY

        CLI
        RTS


;CONFIGURE THROBBER
;-------------------------------------------------------------------------------
; CALL TO CONFIGURE THROBBER TYPE (ENABLE/DISABLE AND LOCATE/INIT/SELECT)
;
;       EXPORTED
;
;       INPUTS          .P_C = ENABLE/DISABLE
;                       .A   = THROBBER TYPE
;                       .X   = (FOR TYPES THRBTP$ AND THRBTW$) X COL
;                              (FOR TYPE  THRBTG$) PALETTE TYPE (0, 1)
;                       .Y   = (FOR TYPES THRBTP$ AND THRBTW$) Y ROW
;                              (FOR TYPE  THRBTB$) BEEP TYPE (0 LOW, 1 HIGH)
;                              (FOR TYPE  THRBTG$) INIT PROGRESS H (0-255)
;       OUTPUTS         NONE
;       DESTROYS        NONE
THRBCF$ SEI                     ; CONFIGURE THROBBER
        
        STA     THRBCA          ; WE NEED TO CACHE THESE ARGUMENTS
        STX     THRBCX
        STY     THRBCY

        PHP                     ; ALL OF THIS PUSHING TO THE STACK IS PROBABLY
        PHA                     ; UNECESSARY BUT I LIKE TO BE SURE.  IT CAN BE
        TXA                     ; OPTIMISED AT SOME TIME IN THE FUTURE IF NEEDS
        PHA                     ; BE.
        TYA
        PHA

        LDX     THRBCA          ; INIT FOR TYPE AND ENABLE/DISABLE
        LDA     #$00            ; .X = TYPE NUM
        ROL                     ; .Y = ENABLE/DISABLE
        TAY

        CPX     #$00            ; SANITY CHECK
        BEQ     CFX             ; TODO - COULD ALLOW "ALL OFF" WITH TYPE 0
        CPX     #THRBTTP
        BPL     CFX

        LDA     #$00
        CPX     #THRBTI$
        BNE     CFCN1

        LDX     #THRBTW$
        LDA     #$01
        BNE     CFCN2

CFCN1   CPX     #THRBTW$
        BNE     CFCN3

CFCN2   STA     THRBWN

CFCN3   DEX                     ; PREP FOR LOOKUP
        LDA     THRBTF,X        ; GET BIT VALUE FOR TYPE

        CPY     #$01            ; CHECK IF ENABLE OR DISABLE
        BNE     CFU
        
        BIT     THRBTY          ; CHECK NOT ALREADY ENABLED
        BNE     CFX

        ORA     THRBTY          ; STORE IS ENABLED
        STA     THRBTY
        
        LDA     THRBIL,X        ; GET ADDRESS OF ENABLE ROUTINE AND SELF
        STA     CFIJ+1          ; MODIFY TO CALL
        LDA     THRBIH,X
        STA     CFIJ+2

CFIJ    JSR     THRBINP         ; CALL ROUTINE FROM LOOKUP FOR ENABLE
        JMP     CFX             ; DONE
        
CFU     EOR     THRBTY          ; STORE IS DISABLED
        STA     THRBTY

        LDA     THRBUL,X        ; GET ADDRESS OF DISABLE ROUTINE AND SELF
        STA     CFUJ+1          ; MODIFY TO CALL
        LDA     THRBUH,X
        STA     CFUJ+2

CFUJ    JSR     THRBUNP         ; CALL ROUTINE FROM LOOKUP TO DISABLE

CFX     PLA                     ; COMPLETED
        TAY
        PLA
        TAX
        PLA
        PLP
        CLI
        RTS


;UPDATE PROGRESS THROBBER 
;-------------------------------------------------------------------------------
; CALL TO SET THE CURRENT PROGRESS STATE
;
;       EXPORTED
;
;       INPUTS          .Y   = PROGRESS H
;                       .X   = PROGRESS L
;       OUTPUTS         NONE
;       DESTROYS        NONE
THRBSG$ SEI
        STY     GRPHNH      
        STA     GRPHNL
        CLI
        RTS


; FILL GRAPH (ONLY DOES BG/FG, NO INCREMENTAL (L) PROGRESS)
; .X = #PALETTE, .Y = #>PROGRESS
GRHFIL  TXA
        ASL
        TAX
        LDA     GRHPBF,X
        STA     GPFBG+1
        INX
        LDA     GRHPBF,X 
        STA     GPFFG+1
        
        STY     GPFSF+1
        STY     GPFDB+1

        LDX     #$00
GPFSF   CPX     #$00
        BEQ     GPFDB
        LDA     #GPHFCH
        STA     GPHSSA,X
GPFFG   LDA     #$00
        STA     GPHCSA,X
        INX
        BNE     GPFSF

GPFDB   LDX     #$00
GPFBL   LDA     #GPHFCH
        STA     GPHSSA,X
GPFBG   LDA     #$00
        STA     GPHCSA,X
        INX
        BNE     GPFBL
        
        LDX     #$00
GPFTL   LDA     #GPHFCH
        STA     GPHTSA,X
        LDA     #GPHCL3
        STA     GPHTCA,X
        INX
        CPX     #$08
        BNE     GPFTL

        RTS

; UPDATE THE GRAPH (SET INCREMENTAL STATE)
; .X = #PALETTE, .Y = >#PROGRESS, .A = #<PROGRESS
GRHUPD  PHA
        
        STY     GPFUP+1

        TXA
        ASL
        TAX
        LDA     GRHPBF,X
        STA     GPUBG+1
        INX
        LDA     GRHPBF,X 
        STA     GPUFG+1
        
        PLA

        LDY     #$00
        LSR
        LSR
        LSR
        LSR
        LSR
        TAX
        TYA
        ROL
        CMP     #$01
        BNE     GPFUN
        INX
        
        TXA
        TAY

GPFUN   LDA     GRHPCS,X  

GPFUP   LDX     #$00
        STA     GPHSSA,X

        CPY     #$08
        BNE     GPUBG
GPUFG   LDA     #$00
        JMP     GPUFS
GPUBG   LDA     #$00
GPUFS   STA     GPHCSA,X
        
        RTS


;UTILITY ROUTINES
;-------------------------------------------------------------------------------

;CALCULATE COLOUR MEMORY POS
THRBCLC LDA     #>SA_CLM$       ; CALCULATE MEMORY ADDRESS FOR COLOUR POS
        STA     THRBC+2         ; THERE IS PROBABLY A BETTER WAY THAN THIS
        LDA     #<SA_CLM$       ; BUT IT REALLY ISN'T MY FORTE...        
        STA     THRBC+1
        
        LDA     #THCLCO - THCLCB
        STA     THCLCJ+1

        LDY     THRBCY
        CPY     #$00
THCLCJ  BEQ     THCLCO
THCLCB  LDX     #THRBKL+1
THCLC1  DEX
        BEQ     THCLC3
        INC     THRBC+1
        BNE     THCLC1
        INC     THRBC+2
THCLC2  BNE     THCLC1
THCLC3  DEY
        JMP     THCLCJ

THCLCO  LDX     THRBCX
        INX
        INY

        LDA     #THCLCX - THCLCB
        STA     THCLCJ+1

        JMP     THCLC1

THCLCX  RTS

;CALCULATE SCREEN MEMORY POS
THRBSNC LDA     #>SA_SCN$       ; CALCULATE MEMORY ADDRESS FOR SCREEN POS
        STA     THRBS+2         ; THERE IS PROBABLY A BETTER WAY THAN THIS
        LDA     #<SA_SCN$       ; BUT IT REALLY ISN'T MY FORTE...        
        STA     THRBS+1
        
        LDA     #THCLSO - THCLSB
        STA     THCLSJ+1

        LDY     THRBCY
        CPY     #$00
THCLSJ  BEQ     THCLSO
THCLSB  LDX     #THRBKL+1
THCLS1  DEX
        BEQ     THCLS3
        INC     THRBS+1
        BNE     THCLS1
        INC     THRBS+2
THCLS2  BNE     THCLS1
THCLS3  DEY
        JMP     THCLSJ

THCLSO  LDX     THRBCX
        INX
        INY

        LDA     #THCLSX - THCLSB
        STA     THCLSJ+1

        JMP     THCLS1

THCLSX  RTS



;ENABLE ROUTINES
;-------------------------------------------------------------------------------

;PROCESSING THROBBER ENABLE
THRBINP LDA     #$01
        STA     THRBPC
        LDA     #$00
        STA     THRBPP

        JSR     THRBCLC
        JSR     THRBSNC

        LDX     #THRBTW$-1
        LDA     THRBTF,X
        BIT     THRBTY
        BEQ     THINPX
        EOR     THRBTY
        STA     THRBTY

THINPX  RTS

;WAITING THROBBER ENABLE
THRBINW LDA     #$01
        STA     THRBWC
        LDA     #$00
        STA     THRBWP

        JSR     THRBCLC
        JSR     THRBSNC

        LDX     #THRBTP$-1
        LDA     THRBTF,X
        BIT     THRBTY
        BEQ     THINWX
        EOR     THRBTY
        STA     THRBTY
        
THINWX  RTS

;KEY PRESS THROBBER ENABLE
THRBINK LDA     #$01
        STA     THRBKC
        LDA     #$00
        STA     THRBKP
        STA     THRBKD

        LDX     #00
@1      LDA     THRBKT,X
        STA     THRBKU,X
        INX
        CPX     #THRBKL
        BNE     @1

        RTS

;BEEP THROBBER ENABLE
THRBINB LDA     #TRBBDR
        STA     THRBBC
        LDA     #THRBBF

        LDY     THRBCY
        BEQ     THRBB1

        STA     VICO3F$
        JMP     THRBB2

THRBB1  STA     VICO2F$

THRBB2  LDA     #$0F
        STA     VICSGV$

        RTS

;GRAPH THROBBER ENABLE
THRBING LDX     THRBCX
        STX     GRPPAL
        LDY     THRBCY
        STY     GRPHNH
        STY     GRPHLH

        JSR     GRHFIL

        LDA     #$00
        STA     GRPHNL

        RTS


;DISABLE ROUTINES
;-------------------------------------------------------------------------------

;PROCESSING THROBBER DISABLE
THRBUNP RTS

;WAITING THROBBER DISABLE
THRBUNW RTS

;KEY PRESS THROBBER DISABLE
THRBUNK RTS

;BEEP THROBBER DISABLE
THRBUNB LDA     #$00
        STA     VICSGV$
        STA     VICO2F$
        STA     VICO3F$

        RTS

;GRAPH THROBBER DISABLE
THRBUNG RTS


;IRQ HANDLER
;-------------------------------------------------------------------------------

THRBIRQ PHP                     ; THROBBER IRQ ROUTINE
        PHA                     ; ALL OF THIS PUSHING TO THE STACK IS PROBABLY
        TXA                     ; UNECESSARY BUT I LIKE TO BE SURE.  IT CAN BE
        PHA                     ; OPTIMISED AT SOME TIME IN THE FUTURE IF NEEDS
;       TYA                     ; BE.
;       PHA

        LDA     THRBTY
        BIT     THRBTF
        BEQ     TIRQ1

        PHA
        JSR     THRBFNP
        PLA
   
TIRQ1   BIT     THRBTF+1
        BEQ     TIRQ2

        PHA
        JSR     THRBFNW
        PLA

TIRQ2   BIT     THRBTF+2
        BEQ     TIRQ3

        PHA
        JSR     THRBFNK
        PLA

TIRQ3   BIT     THRBTF+3
        BEQ     TIRQ4

        PHA
        JSR     THRBFNB
        PLA

TIRQ4   BIT     THRBTF+4
        BEQ     TIRQD

        JSR     THRBFNG
        
;       PLA
;       TAY
TIRQD   PLA
        TAX
        PLA
        PLP
        JMP     (THRBVC)


;THROBBER JIFFY UPDATE FUNCTIONS
;-------------------------------------------------------------------------------

;MODIFY TO WRITE TO SCREEN/COLOUR MEMORY
THRBS   STA     SA_SCN$
        LDA     #THRBCC
THRBC   STA     SA_CLM$
        RTS

;PROCESSING THROBBER UPDATE
THRBFNP LDX     THRBPP
        LDA     THRBPT,X

        DEC     THRBPC
        BNE     TFNPX

        INX
        CPX     #$04
        BNE     TFNPU
        LDX     #$00

TFNPU   STX     THRBPP
        LDX     #TRBDUR
        STX     THRBPC

TFNPD   JMP     THRBS
TFNPX   RTS
    
;WAITING THROBBER UPDATE
THRBFNW LDX     THRBWP
        LDA     THRBWN
        BEQ     TFNW1

        LDA     THRBIT,X
        JMP     TFNW2

TFNW1   LDA     THRBWT,X

TFNW2   DEC     THRBWC
        BNE     TFNWX

        INX
        CPX     #$04
        BNE     TFNWU
        LDX     #$00

TFNWU   STX     THRBWP
        LDX     #TRBDUR
        STX     THRBWC

TFNWD   JMP     THRBS
TFNWX   RTS

;KEY PRESS THROBBER UPDATE
THRBFNK LDX     THRBKP          
        LDA     THRBKS,X
        TAY

        DEC     THRBKC
        LDA     THRBKC
        BNE     TFNKD
      
        TYA
        LDX     #00
@1      STA     THRBKM,X
        INX
        CPX     #THRBKL
        BNE     @1

        LDX     THRBKP          
        LDA     #$00
        CMP     THRBKD
        BEQ     TFNKA
        DEX
        BNE     TFNKU
        STA     THRBKD
        JMP     TFNKU
        
TFNKA   INX
        CPX     #$03
        BNE     TFNKU
        LDA     #$01
        STA     THRBKD

TFNKU   STX     THRBKP
        LDA     #TRBDUR

TFNKD   STA     THRBKC
        RTS

;BEEP THROBBER UPDATE
THRBFNB DEC     THRBBC
        BNE     TFNBD

        JSR     THRBUNB

        LDX     #THRBTB$-1
        LDA     THRBTF,X
        EOR     THRBTY
        STA     THRBTY

TFNBD   RTS


;GRAPH THROBBER UPDATE
THRBFNG LDY     GRPHLH
        CPY     GRPHNH
        BEQ     TFNGN

        LDX     GRPPAL
        LDA     #$FF
        JSR     GRHUPD
        

TFNGN   LDX     GRPPAL
        LDY     GRPHNH
        STY     GRPHLH
        LDA     GRPHNL

        JSR     GRHUPD

        RTS
