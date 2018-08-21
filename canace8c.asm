  TITLE   "Source for CANACE8C node for CBUS"
; filename CANACE8Cp.asm    1/11/09
; 
; Uses 4 MHz resonator and PLL for 16 MHz clock
; This is an 8 input SLiM producer node with readout.
; Has 8 switch / logic inputs with 100K pullups.
; Uses a 6 way DIP switch to set base NN and CAN ID (unless set otherwise)
; and for learning request events (learn + unlearn)
; Sends 8 ON / OFF events using the 32 bit EN protocol.
; Sends response event with the inputs as the LSbyte

; The setup timer is TMR3. This should not be used for anything else
; CAN bit rate of 125 Kbits/sec
; Standard frame only


; this code is for 18F2480


; event command is 90h (on) and 91h (off) for now.
; This code includes the  CAN ID enumeration readback (FLiM compatibility)
; The NN and CAN ID are set by the 4 DIP switches. 

; working as just a producer as per CANACE8  02/12/07

; now add the consumer bits

;seems to be working. not tried unlearn yet.
;If the request event is 0x90 or 0x91 (ON or OFF) the response is an ON or OFF
;If the request event is 0x92 (request), then the response is 0x93 (info)
;The node adds a 1 to the EN hi byte to distinguish response ON / OFF from change 
;triggered events.

;added node number change during running.  27/12/07
;added erase of all ENs if reset with unlearn on.
;added RUN LED 
;added handling of short events for 'many' producers


;DIL switch ettings

; 1 NN select LSB
; 2 NN select
; 3 NN select
; 4 NN select   MSB
; 5 Learn
; 6 Unlearn / reset

; 1 to 4 also select the response type when in learn mode. Must be put back to
; the NN after learning.


;CANACE8Ca  as CANACE8C but with all self enumeration removed 31/01/08
;Change to make NN and CAN ID the same
;movff changed for TX buffer load

;CANACE8Ca_2 is ACE8Ca modified for optional responses to a trigger event
;Uses the NN setting switches during learn mode to select response type
;Mode 00 is 8 successive events identical to an 'input change' event. Event is ON or
;OFF depending on the input state. A low (0v) input generates an ON event.
;Also has NN range extended by 3 bits using the spare inputs. Note, no jumper gives a 0 in the
;NN, the opposite to the DIL switches. Allows the basic 1 to 16 NN without any jumpers.
;Changed so there is no output following a learn. This was a problem with multiple ACE8C_2
;modules if you wanted them to learn the same event e.g. for layout config readout.
;mods to sendTX1
;now CANACE8Cc_2.  Modified CANACE8Cb_2 for event type 0001
;sends ON event in response to trigger. LSByte is 8 inputs. Bit 0 set in d3.
;changed to CANACE8C rev d. 03/05/08
;increased number of events
;LED flashes when full
;working OK  04/05/08
;version e incorporates changes by Roger Healey to the scan routine for multiple simultaneous input changes
;04/05/08  added clear carry in change1
;version f. Changed way the mode is stored.  03/07/08
;changed the CONFIG and added 'LOW' so it builds without warnings. 16/07/08
;added clear of Tx1d3 in ev_set so it gets the response event right if first time. 16/07/08
;mods to EV setting to correct bug. 22/08/08
;revision now 'g'
;revision 'h'  (29/08/08)  response to RTR added back for FLiM compatibiliy
;seems to work OK (29/08/08)
;ignores any zero data frames to avoid buffer overflow during enum.
;Rev 'i' and Rev 'j' not used

;08/11/08  Rev k.  fix bug where input changes are incorrectly reported as ON events by Roger Healey

; 19/11/08 Rev m. fix bug where start-of-day event messages can get lost
;21/09/9 Rev n. Changed top bit of NN jumpers to select mode. Now can just send ON events on switch change.
; Default (no jumper) is original ON / OFF. With jumper is ON only. can be changed while running.
; NN range now limited to 1 to 64. 

; 1/11/09 Rev p (no Rev o) Fix to CAN filters so extended frames are rejected






; 
; Assembly options
  LIST  P=18F2480,r=hex,N=75,C=120,T=ON

  include   "p18f2480.inc"
  
  ;definitions  Change these to suit hardware.
  

LEARN   equ 4 ;learn switch in port A
UNLEARN equ 5 ;unlearn switch in port A

CMD_ON  equ 0x90  ;on event
CMD_OFF equ 0x91  ;off event
CMD_REQ equ 0x92
SCMD_ON equ 0x98
SCMD_OFF  equ 0x99
SCMD_REQ  equ 0x9A
EN_NUM  equ .32   ;number of allowed events

;set config registers

; note. there seem to be differences in the naming of the CONFIG parameters between
; versions of the p18F2480.inf files

  CONFIG  FCMEN = OFF, OSC = HSPLL, IESO = OFF
  CONFIG  PWRT = ON,BOREN = BOHW, BORV=0
  CONFIG  WDT=OFF
  CONFIG  MCLRE = ON
  CONFIG  LPT1OSC = OFF, PBADEN = OFF
  CONFIG  DEBUG = OFF
  CONFIG  XINST = OFF,LVP = OFF,STVREN = ON,CP0 = OFF
  CONFIG  CP1 = OFF, CPB = OFF, CPD = OFF,WRT0 = OFF,WRT1 = OFF, WRTB = OFF
  CONFIG  WRTC = OFF,WRTD = OFF, EBTR0 = OFF, EBTR1 = OFF, EBTRB = OFF




;original CONFIG settings left here for reference
  
; __CONFIG  _CONFIG1H,  B'00100110' ;oscillator HS with PLL
; __CONFIG  _CONFIG2L,  B'00001110' ;brown out voltage and PWT  
; __CONFIG  _CONFIG2H,  B'00000000' ;watchdog time and enable (disabled for now)
; __CONFIG  _CONFIG3H,  B'10000000' ;MCLR enable  
; __CONFIG  _CONFIG4L,  B'10000001' ;B'10000001'  for   no debug
; __CONFIG  _CONFIG5L,  B'00001111' ;code protection (off)  
; __CONFIG  _CONFIG5H,  B'11000000' ;code protection (off)  
; __CONFIG  _CONFIG6L,  B'00001111' ;write protection (off) 
; __CONFIG  _CONFIG6H,  B'11100000' ;write protection (off) 
; __CONFIG  _CONFIG7L,  B'00001111' ;table read protection (off)  
; __CONFIG  _CONFIG7H,  B'01000000' ;boot block protection (off)

; processor uses  4 MHz. Resonator

;********************************************************************************



;****************************************************************
; define RAM storage
  
  CBLOCK  0   ;file registers - access bank
          ;interrupt stack for low priority
          ;hpint uses fast stack
  W_tempL
  St_tempL
  Bsr_tempL
  PCH_tempH   ;save PCH in hpint
  PCH_tempL   ;save PCH in lpint (if used)
  Fsr_temp0L
  Fsr_temp0H 
  Fsr_temp1L
  Fsr_temp1H 
  Fsr_temp2L
  
  TempCANCON
  TempCANSTAT
  CanID_tmp ;temp for CAN Node ID
  IDtemph   ;used in ID shuffle
  IDtempl
  NN_temph  ;node number in RAM
  NN_templ
  
  IDcount   ;used in self allocation of CAN ID.
  Datmode   ;flag for data waiting 
  Count   ;counter for loading
  Count1
  Latcount  ;latency counter

  Temp    ;temps
  Temp1
  Intemp
  Intemp1
  Inbit
  Incount
  Input
  Atemp   ;port a temp value
  Mode    ;used to flag mode (on/off or just on)
  
          ;the above variables must be in access space (00 to 5F)
        
  

  ENDC      ;ends at 5F 
  

  
  CBLOCK  h'60' ;rest of bank 0
  
  Rx0con      ;start of receive packet 0
  Rx0sidh
  Rx0sidl
  Rx0eidh
  Rx0eidl
  Rx0dlc
  Rx0d0
  Rx0d1
  Rx0d2
  Rx0d3
  Rx0d4
  Rx0d5
  Rx0d6
  Rx0d7
  
  Cmdtmp    ;command temp for number of bytes in frame jump table
  
  DNindex   ;holds number of allowed DNs
  Match   ;match flag
  DNcount   ;which DN matched?
  ENcount   ;which EN matched
  ENcount1  ;temp for count offset
  ENend   ;last  EN number
  ENtemp
  EVtemp    ;holds current EV
  EVtemp1 
  EVtemp2   ;holds current EV qualifier
  EVtemp3 
  Mask
  Shift
  Shift1
  
  
  Eadr    ;temp eeprom address
  
  Tx1con      ;start of transmit frame  1
  Tx1sidh
  Tx1sidl
  Tx1eidh
  Tx1eidl
  Tx1dlc
  Tx1d0
  Tx1d1
  Tx1d2
  Tx1d3
  Tx1d4
  Tx1d5
  Tx1d6
  Tx1d7
  
  ;add variables to suit

    
  ENDC
  
  CBLOCK  0x100   ;bank 1
  EN1         ;start of EN ram
  EN1a
  EN1b
  EN1c
  
  EN2
  EN2a
  EN2b
  EN2c
  
  ENDC
  
  CBLOCK  0x200   ;bank 2
  EV1         ;start of EV ram
  ENDC

;****************************************************************
;
;   start of program code

    ORG   0000h
    nop           ;for debug
    goto  setup

    ORG   0008h
    goto  hpint     ;high priority interrupt

    ORG   0018h 
    goto  lpint     ;low priority interrupt


;*******************************************************************

    ORG   0020h     ;start of program
; 
;
;   high priority interrupt. Used for CAN receive and transmit error.

hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
    movff PCLATH,PCH_tempH    ;save PCLATH
    clrf  PCLATH
  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H
    
    

  
    movf  TempCANSTAT,W     ;Jump table
    andlw B'00001110'
    addwf PCL,F     ;jump
    bra   back
    bra   errint      ;error interrupt
    bra   back
    bra   back
    bra   back
    bra   rxb1int     ;only receive interrupts used
    bra   rxb0int
    bra   back
    
rxb1int bcf   PIR3,RXB1IF   ;uses RB0 to RB1 rollover so may never use this
    
    lfsr  FSR0,Rx0con   ;
    
    goto  access
    
rxb0int bcf   PIR3,RXB0IF
  
    lfsr  FSR0,Rx0con
    
    goto  access
    
    ;error routine here. Only acts on lost arbitration  
errint  movlb .15         ;change bank      
    btfss TXB1CON,TXLARB
    bra   errbak        ;not lost arb.
    movf  Latcount,F      ;is it already at zero?
    bz    errbak
    decfsz  Latcount,F
    bra   errbak
    bcf   TXB1CON,TXREQ
    movlw B'00111111'
    andwf TXB1SIDH,F      ;change priority
txagain bsf   TXB1CON,TXREQ   ;try again
          
errbak  movlb .15
    bcf   RXB1CON,RXFUL 
    movlb 0
    bcf   RXB0CON,RXFUL ;ready for next
      
    bra   back1

access  movf  CANCON,W        ;switch buffers
    andlw B'11110001'
    movwf CANCON
    movf  TempCANSTAT,W
    andlw B'00001110'
    iorwf CANCON
    lfsr  FSR1,RXB0CON  ;this is switched bank
load  movf  POSTINC1,W
    movwf POSTINC0
    movlw 0x6E      ;end of access buffer lo byte
    cpfseq  FSR1L
    bra   load
    bcf   RXB0CON,RXFUL
    
    
    btfsc Rx0dlc,RXRTR    ;is it RTR?
    bra   isRTR
    movf  Rx0dlc,F      ;ignore zero data frames
    bz    back
;   btfss S_PORT,S_BIT  ;setup mode?
;   bra   setmode 
    
    bsf   Datmode,0   ;valid message frame  
    
back  movlb .15
    bcf   RXB1CON,RXFUL 
    movlb 0
    bcf   RXB0CON,RXFUL ;ready for next
    
  
back1 clrf  PIR3      ;clear all flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
    movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers

    ;RTR response in SLiM version forFLiM compatibility

isRTR ;btfss  S_PORT,S_BIT  ;setup mode?
    ;bra    back      ;back
    movlb .15
    bsf   TXB2CON,TXREQ ;send ID frame - preloaded in TXB2
isRTR1  btfsc TXB2CON,TXREQ ;wait till sent
    bra   isRTR1
    movlb 0
    bra   back


;   Not used for SLiM
;setmode  movf  Rx0dlc,F
;   bnz   back        ;only zero length frames for setup
    
;   call  shuffin       ;get CAN ID as a single byte in W
;   cpfseq  IDcount
;   bra   back        ;not equal
;   incf  IDcount,F
;   movlw 0x7F
;   cpfslt  IDcount       ;too many?
;   decf  IDcount,F     ;stay at 7F
;   bra   back


;**************************************************************
;
;
;   low priority interrupt. (if used)
; 

lpint retfie  
        
        
  
            
        
  
                

;*********************************************************************


    


main  btfss PORTA,LEARN   ;ignore NN switches if in learn mode
    bra   main1
    movlw B'00001111'   ;get DIP switch setting
    andwf PORTA,W
    movwf Temp
    movlw B'00010010'   ;get jumpers for high bits. Just bits 1 and 4.
    andwf PORTB,W
    movwf Temp1
    rlncf Temp1,F
    btfsc Temp1,2
    bsf   Temp1,4
    comf  Temp1,W
    andlw B'00110000'
    iorwf Temp,W
    addlw 1     
    
    cpfseq  Atemp
    bra   setnew
    bra   main1
setnew  movwf Atemp

    movwf NN_templ

    clrf  NN_temph
    call  newid1      ;put ID into Tx1buf, TXB2 and ID number store
  
  
  
main1 clrf  Mode      ;check mode
    btfss PORTB,5     ;mode change?
    bsf   Mode,0      ;set mode to 1 (no OFF)

    btfss PIR2,TMR3IF   ;flash timer overflow?
    bra   noflash
    btg   PORTB,6     ;toggle LED
    bcf   PIR2,TMR3IF
noflash btfsc Datmode,0   ;any new CAN frame received?
    bra   packet      ;yes
    bra   do        ;look for inputs
    

                ;main packet handling is here
    
packet  movlw CMD_ON  ;only ON, OFF and REQ events supported
    subwf Rx0d0,W
    bz    go_on
    movlw CMD_OFF
    subwf Rx0d0,W
    bz    go_on
    movlw CMD_REQ
    subwf Rx0d0,W
    bz    go_on
    movlw SCMD_ON
    subwf Rx0d0,W
    bz  short
    movlw SCMD_OFF
    subwf Rx0d0,W
    bz  short
    movlw SCMD_REQ
    subwf Rx0d0,W
    bz  short 
main2 bcf   Datmode,0
    goto  main      ;loop

short   clrf  Rx0d1
    clrf  Rx0d2 
    
go_on btfss PORTA,LEARN
    bra   learn1      ;is in learn mode
    call  enmatch
    sublw 0
    bz    do_it
    bra   main2     ;not here
    
do_it 
    call  ev_set      ;do it
    bra   main2
    
    
learn1  call  enmatch     ;is it there already?
    sublw   0
    bz    isthere
    btfss PORTA,UNLEARN ;if unset and not here
    bra   l_out     ;do nothing else 
    call  learnin     ;put EN into stack and RAM
    sublw 0
    bz    new_EV
    bra   l_out     ;too many
isthere btfss PORTA,UNLEARN ;is it here and unlearn,goto unlearn
    bra   unlearn     ;else modify EVs
    bra   mod_EV
    
    
  
    
  
new_EV  movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    decf  EEDATA,W
    movwf ENcount       ;recover EN counter
mod_EV  rlncf ENcount,W     ;two byte values
    addlw LOW EVstart     ;point to EV
    movwf EEADR
  
    call  getop
    movf  EVtemp,W      ;EVtemp has switch setting (0 to 15)
        
  
    call  eewrite       ;put back EV value  
;   incf  EEADR       ;other qualifier byte not used in ACE8C yet
;   bsf   EECON1,RD
;   movf  EVtemp2,W
        
;   iorwf EEDATA,W
;   call  eewrite       ;put back EV qual value 
  
;   call  ev_set        ;try it - no. This messes up any other ACE8C_2 modules
      

l_out bcf   Datmode,0
    clrf  PCLATH
    goto  main2
                ;unlearn an EN. 
unlearn movlw LOW ENindex+1   ;get number of events in stack
    movwf EEADR
    bsf   EECON1,RD
    
    movff EEDATA,ENend
    movff EEDATA,ENtemp
    rlncf ENend,F     ;ready for end value
    rlncf ENend,F
    movlw LOW ENstart
    addwf ENend,F     ;end now points to next past end in EEPROM
    movlw 4
    addwf ENend,F
    rlncf ENcount,F   ;Double the counter for two bytes
    rlncf ENcount,F   ;Double the counter for two bytes
    movlw LOW ENstart + 4
    addwf ENcount,W
    movwf EEADR
un1   bsf   EECON1,RD
    movf  EEDATA,W    ;get byte
    decf  EEADR,F
    decf  EEADR,F
    decf  EEADR,F
    decf  EEADR,F
    call  eewrite     ;put back in
    movlw 5
    addwf EEADR,F
    movf  ENend,W
    cpfseq  EEADR
    bra   un1
    
    rrncf ENcount,F   ;back to double bytes
    rlncf ENtemp,F
    movlw LOW EVstart
    addwf ENtemp,F
    movlw 2
    addwf ENtemp,F
    movlw LOW EVstart + 2
    addwf ENcount,W
    movwf EEADR
un2   bsf   EECON1,RD
    movf  EEDATA,W    ;get byte
    decf  EEADR,F
    decf  EEADR,F
    call  eewrite     ;put back in
    movlw 3
    addwf EEADR,F
    movf  ENtemp,W
    cpfseq  EEADR
    bra   un2
    movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf Temp
    decf  Temp,W
    call  eewrite     ;put back number in stack less 1
    call  en_ram      ;rewrite RAM stack
    bcf   T3CON,TMR3ON  ;flash timer off
    bcf   PIR2,TMR3IF
    bcf   PORTB,6     ;LED off
    bra   l_out
        

  
do    call  scan      ;scan inputs for change
    
                
    goto  main
  
  
  
    

        
    
    
    
    
;***************************************************************************
;   main setup routine
;*************************************************************************

setup clrf  INTCON      ;no interrupts yet
    clrf  ADCON0      ;turn off A/D, all digital I/O
    movlw B'00001111'
    movwf ADCON1
    
    ;port settings will be hardware dependent. RB2 and RB3 are for CAN.
    ;set S_PORT and S_BIT to correspond to port used for setup.
    ;rest are hardware options
    
  
    movlw B'00111111'   ;Port A inputs for NN and learn / unlearn (DIL switch)
    movwf TRISA     ;
    movlw B'00111011'   ;RB0 is setup PB, RB1, RB4 are bits 5 and 6 of ID. Bit 5 is mode.
            ;RB2 = CANTX, RB3 = CANRX, 
            ;RB6,7 for debug and ICSP and diagnostics
    movwf TRISB
    bcf   PORTB,6
    bsf   PORTB,2     ;CAN recessive
    movlw B'11111111'   ;Port C  is the 8 switch inputs
    movwf TRISC
    movf  PORTC,W
    movwf Input   ;initial switch positions
    
; next segment is essential.
    
    bsf   RCON,IPEN   ;enable interrupt priority levels
    clrf  BSR       ;set to bank 0
    clrf  EECON1      ;no accesses to program memory  
    clrf  Datmode
    clrf  Latcount
    clrf  ECANCON     ;CAN mode 0 for now. 
     
    bsf   CANCON,7    ;CAN to config mode
    movlw B'00000011'   ;set CAN bit rate at 125000 for now
    movwf BRGCON1
    movlw B'10011110'   ;set phase 1 etc
    movwf BRGCON2
    movlw B'00000011'   ;set phase 2 etc
    movwf BRGCON3
    movlw B'00100000'
    movwf CIOCON      ;CAN to high when off
    movlw B'00100100'   ;B'00100100'
    movwf RXB0CON     ;enable double buffer of RX0
    movlb .15
    movlw B'00100000'   ;block extended frames in both Rx buffers
    movwf RXB1CON
    clrf  RXF0SIDL
    clrf  RXF1SIDL
    movlb 0
    


    
mskload lfsr  0,RXM0SIDH    ;Clear masks, point to start
mskloop clrf  POSTINC0    
    movlw LOW RXM1EIDL+1    ;end of masks
    cpfseq  FSR0L
    bra   mskloop
    
    clrf  CANCON      ;out of CAN setup mode
    movlw B'10110000'
    movwf T3CON     ;set T3 for LED flash
    
    clrf  Tx1con
    clrf  Tx1eidh
    clrf  Tx1eidl
    movlw B'00100011'
    movwf IPR3      ;high priority CAN RX and Tx error interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2



;next segment required
    
;   movlw B'00000001'
;   movwf IDcount     ;set at lowest value for starters
    
    clrf  INTCON2     ;
    clrf  INTCON3     ;
    

    movlw B'00100011'   ;Rx0 and RX1 interrupt and Tx error
    movwf PIE3
  
    clrf  PIR1
    clrf  PIR2
    clrf  PIR3      ;clear all flags
    bcf   RXB0CON,RXFUL ;enable RX0 buffer
  
    
    
    ;   test for setup mode
    

  
    
setid movlw B'00001111'   ;get DIP switch setting
    andwf PORTA,W
    movwf Temp
    movlw B'00010010'   ;get jumpers for high bits
    andwf PORTB,W
    movwf Temp1
    rlncf Temp1,F
    btfsc Temp1,2
    bsf   Temp1,4
    comf  Temp1,W
    andlw B'00110000'
    iorwf Temp,W
    addlw 1       ;NN start at 1
    movwf Atemp     ;for any changes

    clrf  NN_temph
    movwf NN_templ
    clrf  Mode      ;mode is normal
    btfss PORTB,5     ;is it ON only?
    bsf   Mode,0      ;flag ON only
  
  
    
    call  newid1      ;put ID into Tx1buf, TXB2 and ID number store
    
    ;test for clear all events
    btfss PORTA,LEARN   ;ignore the clear if learn is set
    goto  seten
    btfss PORTA,UNLEARN
    call  enclear     ;clear all events if unlearn is set during power up
seten call  en_ram      ;put events in RAM
  
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bsf   PORTB,7     ;RUN LED on.
    goto  main


    
;****************************************************************************
;   start of subroutines    



;   Send contents of Tx1 buffer via CAN TXB1

sendTX1 lfsr  FSR0,Tx1con
    lfsr  FSR1,TXB1CON
    
    movlb .15       ;check for buffer access
ldTx2 btfsc TXB1CON,TXREQ
    bra   ldTx2
;   bcf   TXB1CON,TXREQ
    movlb 0
ldTX1 movf  POSTINC0,W
    movwf POSTINC1  ;load TXB1
    movlw Tx1d7+1
    cpfseq  FSR0L
    bra   ldTX1

    
    movlb .15       ;bank 15
tx1test btfsc TXB1CON,TXREQ ;test if clear to send
    bra   tx1test
    bsf   TXB1CON,TXREQ ;OK so send
    
tx1done movlb 0       ;bank 0
    return          ;successful send

    
    




    

newid1  movwf CanID_tmp   ;put in stored ID   
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
    clrf  TXB2CON
    clrf  TXB2SIDH
    movf  IDtemph,W
    movwf TXB2SIDH
    movf  IDtempl,W
    movwf TXB2SIDL
    movlw 0xB0
    iorwf TXB2SIDH    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0
    return



    
;*****************************************************************************
;
;   shuffle for standard ID. Puts 7 bit ID into IDtemph and IDtempl for CAN frame
shuffle movff CanID_tmp,IDtempl   ;get 7 bit ID
    swapf IDtempl,F
    rlncf IDtempl,W
    andlw B'11100000'
    movwf IDtempl         ;has sidl
    movff CanID_tmp,IDtemph
    rrncf IDtemph,F
    rrncf IDtemph,F
    rrncf IDtemph,W
    andlw B'00001111'
    movwf IDtemph         ;has sidh
    return

;*********************************************************************************

;   reverse shuffle for incoming ID. sidh and sidl into one byte.

shuffin movff Rx0sidl,IDtempl
    swapf IDtempl,F
    rrncf IDtempl,W
    andlw B'00000111'
    movwf IDtempl
    movff Rx0sidh,IDtemph
    rlncf IDtemph,F
    rlncf IDtemph,F
    rlncf IDtemph,W
    andlw B'01111000'
    iorwf IDtempl,W     ;returns with ID in W
    return
;************************************************************************************
;   
eeread  bcf   EECON1,EEPGD  ;read a EEPROM byte, EEADR must be set before this sub.
    bcf   EECON1,CFGS
    bsf   EECON1,RD
    movf  EEDATA,W
    return

;**************************************************************************
eewrite movwf EEDATA      ;write to EEPROM, EEADR must be set before this sub.
    bcf   EECON1,EEPGD
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    
    clrf  INTCON  ;disable interrupts
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR
eetest  btfsc EECON1,WR
    bra   eetest
    bcf   PIR2,EEIF
    bcf   EECON1,WREN
    movlw B'11000000'
    movwf INTCON    ;reenable interrupts
    
    return  
    
;***************************************************************

scan  movf  PORTC,W
    movwf Intemp
    movf  Intemp,W
    cpfseq  Input     ;any change?
    bra   change2
    return
change2 call  dely      ;debounce
  
    movf  Intemp,W
    cpfseq  PORTC     ;same?
    return          ;no so bounce

change  xorwf Input,W     ;which has changed
    movwf Intemp1     ;hold it
    clrf  Incount
    clrf  Inbit
    bsf   Inbit,0     ;rolling bit
change1 bcf   STATUS,C    ;carry bit must be clear
    rrcf  Intemp1,F
    bc    this
    incf  Incount,F
    rlcf  Inbit,F   ;added by Roger
    bc  end_scan
    bra   change1
this  movff Incount,Tx1d4 ;EN number lo byte
    incf  Tx1d4     ;ENs start at 1
    movlw B'10010000'   ;Command byte (temp for now = 90h)
    movwf Tx1d0     ;set up event
    movf  Intemp,W
    andwf Inbit,W     ;what is the new state - (bug fix for incorrect event type)
    bz    this0     ;is a 0  (an ON)
    btfsc Mode,0      ;what mode?
    bra   end_scan    ;if mode 1, then don't send an OFF
    bsf   Tx1d0,0     ;set to a 1 (off state)
this2   movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    clrf  Tx1d3
    movlw   5
    movwf Tx1dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh     ;low priority
    movlw .10
    movwf Latcount
    call  sendTX1     ;send frame
    incf  Incount,F
    rlcf  Inbit,F
    bc  end_scan
    call  dely
    bra change1
end_scan    movff Intemp,Input
    return
    
this0 bcf   Tx1d0,0     ;set to a 0 (on state)
    bra   this2



    nop
    return

;*********************************************************

;   learn input of EN

learnin btfss PORTA,UNLEARN   ;don't do if unlearn
    return
    movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf ENcount   ;hold pointer
    movlw EN_NUM
    cpfslt  ENcount
    retlw 1         ;too many
    lfsr  FSR0,EN1      ;point to EN stack in RAM
    
    rlncf ENcount,F     ;double it
    rlncf ENcount,F     ;double again
    movf  ENcount,W
    movff Rx0d1,PLUSW0    ;put in RAM stack
    addlw 1
    movff Rx0d2,PLUSW0
    addlw 1
    movff Rx0d3,PLUSW0
    addlw 1
    movff Rx0d4,PLUSW0
    movlw LOW ENstart
    addwf ENcount,W
    movwf EEADR
    movf  Rx0d1,W       ;get EN hi byte
    call  eewrite
    incf  EEADR
    movf  Rx0d2,W
    call  eewrite
    incf  EEADR
    movf  Rx0d3,W
    call  eewrite
    incf  EEADR
    movf  Rx0d4,W
    call  eewrite
    
    
    movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    addlw 1         ;increment for next
    movwf Temp
    call  eewrite       ;put back
    movlw EN_NUM        ;is it full now?
    subwf Temp,W
    bnz   notful
    bsf   T3CON,TMR3ON    ;set for flash
    retlw 1
notful  retlw 0
    
;**************************************************************************
;
;   EN match. Compares EN (in Rx0d1, Rx0d2, Rx0d3 and Rx0d4) with stored ENs
;   If match, returns with W = 0
;   The matching number is in ENcount. 
;
enmatch lfsr  FSR0,EN1  ;EN ram image
    movlw LOW ENindex+1 ;
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf Count
    movf  Count,F
  
    bz    en_out    ;if no events set, do nothing
    clrf  ENcount
  
    
ennext  clrf  Match
    movf  POSTINC0,W
    cpfseq  Rx0d1
    incf  Match
    movf  POSTINC0,W
    cpfseq  Rx0d2
    incf  Match
    movf  POSTINC0,W
    cpfseq  Rx0d3
    incf  Match
    movf  POSTINC0,W
    cpfseq  Rx0d4
    incf  Match
    tstfsz  Match
    bra   en_match
    rlncf ENcount,W   ;get EVs
    addlw LOW EVstart   
    movwf EEADR
    bcf   EEADR,0   ;multiple of 2
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf EVtemp    ;EV
    incf  EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf EVtemp2 ;EV qualifier
    
    retlw 0     ;is a match
en_match  
    movf  Count,F
    bz    en_out
    decf  Count,F
    incf  ENcount,F
    bra   ennext
en_out  retlw 1   
    
;********************************************************************
;   Do an event.  arrives with EV in EVtemp and EVtemp2

ev_set  clrf  Tx1d3   ;hi byte of event
    movlw 0     ;what is EV?
    subwf EVtemp,W
    bz    state_seq ;send state sequence if EV = 0
    movlw 1
    subwf EVtemp,W
    bz    route   ;send event to set route
    return        ;no more yet
state_seq
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    clrf  Tx1d4
    incf  Tx1d4   ;start at 1
    btfsc PORTC,0   ;test input state
    bsf   Tx1d0,0   ;off
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,1
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,2
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,3
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,4
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,5
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,6
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    call  dely
    movlw 0x90
    movwf Tx1d0   ;set for ON
    btfsc PORTC,7
    bsf   Tx1d0,0   ;off
    incf  Tx1d4
    call  sendTX
    return
    
route movlw 0x90
    movwf Tx1d0   ;on events only
    bsf   Tx1d3,0   ;to distinguish it from input change
    movf  PORTC,W
    movwf Tx1d4   ;set event to switch inputs
    call  sendTX
    bcf   Tx1d3,0
    return 
    
sendTX  movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
;   clrf  Tx1d3
    movlw   5
    movwf Tx1dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh     ;low priority
    movlw .10
    movwf Latcount
    call  sendTX1     ;send frame
    return      

;**************************************************************************

getop movlw B'00001111'   ;get DIP switch setting for output
    andwf PORTA,W
    movwf EVtemp
    clrf  EVtemp2     ;event qualifier (type) for output. Not used in ACE8C
    
    return

              
;**********************************************************************
;   loads ENs from EEPROM to RAM for fast access
;   shifts all 32 even if less are used

en_ram  movlw EN_NUM
    movwf Count     ;number of ENs allowed 
    
    bcf   STATUS,C    ;clear carry
    rlncf Count,F     ;double it
    rlncf Count,F     ;double again
    lfsr  FSR0,EN1    ;set FSR0 to start of ram buffer
    movlw LOW ENstart     ;load ENs from EEPROM to RAM
    movwf EEADR
enload  bsf   EECON1,RD   ;get first byte
    movf  EEDATA,W
    movwf POSTINC0
    incf  EEADR
    decfsz  Count,F
    bra   enload
    return  
    
    
;   clears all stored events

enclear movlw EN_NUM * 6 + 2    ;number of locations in EEPROM
    movwf Count
    movlw LOW ENindex
    movwf EEADR
enloop  movlw 0
    call  eewrite
    incf  EEADR
    decfsz  Count
    bra   enloop
    return  

    


;*********************************************************
;   a delay routine

;bigdely  movlw .20
;   goto  dely3
      
dely  movlw .10
dely3 movwf Count1
dely2 clrf  Count
dely1 decfsz  Count,F
    goto  dely1
    decfsz  Count1
    bra   dely2
    return    
    
;************************************************************************   
  ORG 0xF00000      ;EEPROM data. Defaults
  


ENindex de  0,0   ;points to next available EN number (only hi byte used)

  ORG 0xF00002

ENstart 

    ORG 0xF00082
    
EVstart de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0
      
    end


