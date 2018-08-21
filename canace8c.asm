;     TITLE   "Source for CANACE8C/CANTOTI node for CBUS"
; filename CANACE8C_v2k.asm 25-Jul-13 PJW
; Uses 4 MHz resonator and PLL for 16 MHz clock
; This is an 8 input FLiM producer node with readout.
; Has 8 switch / logic inputs with pullups.
; Uses a 6 way DIP switch to set base NN and CAN ID (unless set otherwise)and 
; for learning request events (learn + unlearn)
; Sends 8 ON / OFF events using the 32 bit EN protocol.
; Sends response event with the inputs as the LSbyte
; Additional input control for inversion, delays and push button input.

; This source file can also generate CANTOTI code by defining 'CANTOTI'

; The setup timer is TMR3. This should not be used for anything else
; CAN bit rate of 125 Kbits/sec
; Standard frame only


; this code is for 18F2480


; event command is 90h (on) and 91h (off) for now.
; This code includes the CAN ID enumeration readback (FLiM compatibility)
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
;version e incorporates changes by Roger Healey to the scan routine for multiple simultaneous
; input changes
;04/05/08
;Changed for Full Model  01/06/08
;1st stage is to restore the self enumeration, leaving DIL switches for SLiM.
;CAN_ID so it can be tested against other self enum. modules
;mods to error handling
;enum scheme seems OK now. 04/06/08
;now add rest of FLiM. DIL switches not used now.
;Problem found on the event numbers with the input switches. (may apply to CANACE8C as well)
;Fixed by added clear of STATUS bit
;setting and readback of NN working  05/06/08
;Flash timer changed to TMR0.
;NN readback (0x10)removed. Always sent in setup mode.
;added node number release frame (0x51)
;added keep alive frame  (0x52)
;set learn mode (0x53)
;out of learn mode (0x54)
;clear all events (0x55)
;read no. of events left (0x56)
;set event in learn mode  (0xD2)  uses EV indexing
;read event in learn mode (0xB2)
;unset event in learn mode (0x95)
;reply to 0xB2. (0xD3)Also sent if attempt to read / write too many EVs. EV# = 0 if too many.
;set NV  in learn mode (0x96)
;read NV (0x71)by NV index
;reply to read NV (0x97)

;read node parameters (0x10) Only works in setup mode. sends string of 7 bytes (0xEF)
;corrected so it only responds to an ON event (0x90) 15/07/08
;added NN check to rden  (0x56) 29/07/08
;changed event readback for indexed EVs
;Tx error interrupt disabled 5/8/08.  
;mods to error handling in interupt
;Tx error enabled 06/09/08

; corrected flow control bug in SendTx1, as per Slim version m 29/04/09
; version e - enhanced properties ?

;started changes for combined FLiM / SLiM  14/11/09
;incorporated ON only for SLiM. NN range now 64
;rev f.  Incorporated NV for setting individual inputs to ON / OFF or ON only 
;ACE8C_f  27/11/09. Added bootloader and other OPCs for full FLiM / SLiM version 
;ACE8C_g  28/11/09  Changed read of EVs so it is not in read mode  
;Mods to bootloader for LEDs and WDT  30/12/09
;ACE8C_h  27/01/10  Added NNACK for config
;Block to non supported Events in SLiM
;Mods to RTR and Unlearn  rev j
;Roger's mods to TXB2CON  rev k
;Added facility to trigger mode 1 events using the PB in SLiM mode. Rev m  (no rev l)02/03/10
;Prevent eror messages in unset. Rev n. (17/03/10)
;Rev p. new enum scheme  24/03/10  (no rev o)
;Rev q. Added clear of RXB overflow flags in COMSTAT
;Rev r. Added response to short events and also mode for individual event polling. 04/12/10
;   Modes 8 to 15 correspond to inputs 1 to 8
;   Short events taught with the mode set to 8 - 15 give a response matching inputs
;   1 to 8. Poll with event 0x9A using the lower two bytes for the 'device number'
;   Response is 0x93 (input ON) or 0x94 (input off). The two MSbytes of this response is the Node
;   Number, the two LSbytes is the 'device number'.
;Fix in 'route' routine for short mode events
;Changes to state sequence and route routines for short event requests and responses 12/12/10
;Rev s  Added short event output events if there is an allocated 'device number'. (Modes 8 to 15 set)19/01/11.
;Rev t  07/03/11 Only allow Boot with NN of zero. 
;   Allow read params by index in slim mode with NN of zero
;Rev u  Revert to allowing Boot and Read Params in SLiM mode with nodes NN
;Rev v  Corrected bug so now responds to a RTR in SLiM mode.
;Rev w  Now produces short events on a SOD if the inputs have a 'device number'.
;   same short events with a short poll (0x9A) to a device no.
;Rev x  Polling inputs (short mode) gives a response of 0x9D for ON and 0x9E for OFF (27/07/11)
;   Added WRACK to all EEPROM write sequences. Added error responses. (28/07/11)

;Rev 102_a  First version wrt CBUS Developers Guide
;     Add code to support 0x11 (RQMN)
;     Add code to return 8th parameter by index - Flags

; Rev 102c  remove fix for extended frames - test
; Rev 102d  set RXM0 in RXB0CON and RXB1CON
; Rev 102e  Change CAN initialisation code for extended frams bug

; Rev v2a First release build
; Rev v2b Change reply to QNN to OPC_PNN
; Rev v2c Allow QNN to work in SLiM mode
; Rev v2d Fix bug when reponding to SOD, Txld3 not cleared
;     leaving device ms byte in buffer for following
;     no-device inputs
; Rev v2e Add check for zero index reading parameters
; Rev v2f Correct error codes when reading events, fix bug in evsend
; Rev v2g Change parameters to new format
; Rev v2h Changes for new self-enum as subroutine. new OpCodes 0x5D and 0x75 for self enum. 
; Rev v2j Added optional input inversion and input delay controls (Phil Wheeler)
;       Corrected event initialisation (Phil Wheeler)
;     Corrected firmware ID (Phil Wheeler)
; Rev v2k 25-Jul13 Added optional push button toggle input mode (Phil Wheeler)
;     Single source file now builds CANACE8C and CANTOTI firmware
;     Requires FCU V1.4.7.9 or later to configure this mode.

; Push Button Input Toggle Mode (added by Phil Wheeler, July 2013)
; If enabled for an input, this allows a push button on the input to alternately generate
; ON and OFF events. The first event after a SOD will be ON. A SOD will NOT send events
; for toggle inputs, as this doesn't make sense. The push button toggle mode is processed
; after any input inversion or delay settings.

;end of comments for ACE8C

; This is the bootloader section

;*  Filename Boot2.asm  30/10/09

;*************************************************************** * * * * * * * * * * * * * * ;*
;*  CBUS bootloader

;*  Based on the Microchip botloader 'canio.asm' tho which full acknowledgement is made.
;*  Relevant information is contained in the Microchip Application note AN247

;*
;* Basic Operation:
;* The following is a CAN bootloader designed for PIC18F microcontrollers
;* with built-in CAN such as the PIC18F458. The bootloader is designed to
;* be simple, small, flexible, and portable.
;*
;
;
;*
;* Commands:
;* Put commands received from source (Master --> Slave)
;* The count (DLC) can vary.
;* XXXXXXXXXXX 0 0 8 XXXXXXXX XXXXXX00 ADDRL ADDRH ADDRU RESVD CTLBT SPCMD CPDTL CPDTH
;* XXXXXXXXXXX 0 0 8 XXXXXXXX XXXXXX01 DATA0 DATA1 DATA2 DATA3 DATA4 DATA5 DATA6 DATA7
;*


;*
;* ADDRL - Bits 0 to 7 of the memory pointer.
;* ADDRH - Bits 8 - 15 of the memory pointer.
;* ADDRU - Bits 16 - 23 of the memory pointer.
;* RESVD - Reserved for future use.
;* CTLBT - Control bits.
;* SPCMD - Special command.
;* CPDTL - Bits 0 - 7 of 2s complement checksum
;* CPDTH - Bits 8 - 15 of 2s complement checksum
;* DATAX - General data.
;*
;* Control bits:
;* MODE_WRT_UNLCK-Set this to allow write and erase operations to memory.
;* MODE_ERASE_ONLY-Set this to only erase Program Memory on a put command. Must be on 64-byte
;*  boundary.
;* MODE_AUTO_ERASE-Set this to automatically erase Program Memory while writing data.
;* MODE_AUTO_INC-Set this to automatically increment the pointer after writing.
;* MODE_ACK-Set this to generate an acknowledge after a 'put' (PG Mode only)
;*
;* Special Commands:
;* CMD_NOP      0x00  Do nothing
;* CMD_RESET    0x01  Issue a soft reset after setting last EEPROM data to 0x00
;* CMD_RST_CHKSM  0x02  Reset the checksum counter and verify
;* CMD_CHK_RUN    0x03  Add checksum to special data, if verify and zero checksum
;* CMD_BOOT_TEST  0x04  Just sends a message frame back to verify boot mode.

;*  Modified version of the Microchip code by M Bolton  30/10/09
;
; The user program must have the following vectors

; User code reset vector  0x0800
; User code HPINT vector  0x0808
; user code LPINT vector  0x0818

; Checksum is 16 bit addition of all programmable bytes.
; User sends 2s complement of addition at end of program in command 0x03 (16 bits only)

;**********************************************************************************

; 
; Assembly options
  LIST  P=18F2480,r=hex,N=75,C=120,T=ON

  include   "p18f2480.inc"
  include   "cbuslib/constants.inc"
  
  ;definitions  Change these to suit hardware.
  
S_PORT  equ PORTB ;setup switch
S_BIT equ 0 
LEARN   equ 4 ;learn switch in port A
UNLEARN equ 5 ;unlearn switch in port A

CMD_ON  equ 0x90  ;on event
CMD_OFF equ 0x91  ;off event
CMD_REQ equ 0x92
SCMD_ON equ 0x98
SCMD_OFF  equ 0x99
SCMD_REQ  equ 0x9A
OPC_PNN equ 0xB6    ; reply to QNN

EN_NUM  equ .32   ;number of allowed events
EV_NUM  equ 2   ;number of allowed EVs per event

MIN_TIME  equ 2     ;minimum time for input change (*10mS) + 1

MAN_NO      equ MANU_MERG    ;manufacturer number
MAJOR_VER   equ 2
MINOR_VER   equ "K"
    ifdef CANTOTI
MODULE_ID   equ MTYP_CANTOTI  ; id to identify a CANTOTI module
    else
MODULE_ID   equ MTYP_CANACE8C   ; id to identify a CANACE8C module
    endif
EVT_NUM     equ EN_NUM        ; Number of events
EVperEVT    equ EV_NUM        ; Event variables per event

NV_NUM      equ 7       ; Number of node variables
NODEFLGS    equ PF_COMBI + PF_BOOT
CPU_TYPE    equ P18F2480

Modstat equ 1   ;address in EEPROM


; definitions used by bootloader

#define MODE_SELF_VERIFY  ;Enable self verification of written data (undefine if not wanted)

#define HIGH_INT_VECT 0x0808  ;HP interrupt vector redirect. Change if target is different
#define LOW_INT_VECT  0x0818  ;LP interrupt vector redirect. Change if target is different.
#define RESET_VECT  0x0800  ;start of target
#define CAN_CD_BIT  RXB0EIDL,0  ;Received control / data select bit
#define CAN_PG_BIT  RXB0EIDL,1  ;Received PUT / GET bit
#define CANTX_CD_BIT  TXB0EIDL,0  ;Transmit control/data select bit
#define CAN_TXB0SIDH  B'10000000' ;Transmitted ID for target node
#define CAN_TXB0SIDL  B'00001000'
#define CAN_TXB0EIDH  B'00000000' ;
#define CAN_TXB0EIDL  B'00000100'
#define CAN_RXF0SIDH  B'00000000' ;Receive filter for target node
#define CAN_RXF0SIDL  B'00001000'
#define CAN_RXF0EIDH  B'00000000'
#define CAN_RXF0EIDL  B'00000111'
#define CAN_RXM0SIDH  B'11111111' ;Receive masks for target node
#define CAN_RXM0SIDL  B'11101011'
#define CAN_RXM0EIDH  B'11111111'
#define CAN_RXM0EIDL  B'11111000'
#define CAN_BRGCON1   B'00000011' ;CAN bit rate controls. As for other CBUS modules
#define CAN_BRGCON2   B'10011110'
#define CAN_BRGCON3   B'00000011'
#define CAN_CIOCON    B'00100000' ;CAN I/O control  

#ifndef EEADRH    
#define EEADRH  EEADR+ 1  
#endif      
#define TRUE  1 
#define FALSE 0 
#define WREG1 PRODH ; Alternate working register
#define WREG2 PRODL 
#define MODE_WRT_UNLCK  _bootCtlBits, 0 ; Unlock write and erase
#define MODE_ERASE_ONLY _bootCtlBits, 1 ; Erase without write
#define MODE_AUTO_ERASE _bootCtlBits, 2 ; Enable auto erase before write
#define MODE_AUTO_INC _bootCtlBits, 3 ; Enable auto inc the address
#define MODE_ACK    _bootCtlBits, 4 ; Acknowledge mode
#define ERR_VERIFY    _bootErrStat, 0 ; Failed to verify if set
#define CMD_NOP     0x00  
#define CMD_RESET   0x01  
#define CMD_RST_CHKSM 0x02  
#define CMD_CHK_RUN   0x03
#define CMD_BOOT_TEST   0x04  

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


;set config registers
  
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
; RAM addresses used by boot. can also be used by application.

  CBLOCK 0
  _bootCtlMem
  _bootAddrL    ; Address info
  _bootAddrH    
  _bootAddrU    
  _unused0    ;(Reserved)
  _bootCtlBits  ; Boot Mode Control bits
  _bootSpcCmd   ; Special boot commands
  _bootChkL   ; Chksum low byte fromPC
  _bootChkH   ; Chksum hi byte from PC    
  _bootCount    
  _bootChksmL   ; 16 bit checksum
  _bootChksmH   
  _bootErrStat  ;Error Status flags
  ENDC
  
  ; end of bootloader RAM


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
  TempINTCON
  CanID_tmp ;temp for CAN Node ID
  IDtemph   ;used in ID shuffle
  IDtempl
  NN_temph  ;node number in RAM
  NN_templ
  ;The WV_ variables MUST match the NV definitions and be in the same order
  WV_ononly ;Working Variable: Bitmapped "on-only" flags
  WV_invt   ;Working Variable: Bitmapped Input invert flags
  WV_dlyd   ;Working Variable: Bitmapped Delayed input flags
  WV_ontm   ;Working Variable: On Time (MIN_TIME..255)
  WV_oftm   ;Working Variable: Off Time (MIN_TIME..255)
  WV_push   ;Working Variable: Bitmapped "Push Button Toggle Input" flags
  WV_mode   ;Working Variable: Expanded Mode
  
  IDcount   ;used in self allocation of CAN ID.
  Datmode   ;flag for data waiting and other states
  Count   ;counter for loading
  Count1
  Count2
  Keepcnt   ;keep alive counter
  Latcount  ;latency counter

  Temp    ;temps
  Temp1
  InputX    ;Input from interrupt routine (0=Active)
  TogLast   ;Last toggle state
  InpVal    ;Logical Input State
  InpChg    ;Changed inputs
  Inbit
  Incount
  InputLast
  Atemp   ;port a temp value
  Dlc     ;data length
  Mode    ;used for Flim /SLiM
          ;the above variables must be in access space (00 to 5F)
        
  

  
  
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
  Cmdtemp   ;for command type in responses
  
  DNindex   ;holds number of allowed DNs
  Match   ;match flag
  DNcount   ;which DN matched?
  ENcount   ;which EN matched
  ENcount1  ;temp for count offset
  ENend   ;last  EN number
  ENtemp
  ENtemp1
  EVtemp    ;holds current EV
  EVtemp1 
  EVtemp2   ;holds current EV qualifier
  EVtemp3 
  Mask
  Shift
  Shift1
  SN_temp   ;temp for short EV
  
  
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

  Roll    ;rolling bit for enum
  In_roll   ;rolling bit for input sense
  
  Fsr_tmp1Le  ;temp store for FSR1
  Fsr_tmp1He 
  Enum0   ;bits for new enum scheme.
  Enum1
  Enum2
  Enum3
  Enum4
  Enum5
  Enum6
  Enum7
  Enum8
  Enum9
  Enum10
  Enum11
  Enum12
  Enum13
  
  ;add variables to suit

  ;lpint variables
  Iin_curr      ;Current inputs
  Iin_delta     ;Last delta
  Iin_count0      ;Counter for input 0
  Iin_count1      ;Counter for input 1
  Iin_count2      ;Counter for input 2
  Iin_count3      ;Counter for input 3
  Iin_count4      ;Counter for input 4
  Iin_count5      ;Counter for input 5
  Iin_count6      ;Counter for input 6
  Iin_count7      ;Counter for input 7

    
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

;****************************************************************
; This is the bootloader
; ***************************************************************************** 
;_STARTUPCODE 0x00
  ORG 0x0000
; *****************************************************************************
  bra _CANInit
  bra _StartWrite
; ***************************************************************************** 
;_INTV_H CODE 0x08
  ORG 0x0008
; *****************************************************************************

  goto  HIGH_INT_VECT

; ***************************************************************************** 
;_INTV_L CODE 0x18
  ORG 0x0018
; *****************************************************************************

  goto  LOW_INT_VECT 

; ************************************************************** 
; Code start
; **************************************************************
  ORG 0x0020
;_CAN_IO_MODULE CODE
; ************************************************************ ** * * * * * * * * * * * * * * * 
; Function: VOID _StartWrite(WREG _eecon_data)
;PreCondition: Nothing
;Input: _eecon_data
;Output: Nothing. Self write timing started.
;Side Effects: EECON1 is corrupted; WREG is corrupted.
;Stack Requirements: 1 level.
;Overview: Unlock and start the write or erase sequence to protected
; memory. Function will wait until write is finished.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_StartWrite
  movwf   EECON1
  btfss   MODE_WRT_UNLCK  ; Stop if write locked
  return
  movlw   0x55  ; Unlock
  movwf    EECON2 
  movlw  0xAA 
  movwf    EECON2
  bsf  EECON1, WR ; Start the write
  nop
  btfsc   EECON1, WR  ; Wait (depends on mem type)
  bra $ - 2
  return
; ************************************************************ ** * * * * * * * * * * * * * * *

; Function: _bootChksm _UpdateChksum(WREG _bootChksmL)
;
; PreCondition: Nothing
; Input: _bootChksmL
; Output: _bootChksm. This is a static 16 bit value stored in the Access Bank.
; Side Effects: STATUS register is corrupted.
; Stack Requirements: 1 level.
; Overview: This function adds a byte to the current 16 bit checksum
; count. WREG should contain the byte before being called.
;
; The _bootChksm value is considered a part of the special
; register set for bootloading. Thus it is not visible. ;
;*************************************************************** * * * * * * * * * * * *
_UpdateChksum:
  addwf _bootChksmL,  F ; Keep a checksum
  btfsc STATUS, C
  incf  _bootChksmH,  F
  return
;************************************************************ ** * * * * * * * * * * * * * * *
;
; Function: VOID _CANInit(CAN,  BOOT)
;
; PreCondition: Enter only after a reset has occurred.
; Input: CAN control information, bootloader control information ; Output: None.
; Side Effects: N/A. Only run immediately after reset.
; Stack Requirements: N/A
; Overview: This routine is technically not a function since it will not
; return when called. It has been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied. ;
; This routine tests the boot flags to determine if boot mode is
; desired or normal operation is desired. If boot mode then the
; routine initializes the CAN module defined by user input. It
; also resets some registers associated to bootloading.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_CANInit:
  clrf  EECON1
  setf  EEADR ; Point to last location of EEDATA
  setf  EEADRH
  bsf EECON1, RD  ; Read the control code
  incfsz EEDATA, W

  goto  RESET_VECT


  clrf  _bootSpcCmd   ; Reset the special command register
  movlw   0x1C    ; Reset the boot control bits
  movwf   _bootCtlBits 
  movlb d'15'   ; Set Bank 15
  bcf   TRISB, CANTX  ; Set the TX pin to output 
  movlw   CAN_RXF0SIDH  ; Set filter 0
  movwf   RXF0SIDH
  movlw   CAN_RXF0SIDL 
  movwf   RXF0SIDL
  comf  WREG    ; Prevent filter 1 from causing a receive event





  movwf RXF1SIDL  ;   
  movlw CAN_RXF0EIDH  
  movwf RXF0EIDH  
  movlw CAN_RXF0EIDL  
  movwf RXF0EIDL  
  movlw CAN_RXM0SIDH  ; Set mask
  movwf RXM0SIDH  
  movlw CAN_RXM0SIDL  
  movwf RXM0SIDL  
  movlw CAN_RXM0EIDH  
  movwf RXM0EIDH  
  movlw CAN_RXM0EIDL  
  movwf RXM0EIDL  
  movlw CAN_BRGCON1 ; Set bit rate
  movwf BRGCON1 
  movlw CAN_BRGCON2 
  movwf BRGCON2 
  movlw CAN_BRGCON3 
  movwf BRGCON3 
  movlw CAN_CIOCON  ; Set IO
  movwf CIOCON  
  
  clrf  CANCON  ; Enter Normal mode
  movlw B'00001110'
  movwf ADCON1
  bcf TRISB,7
  bcf TRISB,6
  bsf PORTB,7   ;gren LED on
  bsf PORTB,6   ;yellow LED on


; ************************************************************ ** * * * * * * * * * * * * * * * 
; This routine is essentially a polling loop that waits for a
; receive event from RXB0 of the CAN module. When data is
; received, FSR0 is set to point to the TX or RX buffer depending
; upon whether the request was a 'put' or a 'get'.
; ************************************************************ ** * * * * * * * * * * * * * * * 
_CANMain
  
  bcf RXB0CON, RXFUL  ; Clear the receive flag
_wait clrwdt      ; Clear WDT while waiting
  btfss   RXB0CON, RXFUL  ; Wait for a message  
  bra _wait



_CANMainJp1
  lfsr  0, RXB0D0
  movf  RXB0DLC, W 
  andlw   0x0F
  movwf   _bootCount 
  movwf   WREG1
  bz  _CANMain 
_CANMainJp2       ;?
  


; ************************************************************** * * * * * * * * * * * * * * * 
; Function: VOID _ReadWriteMemory()
;
; PreCondition:Enter only after _CANMain().
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: This routine is technically not a function since it will not
; return when called. It has been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;This is the memory I/O engine. A total of eight data bytes are received and decoded. In addition two control bits are received, put/get and control/data.
;A pointer to the buffer is passed via FSR0 for reading or writing. 
;The control register set contains a pointer, some control bits and special command registers.
;Control
;<PG><CD><ADDRL><ADDRH><ADDRU><_RES_><CTLBT>< SPCMD><CPDTL><CPDTH>
;Data
;<PG>< CD>< DATA0>< DATA1>< DATA2>< DATA3>< DATA4>< DATA5>< DATA6>< DATA7>
;PG bit:  Put = 0, Get = 1
;CD bit:  Control = 0, Data = 1

; ************************************************************ ** * * * * * * * * * * * * * * *
_ReadWriteMemory:
  btfsc CAN_CD_BIT  ; Write/read data or control registers
  bra _DataReg
; ************************************************************ ** * * * * * * * * * * * * * * * ; This routine reads or writes the bootloader control registers,
; then executes any immediate command received.
_ControlReg
  lfsr  1, _bootAddrL   ;_bootCtlMem
_ControlRegLp1

  movff   POSTINC0, POSTINC1 
  decfsz  WREG1, F
  bra _ControlRegLp1

; ********************************************************* 
; This is a no operation command.
  movf  _bootSpcCmd, W    ; NOP Command
  bz  _CANMain
; bz  _SpecialCmdJp2    ; or send an acknowledge

; ********************************************************* 
; This is the reset command.
  xorlw   CMD_RESET   ; RESET Command 
  btfss   STATUS, Z
  bra   _SpecialCmdJp4
  setf  EEADR   ; Point to last location of EEDATA
  setf  EEADRH
  clrf  EEDATA    ; and clear the data (FF for now)
  movlw   b'00000100' ; Setup for EEData
  rcall   _StartWrite
  bcf   PORTB,6   ;yellow LED off
  reset
; *********************************************************
; This is the Selfcheck reset command. This routine 
; resets the internal check registers, i.e. checksum and 
; self verify.
_SpecialCmdJp4
  movf  _bootSpcCmd, W 
  xorlw   CMD_RST_CHKSM
  bnz   _SpecialCmdJp1
  clrf  _bootChksmH
  clrf  _bootChksmL
  bcf   ERR_VERIFY    
  clrf  _bootErrStat
  bra   _CANMain
; RESET_CHKSM Command
; Reset chksum
; Clear the error verify flag

;This is the Test and Run command. The checksum is
; verified, and the self-write verification bit is checked. 
; If both pass, then the boot flag is cleared.
_SpecialCmdJp1
  movf  _bootSpcCmd, W    ; RUN_CHKSM Command
  xorlw   CMD_CHK_RUN 
  bnz _SpecialCmdJp3
  movf  _bootChkL, W  ; Add the control byte
  addwf  _bootChksmL, F
  bnz _SpecialCmdJp2
  movf  _bootChkH, W 
  addwfc  _bootChksmH, F
  bnz _SpecialCmdJp2
  btfsc   ERR_VERIFY    ; Look for verify errors
  bra _SpecialCmdJp2

  bra   _CANSendOK  ;send OK message


_SpecialCmdJp2

  bra _CANSendNOK ; or send an error acknowledge


_SpecialCmdJp3
  movf  _bootSpcCmd, W    ; RUN_CHKSM Command
  xorlw   CMD_BOOT_TEST 
  bnz _CANMain
  bra _CANSendBoot

; ************************************************************** * * * * * * * * * * * * * * * 
; This is a jump routine to branch to the appropriate memory access function.
; The high byte of the 24-bit pointer is used to determine which memory to access. 
; All program memories (including Config and User IDs) are directly mapped. 
; EEDATA is remapped.
_DataReg
; *********************************************************
_SetPointers
  movf  _bootAddrU, W ; Copy upper pointer
  movwf   TBLPTRU
  andlw   0xF0  ; Filter
  movwf   WREG2
  movf  _bootAddrH, W ; Copy the high pointer
  movwf   TBLPTRH
  movwf   EEADRH
  movf  _bootAddrL, W ; Copy the low pointer
  movwf   TBLPTRL
  movwf  EEADR
  btfss   MODE_AUTO_INC ; Adjust the pointer if auto inc is enabled
  bra _SetPointersJp1
  movf  _bootCount, W ; add the count to the pointer
  addwf  _bootAddrL, F 
  clrf  WREG
  addwfc   _bootAddrH, F 
  addwfc   _bootAddrU, F 

_SetPointersJp1     ;?

_Decode
  movlw   0x30
  cpfslt  WREG2
  bra _DecodeJp1



  bra _PMEraseWrite

_DecodeJp1
  movf  WREG2,W
  xorlw   0x30
  bnz _DecodeJp2



  bra _CFGWrite 
_DecodeJp2
  movf  WREG2,W 
  xorlw 0xF0
  bnz _CANMain
  bra _EEWrite

f 

; Program memory < 0x300000
; Config memory = 0x300000
; EEPROM data = 0xF00000
  
; ************************************************************ ** * 
; ************************************************************** * 
; Function: VOID _PMRead()
; VOID _PMEraseWrite ()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of
; the source data.
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;These are the program memory read/write functions. Erase is available through control flags. An automatic erase option is also available.
; A write lock indicator is in place to ensure intentional write operations.
;Note: write operations must be on 8-byte boundaries and must be 8 bytes long. Also erase operations can only occur on 64-byte boundaries.
; ************************************************************ ** * * * * * * * * * * * * * * *



_PMEraseWrite:
  btfss   MODE_AUTO_ERASE
  bra _PMWrite
_PMErase:
  movf  TBLPTRL, W
  andlw b'00111111'
  bnz _PMWrite
_PMEraseJp1
  movlw b'10010100' 
  rcall   _StartWrite 
_PMWrite:
  btfsc   MODE_ERASE_ONLY


  bra _CANMain 

  movf  TBLPTRL, W
  andlw b'00000111'
  bnz _CANMain 
  movlw   0x08
  movwf WREG1

_PMWriteLp1         ; Load the holding registers
  movf  POSTINC0, W 
  movwf   TABLAT
  rcall  _UpdateChksum  ; Adjust the checksum
  tblwt*+
  decfsz   WREG1, F
  bra _PMWriteLp1

#ifdef MODE_SELF_VERIFY 
  movlw  0x08
  movwf   WREG1 
_PMWriteLp2
  tblrd*-     ; Point back into the block
  movf  POSTDEC0, W 
  decfsz   WREG1, F
  bra _PMWriteLp2
  movlw  b'10000100'  ; Setup writes
  rcall _StartWrite   ; Write the data
  movlw   0x08
  movwf   WREG1
_PMReadBackLp1
  tblrd*+     ; Test the data
  movf  TABLAT, W 
  xorwf   POSTINC0, W
  btfss STATUS, Z
  bsf ERR_VERIFY 
  decfsz  WREG1, F
  bra _PMReadBackLp1  ; Not finished then repeat
#else
  tblrd*-     ; Point back into the block
         ; Setup writes
  movlw   b'10000100'   ; Write the data
  rcall   _StartWrite   ; Return the pointer position
  tblrd*+
#endif

  bra _CANMain


; ************************************************************** * * * * * * * * * * * * * * *
 ; Function: VOID _CFGWrite()
; VOID _CFGRead()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of the source data. 
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;
; These are the Config memory read/write functions. Read is
; actually the same for standard program memory, so any read
; request is passed directly to _PMRead.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_CFGWrite

#ifdef MODE_SELF_VERIFY   ; Write to config area
  movf  INDF0, W    ; Load data
#else
  movf  POSTINC0, W
#endif
  movwf   TABLAT
  rcall   _UpdateChksum ; Adjust the checksum
  tblwt*      ; Write the data
  movlw b'11000100' 
  rcall   _StartWrite
  tblrd*+     ; Move the pointers and verify
#ifdef MODE_SELF_VERIFY 
  movf  TABLAT, W 
  xorwf   POSTINC0, W

#endif
  decfsz  WREG1, F
  bra _CFGWrite ; Not finished then repeat

  bra _CANMain 



; ************************************************************** * * * * * * * * * * * * * * * 
; Function: VOID _EERead()
; VOID _EEWrite()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of
 ;  the source data.
; Input:  None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;
; This is the EEDATA memory read/write functions.
;
; ************************************************************ ** * * * * * * * * * * * * * * *


_EEWrite:

#ifdef MODE_SELF_VERIFY
  movf  INDF0, W
#else
  movf  POSTINC0, W 
#endif

  movwf   EEDATA
  rcall   _UpdateChksum 
  movlw b'00000100' 
  rcall  _StartWrite

#ifdef MODE_SELF_VERIFY 
  clrf  EECON1
  bsf EECON1, RD
  movf  EEDATA, W 
  xorwf   POSTINC0, W
  btfss STATUS, Z
  bsf ERR_VERIFY
#endif

  infsnz   EEADR, F 
  incf  EEADRH, F 
  decfsz  WREG1, F
  bra _EEWrite


  bra _CANMain 
  

; Read the data

; Adjust EEDATA pointer
; Not finished then repeat
; Load data
; Adjust the checksum 
; Setup for EEData
; and write
; Read back the data ; verify the data ; and adjust pointer
; Adjust EEDATA pointer
; Not finished then repeat

; ************************************************************** * * * * * * * * * * * * * * *
; Function: VOID _CANSendAck()
; VOID _CANSendResponce ()
;
; PreCondition:TXB0 must be preloaded with the data.
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied. ;
; These routines are used for 'talking back' to the source. The
; _CANSendAck routine sends an empty message to indicate
; acknowledgement of a memory write operation. The
; _CANSendResponce is used to send data back to the source. ;
; ************************************************************ ** * * * * * * * * * * * * * * *



_CANSendMessage
  btfsc   TXB0CON,TXREQ 
  bra $ - 2
  movlw   CAN_TXB0SIDH 
  movwf   TXB0SIDH
  movlw   CAN_TXB0SIDL 
  movwf   TXB0SIDL
  movlw   CAN_TXB0EIDH 
  movwf   TXB0EIDH  

  movlw CAN_TXB0EIDL
  movwf TXB0EIDL
  bsf CANTX_CD_BIT
  btfss CAN_CD_BIT 
  bcf CANTX_CD_BIT
  bsf TXB0CON, TXREQ
      bra  _CANMain ; Setup the command bit

_CANSendOK        ;send OK message 
  movlw 1     ;a 1 is OK
  movwf TXB0D0
  movwf TXB0DLC
  bra   _CANSendMessage
  
_CANSendNOK       ;send not OK message
  clrf  TXB0D0    ;a 0 is not OK
  movlw 1
  movwf TXB0DLC
  bra   _CANSendMessage

_CANSendBoot
  movlw 2     ;2 is confirm boot mode
  movwf TXB0D0
  movlw 1
  movwf TXB0DLC
  bra   _CANSendMessage
    
; Start the transmission

;   End of bootloader

;************************************************************************************
;
;   start of program code

    ORG   0800h
loadadr   
    nop           ;for debug
    goto  setup

    ORG   0808h
    goto  hpint     ;high priority interrupt
    
    ORG   0810h     
    ifdef CANTOTI
myName  db  "TOTI   "   
    else
myName  db  "ACE8C  "
    endif
    
    ORG   0818h 
    goto  lpint     ;low priority interrupt

    ORG   0820h

nodeprm     db  MAN_NO, MINOR_VER, MODULE_ID, EVT_NUM, EVperEVT, NV_NUM 
      db  MAJOR_VER,NODEFLGS,CPU_TYPE,PB_CAN    ; Main parameters
            dw  RESET_VECT     ; Load address for module code above bootloader
            dw  0           ; Top 2 bytes of 32 bit address not used
sparprm     fill 0,prmcnt-$ ; Unused parameter space set to zero

PRMCOUNT    equ sparprm-nodeprm ; Number of parameter bytes implemented

             ORG 0838h

prmcnt      dw  PRMCOUNT    ; Number of parameters implemented
nodenam     dw  myName      ; Pointer to module type name
            dw  0 ; Top 2 bytes of 32 bit address not used


PRCKSUM     equ MAN_NO+MINOR_VER+MODULE_ID+EVT_NUM+EVperEVT+NV_NUM+MAJOR_VER+NODEFLGS+CPU_TYPE+PB_CAN+HIGH myName+LOW myName+HIGH loadadr+LOW loadadr+PRMCOUNT

cksum       dw  PRCKSUM     ; Checksum of parameters

;*******************************************************************

    ORG   0840h     ;start of program
; 
;
;   high priority interrupt. Used for CAN receive and transmit error.

hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
;   movff PCLATH,PCH_tempH    ;save PCLATH

  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H
    
    

    movlw 8           ;for relocated code
    movwf PCLATH
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
    btfsc Datmode,1     ;setup mode?
    bra   setmode 
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
          
errbak    bcf   RXB1CON,RXFUL
    movlb 0
    bcf   RXB0CON,RXFUL ;ready for next
    
    bcf   COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf   COMSTAT,RXB1OVFL    
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
;   btfsc Datmode,1     ;setup mode?
;   bra   setmode 
    movf  Rx0dlc,F      ;ignore any zero data frames
    bz    back
;   btfss Rx0sidl,3   ;ignore extended frames
    bsf   Datmode,0   ;valid message frame  
    
back  bcf   RXB0CON,RXFUL ;ready for next
  
  
back1 clrf  PIR3      ;clear all flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
;   movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers
    
isRTR btfsc Datmode,1   ;setup mode?
    bra   back      ;back    
;   btfss Mode,1      ;FLiM?    corrected in rev v
;   bra   back
    movlb .15
isRTR1  btfsc TXB2CON,TXREQ ;wait till sent
    bra   isRTR1    
    bsf   TXB2CON,TXREQ ;send ID frame - preloaded in TXB2

    movlb 0
    bra   back

setmode tstfsz  RXB0DLC
    bra   back        ;only zero length frames for setup
    
    swapf RXB0SIDH,W      ;get ID into one byte
    rrcf  WREG
    andlw B'01111000'     ;mask
    movwf Temp
    swapf RXB0SIDL,W
    rrncf WREG
    andlw B'00000111'
    iorwf Temp,W
    movwf IDcount       ;has current incoming CAN_ID

    lfsr  FSR1,Enum0      ;set enum to table
enum_st clrf  Roll        ;start of enum sequence
    bsf   Roll,0
    movlw 8
enum_1  cpfsgt  IDcount
    bra   enum_2
    subwf IDcount,F     ;subtract 8
    incf  FSR1L       ;next table byte
    bra   enum_1
enum_2  dcfsnz  IDcount,F
    bra   enum_3
    rlncf Roll,F
    bra   enum_2
enum_3  movf  Roll,W
    iorwf INDF1,F

    
    bra   back


;**************************************************************
; low priority interrupt, called every 10mS
; This scans and conditions the inputs, storing the conditioned
; inputs in InputX for the foreground routine to send as CBUS events

lpint movwf W_tempL       ;Save registers
    movff STATUS,St_tempL
    movff BSR,Bsr_tempL

;   movff PCLATH,PCH_tempL  ;save PCLATH
;   clrf  PCLATH

    movlw 0x78        ;Timer 1 lo byte. (adjust if needed)
    movwf TMR1L       ;reset timer 1
    clrf  PIR1        ;clear all timer flags

;   btg   PORTB,7       ;toggle green LED for now
    movf  PORTC,W       ;read inputs
    xorwf WV_invt,W     ;invert as required
    xorwf Iin_curr,W      ;compare with current bits
    bz    lpchk0        ;No change, just check timeouts

; One or more inputs in W have changed

    movwf Iin_delta     ;save changed bits
    xorwf Iin_curr      ;save current bits

; Check input 0

    btfss Iin_delta,0     ;Skip if input changed
    bra   lpint1
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,0      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,0     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count0      ;Set counter

; Check input 1       

lpint1  btfss Iin_delta,1     ;Skip if input changed
    bra   lpint2
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,1      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,1     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count1      ;Set counter

; Check input 2       

lpint2  btfss Iin_delta,2     ;Skip if input changed
    bra   lpint3
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,2      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,2     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count2      ;Set counter

; Check input 3       

lpint3  btfss Iin_delta,3     ;Skip if input changed
    bra   lpint4
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,3      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,3     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count3      ;Set counter

; Check input 4       

lpint4  btfss Iin_delta,4     ;Skip if input changed
    bra   lpint5
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,4      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,4     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count4      ;Set counter

; Check input 5       

lpint5  btfss Iin_delta,5     ;Skip if input changed
    bra   lpint6
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,5      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,5     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count5      ;Set counter

; Check input 6       

lpint6  btfss Iin_delta,6     ;Skip if input changed
    bra   lpint7
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,6      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,6     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count6      ;Set counter

; Check input 7       

lpint7  btfss Iin_delta,7     ;Skip if input changed
    bra   lpint8
    movf  WV_ontm,W     ;Get ON time
    btfsc Iin_curr,7      ;Skip if input ON
    movf  WV_oftm,W     ;Get OFF time
    btfss WV_dlyd,7     ;Skip if delay used for this input
    movlw MIN_TIME      ;No delay, set default count
    movwf Iin_count7      ;Set counter

lpint8

; Check for timeouts

lpchk0  movf  Iin_count0,W    ;Get counter
    bz    lpchk1        ;Zero, nothing to do
    decfsz  Iin_count0      ;Decrement counter, skip if zero
    bra   lpchk1        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,0
    btfsc Iin_curr,0      ;Skip if input inactive
    bsf   InputX,0
    
lpchk1  movf  Iin_count1,W    ;Get counter
    bz    lpchk2        ;Zero, nothing to do
    decfsz  Iin_count1      ;Decrement counter, skip if zero
    bra   lpchk2        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,1
    btfsc Iin_curr,1      ;Skip if input inactive
    bsf   InputX,1
      
lpchk2  movf  Iin_count2,W    ;Get counter
    bz    lpchk3        ;Zero, nothing to do
    decfsz  Iin_count2      ;Decrement counter, skip if zero
    bra   lpchk3        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,2
    btfsc Iin_curr,2      ;Skip if input inactive
    bsf   InputX,2
      
lpchk3  movf  Iin_count3,W    ;Get counter
    bz    lpchk4        ;Zero, nothing to do
    decfsz  Iin_count3      ;Decrement counter, skip if zero
    bra   lpchk4        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,3
    btfsc Iin_curr,3      ;Skip if input inactive
    bsf   InputX,3
      
lpchk4  movf  Iin_count4,W    ;Get counter
    bz    lpchk5        ;Zero, nothing to do
    decfsz  Iin_count4      ;Decrement counter, skip if zero
    bra   lpchk5        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,4
    btfsc Iin_curr,4      ;Skip if input inactive
    bsf   InputX,4
      
lpchk5  movf  Iin_count5,W    ;Get counter
    bz    lpchk6        ;Zero, nothing to do
    decfsz  Iin_count5      ;Decrement counter, skip if zero
    bra   lpchk6        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,5
    btfsc Iin_curr,5      ;Skip if input inactive
    bsf   InputX,5
      
lpchk6  movf  Iin_count6,W    ;Get counter
    bz    lpchk7        ;Zero, nothing to do
    decfsz  Iin_count6      ;Decrement counter, skip if zero
    bra   lpchk7        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,6
    btfsc Iin_curr,6      ;Skip if input inactive
    bsf   InputX,6
      
lpchk7  movf  Iin_count7,W    ;Get counter
    bz    lpchk8        ;Zero, nothing to do
    decfsz  Iin_count7      ;Decrement counter, skip if zero
    bra   lpchk8        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputX,7
    btfsc Iin_curr,7      ;Skip if input inactive
    bsf   InputX,7
      
lpchk8

lpend movff Bsr_tempL,BSR   ;End of low priority interrupt
    movf  W_tempL,W
    movff St_tempL,STATUS 
    retfie  

;*********************************************************************
; main waiting loop

main  btfsc Mode,1      ;is it SLiM?
    bra   mainf     ;no

mains             ;is SLiM

    btfss PIR2,TMR3IF   ;flash timer overflow?
    bra   nofl_s      ;no SLiM flash
    btg   PORTB,7     ;toggle green LED
    bcf   PIR2,TMR3IF
nofl_s  bra   noflash       ;main1
    
; here if FLiM mde

mainf btfss INTCON,TMR0IF   ;is it flash?
    bra   noflash
    btfss Datmode,2
    bra   nofl1
    
    btg   PORTB,6     ;flash yellow LED
    
nofl1 bcf   INTCON,TMR0IF
    btfss Datmode,3   ;running mode
    bra   noflash
    decfsz  Keepcnt     ;send keep alive?
    bra   noflash
    movlw .10
    movwf Keepcnt
    movlw 0x52
;   call  nnrel     ;send keep alive frame (works OK, turn off for now)

noflash btfsc S_PORT,S_BIT  ;setup button?
    bra   main3
    movlw .100
    movwf Count
    clrf  Count1
    clrf  Count2
wait  decfsz  Count2
    goto  wait
    btfss Datmode,2
    bra   wait2
    btfss INTCON,TMR0IF   ;is it flash?
    bra   wait2
    btg   PORTB,6     ;flash LED
    bcf   INTCON,TMR0IF
wait2 decfsz  Count1
    goto  wait
    btfsc S_PORT,S_BIT
    bra   main4     ;not held long enough
    decfsz  Count
    goto  wait
    btfss Mode,1      ;is it in FLiM?
    bra   go_FLiM
    clrf  Datmode     ;back to virgin
;   bcf   Mode,1      ;SLiM mode
    bcf   PORTB,6     ;yellow off
    
    bsf   PORTB,7     ;Green LED on
    clrf  INTCON      ;interrupts off
    movlw 1
    movwf IDcount     ;back to start
    movlw Modstat
    movwf EEADR
    movlw   0
    call  eewrite     ;status to reset
    movlw 0x51      ;send node release frame
    call  nnrel
    clrf  NN_temph
    clrf  NN_templ
wait1 btfss S_PORT,S_BIT
    bra   wait1     ;wait till release
    call  ldely
    btfss S_PORT,S_BIT
    bra   wait1
  
    
    movlw LOW NodeID      ;put NN back to 0000
    movwf EEADR
    movlw 0
    call  eewrite
    incf  EEADR
    movlw 0
    call  eewrite 
    btfss Mode,1
    bra   main5       ;FLiM setup
    movlw Modstat
    movwf EEADR
    movlw 0
    call  eewrite       ;mode back to SLiM
    clrf  Datmode
    bcf   Mode,1
    bcf   PORTB,6
    bsf   PORTB,7       ;green LED on
  
    movlw B'11000000'
    movwf INTCON
    goto  main        ;

main5 movlw Modstat
    movwf EEADR
    movlw 1
    call  eewrite       ;mode to FLiM in EEPROM
    bsf   Mode,1        ;to FLiM
    call  self_en       ;self enumerate routine
    bcf   Datmode,1
    call  nnack       ;send request for NN
    bsf   Datmode,2
;   movlw Modstat       ;only if needed
;   movwf EEADR
;   movlw B'00000100'
;   call  eewrite       ;mode to wait for NN in EEPROM
    bra   main1


main4 ;btfss  Datmode,3   
    ;bra    main3
    btfss Datmode,2
    bra   mset2
    bcf   Datmode,2
    bsf   PORTB,6     ;LED on
    movlw 0x52
    call  nnrel
    movlw Modstat
    movwf EEADR
    movlw B'00001000'
    movwf Datmode     ;normal
    call  eewrite
    bra   main3
    
mset2 bsf   Datmode,2
    call  self_en
    bcf   Datmode,1
    call  nnack
    bra   main1

main3 btfss Datmode,1   ;setup mode ?
    bra   main1
;   call  self_en

    bcf   Datmode,1   ;out of setup
    bsf   Datmode,2   ;wait for NN
;   call  nnack     ;send blank NN for config
  
;   
;   bsf   PORTB,7     ;on light
    bra   main1     ;continue normally

go_FLiM bsf   Datmode,1   ;FLiM setup mode
    bcf   PORTB,7     ;green off
    bra   wait1
    
    

; common to FLiM and SLiM   
  
  
main1 
    btfsc Datmode,0   ;any new CAN frame received?
    bra   packet      ;yes
    bra   do        ;look for inputs

;********************************************************************

;   These are here as branch was too long

go_on_x goto  go_on

;********************************************************

unset ;bsf    Datmode,5   ;unlearn this event
    ;bra    go_on
    btfss   Datmode,4
    bra   main2       ;prevent error messages on OPC 0x95
    bsf   Datmode,5
    bra   learn1

readEV  btfss Datmode,4
    bra   main2     ;prevent error message
    bsf   Datmode,6   ;read back an EV
    bra   learn1

evns1 call  thisNN        ;read event numbers
    sublw 0
    bnz   notNNx
    call  evns2
    bra   main2
;evns3  goto  notNN

sendNN  btfss Datmode,2   ;in NN set mode?
    bra   main2     ;no
    movlw 0x50      ;send back NN
    movwf Tx1d0
    movlw 3
    movwf Dlc
    call  sendTX
    bra   main2

reval call  thisNN        ;read event numbers
    sublw 0
    bnz   notNNx
    call  evsend
    bra   main2

name
    btfss Datmode,2   ;only in setup mode
    bra   main2
    call  namesend
    bra   main2

doQnn
    movf  NN_temph,w    ;respond if NN is not zero
    addwf NN_templ,w
    btfss STATUS,Z
    call  whoami
    bra   main2
              
short clrf  Rx0d1
    clrf  Rx0d2
    bra   go_on

notNNx  goto  notNN

paramsx goto  params

;************************************************************

                ;main packet handling is here
    
packet  movlw CMD_ON  ;only ON and REQ events supported
    subwf Rx0d0,W ;now add FLiM commands as well
    bz    go_on_x
    
    movlw CMD_REQ
    subwf Rx0d0,W
    bz    go_on_x
    movlw SCMD_ON
    subwf Rx0d0,W
    bz    short
  
    movlw SCMD_REQ
    subwf Rx0d0,W
    bz    short
    movlw 0x5C      ;reboot
    subwf Rx0d0,W
    bz    reboot
    movlw 0x73
    subwf Rx0d0,W
    bz    para1a      ;read individual parameters
    movlw 0x0d      ; QNN
    subwf Rx0d0,w
    bz    doQnn
    btfss Mode,1      ;FLiM?
    bra   main2
    movlw 0x42      ;set NN on 0x42
    subwf Rx0d0,W
    bz    setNN
    movlw 0x10      ;read manufacturer
    subwf Rx0d0,W
    bz    paramsx     ;read node parameters
    movlw 0x11
    subwf Rx0d0,w
    bz    name      ;read module name   
    movlw 0x53      ;set to learn mode on 0x53
    subwf Rx0d0,W
    bz    setlrn    
    movlw 0x54      ;clear learn mode on 0x54
    subwf Rx0d0,W
    bz    notlrn
    movlw 0x55      ;clear all events on 0x55
    subwf Rx0d0,W
    bz    clrens
    movlw 0x56      ;read number of events left
    subwf Rx0d0,W
    bz    rden
    movlw 0xD2      ;is it set event?
    subwf Rx0d0,W
    bz    chklrn      ;do it
    movlw 0x95      ;is it unset event
    subwf Rx0d0,W     
    bz    unset
    movlw 0xB2      ;read event variables
    subwf Rx0d0,W
    bz    readEV
  
    movlw 0x71      ;read NVs
    subwf Rx0d0,W
    bz    readNV1
    movlw 0x96      ;set NV
    subwf Rx0d0,W
    bz    setNV
    movlw 0x57      ;is it read events
    subwf Rx0d0,W
    bz    readEN1
    movlw 0x72
    subwf Rx0d0,W
    bz    readENi     ;read event by index
    movlw 0x58
    subwf Rx0d0,W
    bz    evns
    movlw 0x9C      ;read event variables by EN#
    subwf Rx0d0,W
    bz    reval
    movlw 0x5D      ;re-enumerate
    subwf Rx0d0,W
    bz    enum1
    movlw 0x75      ;force new CAN_ID
    subwf Rx0d0,W
    bz    newID1
    bra   main2

evns  goto  evns1
newID1  goto  newID
enum1 goto  enum
readNV1 goto  readNV
readEN1 goto  readEN

reboot
    call  thisNN
    sublw 0
    bnz   notNN
reboot1 movlw 0xFF
    movwf EEADR
    movlw 0xFF
    call  eewrite     ;set last EEPROM byte to 0xFF
    reset         ;software reset to bootloader
        
main2 bcf   Datmode,0
    goto  main      ;loop
    
para1a  
    call  thisNN      ;read parameter by index
    sublw 0
    bnz   notNN
    call  para1rd
    bra   main2

newID call  thisNN
    sublw 0
    bnz   notNN
    movff Rx0d3,IDcount

    call  here2       ;put in as if it was enumerated
    movlw 0x52
    call  nnrel       ;acknowledge new CAN_ID
    goto  main2
    
setNN btfss Datmode,2   ;in NN set mode?
    bra   main2     ;no
    call  putNN     ;put in NN
    bcf   Datmode,2
    bsf   Datmode,3
    bcf   PORTB,7     ;green LED off
    movlw .10
    movwf Keepcnt     ;for keep alive
    movlw 0x52
    call  nnrel     ;confirm NN set
    bsf   PORTB,6     ;LED ON
    bra   main2
    
rden  goto  rden1
    
setlrn  call  thisNN
    sublw 0
    bnz   notNN
    bsf   Datmode,4
;   bsf   PORTB,6     ;LED on
    bra   main2

notlrn  call  thisNN
    sublw 0
    bnz   notNN
    bcf   Datmode,4
notln1    ;leave in learn mode
    bcf   Datmode,5
;   bcf   PORTB,6
    bra   main2
clrens  call  thisNN
    sublw 0
    bnz   notNN
    call  enclear
notNN bra   main2

clrerr  movlw 2     ;not in learn mode
    goto  errmsg
  
go_on btfss Mode,1      ;FLiM?
    bra   go_on_s     ; j if SLiM
  
go_on2  ;movlw  0x90      ;is it an ON or request event?
    ;subwf  Rx0d0,W     ;only here if it is.
    ;bnz  main2
go_on1  call  enmatch
    sublw 0
    bz    do_it
    bra   main2     ;not here
    
go_on_s btfss PORTA,LEARN
    bra   learn1      ;is in learn mode
    bra   go_on1

chklrn  btfsc Datmode,4
    bra   learn1      ;is in learn mode
;   movlw 2       ;errror not in learn mode
;   call  errsub
    bra   main2 

readENi call  thisNN      ;read event by index
    sublw 0
    bnz   notNN
    call  enrdi
    bra   main2

params  btfsc Datmode,2   ;only in setup mode
    bra   para1b
;   movlw 3
;   call  errsub
    bra   main2
para1b  call  parasend
    bra   main2
    
setNV call  thisNN
    sublw 0
    bnz   notNN     ;not this node
    call  putNV
    call  nvcopy      ;Copy NV's to RAM
    bra   main2

readNV  call  thisNN
    sublw 0
    bnz   notNN     ;not this node
    call  getNV
    bra   main2

readEN  call  thisNN
    sublw 0
    bnz   notNN
    call  enread
    bra   main2
do_it 
    call  ev_set      ;do it
    bra   main2
    
rden1 call  thisNN
    sublw 0
    bnz   notNN
    movlw LOW ENindex+1   ;read number of events available
    movwf EEADR
    call  eeread
    sublw EN_NUM
    movwf Tx1d3
    movlw 0x70
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    bra   main2

learn1  btfss Mode,1      ;FLiM?
    bra   learn2
    movlw 0xD2
    subwf Rx0d0,W     ;is it a learn command
    bz    learn2      ;OK
    movlw 0x95      ;is it unlearn
    subwf Rx0d0,W
    bz    learn2
    movlw 0xB2
    subwf Rx0d0
    bz    learn2
    bra   l_out2    
    
learn2  call  enmatch     ;is it there already?
    sublw   0
    bz    isthere
    btfsc Mode,1      ;FLiM?
    bra   learn3
    btfss PORTA,UNLEARN ;if unset and not here
    bra   l_out2      ;do nothing else 
    call  learnin     ;put EN into stack and RAM
    sublw 0
    bz    new_EV
    bra   l_out2      ;too many
    
learn3  btfsc Datmode,6   ;read EV?
    bra   rdbak1      ;not here
    btfsc Datmode,5   ;if unset and not here
    bra   l_out1      ;do nothing else 
learn4  call  learnin     ;put EN into stack and RAM
    sublw 0
    bz    new_EV
    movlw 4
    call  errsub
    bra   l_out1      ;too many
isthere btfsc Mode,1
    bra   isth1
    btfss PORTA,UNLEARN ;is it here and unlearn,goto unlearn
    bra   unlearn     ;else modify EVs

isth1 btfss Datmode,5   ;FLiM unlearn?
    bra   mod_EV
    bra   unlearn

enum  call  thisNN
    sublw 0
    bnz   notNN1
    call  self_en
    movlw 0x52
    call  nnrel     ;send confirm frame
    movlw B'00001000'   ;back to normal running
    movwf Datmode
    goto  main2
notNN1  goto  notNN
  


rdbak movff EVtemp,Tx1d5    ;Index for readout  
    incf  Tx1d5,F       ;add one back 
    bsf   EECON1,RD     ;address set already
    movff EEDATA,Tx1d6
    bra   shift4
rdbak1  clrf  Tx1d5       ;no match
    clrf  Tx1d6
  

shift4  movlw 0xD3        ;readback of EVs
    movwf Tx1d0
    movff Rx0d1,Tx1d1
    movff Rx0d2,Tx1d2
    movff Rx0d3,Tx1d3
    movff Rx0d4,Tx1d4
    movlw 7
    movwf Dlc
    call  sendTXa 
    bra   l_out1

new_EV  btfsc Mode,1        ;FLiM?
    bra   new_EVf     ;not relevant if FLiM
    movlw LOW ENindex+1   ;here if a new event
    movwf EEADR
    bsf   EECON1,RD
    decf  EEDATA,W
    movwf ENcount       ;recover EN counter

mod_EV  btfsc Mode,1        ;FLiM?
    bra   mod_EVf       ;not relevant if FLiM
    rlncf ENcount,W     ;two byte values
    addlw LOW EVstart     ;point to EV
    movwf EEADR
    bsf   EECON1,RD
    call  getop       ;get switch. value in EVtemp
    movf  EVtemp,W
        
    
    call  eewrite       ;put back EV value  
    
shft3 bra   l_out2

new_EVf movlw LOW ENindex+1   ;here if a new event in FLiM mode
    movwf EEADR
    bsf   EECON1,RD
    decf  EEDATA,W
    movwf ENcount       ;recover EN counter
mod_EVf movff Rx0d5,EVtemp  ;store EV index
    movf  EVtemp,F    ;is it zero?
    bz    noEV
    decf  EVtemp,F    ;decrement. EVs start at 1
    movlw EV_NUM
    cpfslt  EVtemp
    bra   noEV    
;   btfsc Datmode,5   ;is it here and unlearn,goto unlearn
;   bra   unlearn     ;else modify EVs
    
    movff Rx0d6,EVtemp2 ;store EV
    
    
    rlncf ENcount,W     ;two byte values
    addlw LOW EVstart     ;point to EV
    movwf EEADR
    movf  EVtemp,W      ;add index to EEPROM value
    addwf EEADR,F
    btfsc Datmode,6     ;is it readback
    bra   rdbak
    movf  EVtemp2,W
    call  eewrite       ;put in
    call  wrack       ;write acknowledge
    bra   l_out2


      

l_out bcf   Datmode,4
;   bcf   LED_PORT,LED2
l_out1  bcf   Datmode,6
l_out2  bcf   Datmode,0
    

    clrf  PCLATH
    goto  main2
    
noEV  clrf  Tx1d5     ;invalid EV index
    clrf  Tx1d6
    movlw 6
    call  errsub
    bra   shift4      ;send with blank EV data


    
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
    btfsc Mode,1
    bra   un3
    bcf   T3CON,TMR3ON  ;flash timer off
    bcf   PIR2,TMR3IF
    bcf   PORTB,7
un3   bcf   Datmode,5
    call  wrack
    bra   l_out
        

  
do    btfss Mode,1      ;in FLiM?
    bra   do2       ;no
    btfss Datmode,3   ;ignore if not set up
    bra   do1
    btfsc Datmode,2   ;don't do if in setup   
    bra   do1
do2   call  scan      ;scan inputs for change
    
                
do1   goto  main
  
  
  
    

        
    
    
    
    
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
    movlw B'00111011'   ;RB0 is setup PB, RB1, RB4 and RB5 are bits 5 to 7 of ID. 
            ;RB2 = CANTX, RB3 = CANRX, 
            ;RB6,7 for debug and ICSP and diagnostics
    movwf TRISB
    bcf   PORTB,6
    bcf   PORTB,7
    bsf   PORTB,2     ;CAN recessive
    movlw B'11111111'   ;Port C  is the 8 switch inputs
    movwf TRISC
    
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
    
;new code for extended frames bug fix
    movlb .15
    movlw B'00100000'   ;reject extended frames
    movwf RXB1CON
    clrf  RXF0SIDL
    clrf  RXF1SIDL
    movlb 0       


    
mskload lfsr  0,RXM0SIDH    ;Clear masks, point to start
mskloop clrf  POSTINC0    
    movlw LOW RXM1EIDL+1    ;end of masks
    cpfseq  FSR0L
    bra   mskloop
;old code
;   movlb .15       ;block extended frames
;   bcf   RXF1SIDL,3    ;standard frames
;   bcf   RXF0SIDL,3    ;standard frames
;   bcf   RXB0CON,RXM1  ;frame type set by RXFnSIDL
;   bsf   RXB0CON,RXM0
;   bcf   RXB1CON,RXM1
;   bsf   RXB1CON,RXM0
;   movlb 0

    clrf  CANCON      ;out of CAN setup mode
    clrf  CCP1CON
    movlw B'10000100'
    movwf T0CON     ;set T0 for LED flash
    movlw B'10000001'   ;Timer 1 control.16 bit write
    movwf T1CON     ;Timer 1 is for output duration
    movlw 0x63
    movwf TMR1H     ;set timer hi byte

    clrf  Tx1con
    movlw B'00100011'
    movwf IPR3      ;high priority CAN RX and Tx error interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2
    movlw B'00000001'
    movwf PIE1      ;enable interrupt for timer 1

;next segment required
    
    movlw B'00000001'
    movwf IDcount     ;set at lowest value for starters
    
    clrf  INTCON2     ;
    clrf  INTCON3     ;
    

    movlw B'00100011'   ;B'00100011'  Rx0 and RX1 interrupt and Tx error
                
    movwf PIE3
  
    clrf  PIR1
    clrf  PIR2
    movlb .15
    bcf   RXB1CON,RXFUL
    movlb 0
    bcf   RXB0CON,RXFUL   ;ready for next
    bcf   COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf   COMSTAT,RXB1OVFL
    clrf  PIR3      ;clear all flags
    
    call  nvcopy      ;Copy NV's to RAM
    movf  PORTC,W     ;Read inputs
    xorwf WV_invt,W   ;invert as required
    movwf InputLast   ;initial input positions
    movf  WV_push,W   ;Get push button toggle inputs
    comf  WREG      ;Invert
    andwf InputLast,W   ;Get inputs
    movwf InputLast   ;and save
    movwf InpVal
    movwf Iin_curr
    clrf  TogLast     ;Clear any toggle states
    
    ;   test for setup mode
    clrf  Mode
    movlw Modstat     ;get setup status
    movwf EEADR
    call  eeread
    movwf Datmode
    sublw 0       ;is SLiM mode
    bnz   setid
    bra   slimset     ;set up in SLiM mode
  
    
setid bsf   Mode,1      ;flag FLiM
    call  newid_f     ;put ID into Tx1buf, TXB2 and ID number store
    
  
seten_f call  en_ram      ;put events in RAM
    call  nvcopy      ;Copy NV's to RAM
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,7
    bsf   PORTB,6     ;RUN LED on. (yellow for FLiM)
    bcf   Datmode,0
    goto  main

slimset movlw B'00001111'   ;get DIP switch setting
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
    bcf   Mode,0
    btfss PORTB,5     ;is it ON only?
    bsf   Mode,0      ;flag ON only
    bcf   Mode,1      ;not FLiM
  
    
    call  newid1      ;put ID into Tx1buf, TXB2 and ID number store
    
    ;test for clear all events
    btfss PORTA,LEARN   ;ignore the clear if learn is set
    goto  seten
    btfss PORTA,UNLEARN
    call  enclear     ;clear all events if unlearn is set during power up
seten call  en_ram      ;put events in RAM
  
  
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,6
    bsf   PORTB,7     ;RUN LED on. Green for SLiM
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

    
;*********************************************************************
;   put in NN from command

putNN movff Rx0d1,NN_temph
    movff Rx0d2,NN_templ
    movlw LOW NodeID
    movwf EEADR
    movf  Rx0d1,W
    call  eewrite
    incf  EEADR
    movf  Rx0d2,W
    call  eewrite
    movlw Modstat
    movwf EEADR
    movlw B'00001001'   ;Module status has NN set
    call  eewrite
    return  




    

newid1  movwf CanID_tmp   ;put in stored ID SLiM mode 
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
new_1 btfsc TXB2CON,TXREQ ;wait till sent
    bra   new_1
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

newid_f   movlw LOW CANid     ;put in stored ID. FLiM mode
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf CanID_tmp     
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlw LOW NodeID
    movwf EEADR
    call  eeread
    movwf NN_temph      ;get stored NN
    incf  EEADR
    call  eeread
    movwf NN_templ  
    
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
    bcf   TXB2CON,TXREQ
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
    
nnack movlw 0x50      ;request frame for new NN or ack if not virgin
nnrel movwf Tx1d0
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw 3
    movwf Dlc
    call  sendTX
    return

wrack movlw 0x59
    bra   nnrel

    
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

;***************************************************************************
;   Copies the NV's from EEPROM/Flash to working copies in RAM

nvcopy  movlw LOW NVstart
    movwf EEADR
    lfsr  FSR1,WV_ononly
nvloop  bsf   EECON1,RD
    movff EEDATA,POSTINC1
    incf  EEADR
    movlw LOW NVstart+NV_NUM
    cpfseq  EEADR
    bra   nvloop

;We need to add 1 to the on/off times due to the way the timers count

    infsnz  WV_ontm     ;Adjust time, skip if NZ
    decf  WV_ontm     ;Keep at maximum

    infsnz  WV_oftm     ;Adjust time, skip if NZ
    decf  WV_oftm     ;Keep at maximum

;check that the on & off delay isn't too short

    movlw MIN_TIME    ;Get minimum time 
    cpfsgt  WV_ontm     ;Skip if time OK
    movwf WV_ontm     ;Save minimum time
    cpfsgt  WV_oftm     ;Skip if time OK
    movwf WV_oftm     ;Save minimum time

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
    movff INTCON,TempINTCON
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
  
    movff TempINTCON,INTCON   ;reenable interrupts
    
    return  
    
;***************************************************************
; InputX is the conditioned inputs set by the 10mS timer interrupt
; Generate appropriate events for CBUS on any changes

scan  movf  InputX,W    ;Get inputs to W
    xorwf InputLast,W   ;compare with last inputs
    bz    end_scan      ;nothing changed
    movwf InpChg        ;save changed bits
    xorwf InputLast     ;update last bits
    
;***************************************************************
; If we've got here, one or more conditioned inputs have changed
; Updated inputs are in InputX, changed bits set in InpChg
; Check for any toggle modes
;   InputX  InpChg  WV_push ->  InpVal  InpChg  TogLast
;   x   x   0     InputX  Same  Same
;   0   0   1     TogLast Same  Same
;   0   1   1     TogLast Same  Toggle
;   1   0   1     TogLast Same  Same
;   1   1   1     TogLast 0   Same

; TogLast ^= (~InputX & WV_push & InpChg)
    movf  InputX,W
    comf  WREG
    andwf WV_push,W
    andwf InpChg,W
    xorwf TogLast
    
; InpChg &= ~(InputX & WV_push & InpChg)
    movf  InputX,W
    andwf WV_push,W
    andwf InpChg,W
    comf  WREG
    andwf InpChg

; InpVal = (~TogLast & WV_push)
    movf  TogLast,W
    comf  WREG
    andwf WV_push,W
    movwf InpVal
  
; InpVal |= (InputX & ~WV_push)
    movf  WV_push,W
    comf  WREG
    andwf InputX,W
    iorwf InpVal

; Now process code as normal

    clrf  Incount
    clrf  Inbit
    bsf   Inbit,0     ;rolling bit
change1 bcf   STATUS,C
    rrcf  InpChg,F
    bc    this
    incf  Incount,F
    rlcf  Inbit,F     ;added by Roger
    bc    end_scan
    bra   change1
this  btfss Mode,1      ;is it FLiM?
    bra   this3     ;no
    bcf   Mode,0      ;clear ON only bit
    btfsc Mode,2      ;mods by Brian W
    bra   bri1
    call  nvcopy      ;Copy NV's to RAM
    bsf   Mode,2    
bri1  movf  Inbit,W
    andwf WV_ononly,W   ;check if ononly NV bit is set
    bz    this3     ;no so leave Mode alone
    bsf   Mode,0      ;yes so set for no OFF
this3 call  ev_match    ;is it a device numbered switch?
    sublw 0
    bz    dn_out      ;yes so send as short event with DN
    movff Incount,Tx1d4 ;EN number lo byte
    incf  Tx1d4     ;ENs start at 1
    movlw B'10010000'   ;Command byte (temp for now = 90h)
    movwf Tx1d0     ;set up event
    
    movf  InpVal,W
    andwf Inbit,W     ;what is the new state
    bz    this0     ;is a 0
    btfsc Mode,0      ;what mode?
    bra   change1     ;if mode 1, then don't send an OFF
    bsf   Tx1d0,0     ;set to a 1 (off state)
this2 clrf  Tx1d3
this4 movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    
    movlw   5
    movwf Dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh     ;low priority
    movlw .10
    movwf Latcount
    call  sendTX      ;send frame
    incf  Incount,F
    rlcf  Inbit,F
    bc    end_scan
    call  dely
    
    bra   change1
end_scan
    return
    
this0 bcf   Tx1d0,0     ;set to a 0 (on state)
    bra   this2
    nop
    return

dn_out  movlw LOW ENstart
    movwf EEADR
    bcf   STATUS,C      ;just in case
    rlncf ENcount,F   ;4 bytes per event
    rlncf ENcount,W
    addlw 2       ;just the last two event bytes are the DN
    addwf EEADR
    call  eeread
    movwf Tx1d3
    incf  EEADR
    call  eeread
    movwf Tx1d4
    movlw 0x98      ;set to ON
    movwf Tx1d0
    movf  InpVal,W
    andwf Inbit,W     ;what is the new state
    bz    this4     ;is a 0
    btfss Mode,0      ;on only?
    bra   dn_out1
    bra   change1
dn_out1 bsf   Tx1d0,0     ;off
    bra   this4
    

;*********************************************************

;   learn input of EN

learnin btfsc Mode,1
    bra   lrnin1
    btfss PORTA,UNLEARN   ;don't do if unlearn
    return
lrnin1  movlw LOW ENindex+1
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
    btfsc Mode,1
    bra   notful
    movlw EN_NUM        ;is it full now?
    subwf Temp,W
    bnz   notful
    retlw 1
notful  retlw 0
    
;**************************************************************************
;
;   EN match. Compares EN (in Rx0d1, Rx0d2, Rx0d3 and Rx0d4) with stored ENs
;   If match, returns with W = 0
;   The matching number is in ENcount. 
;   The EVs are in EVtemp ans EVtemp2
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
    movwf EVtemp    ;EV  (EV1)
    incf  EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf EVtemp2 ;EV qualifier  (EV2)
    
    retlw 0     ;is a match
en_match  
    movf  Count,F
    bz    en_out
    decf  Count,F
    incf  ENcount,F
    bra   ennext
en_out  retlw 1 

;******************************************************************
;   Checks EVs for 'device numbers'
;   If any set, puts short event number in ENcount for output
;   Arrives with switch number in Incount

ev_match  movff Incount,SN_temp   ;save switch number
      bsf   SN_temp,3     ;needed for EV with device numbers
      clrf  ENcount
      movlw .32
      movwf Count
      movlw LOW EVstart
      movwf EEADR
ev_mat1   call  eeread
      subwf SN_temp,W
      bz    got_ev
      incf  ENcount
      incf  EEADR
      incf  EEADR   ;EVs are in pairs
      decfsz  Count
      bra   ev_mat1 
      retlw 1     ;no match
got_ev    retlw 0
    
;********************************************************************
;   Do an event.  arrives with EV in EVtemp and EVtemp2

ev_set  movlw 0     ;what is EV?
    subwf EVtemp,W
    bz    state_seq ;send state sequence if EV = 0
    movlw 1
    subwf EVtemp,W
    bz    route_x   ;send event to set route
    btfss EVtemp,3  ;is it a single input state request?
    return        ;no more yet
    bra   ss_in

route_x   goto  route ;branch was too long

state_seq
    call  dely
    clrf  Incount   ;for switch number
    movlw 5
    movwf Dlc
    movf  Rx0d0,W
    movwf Cmdtemp
    movlw CMD_REQ
    subwf Cmdtemp,W
    bz    s_seq1      ;a long request command   
    movlw SCMD_REQ    ;is it a short request
    subwf Cmdtemp,W
    bnz   s_seq3      ;not a request
    movlw 0x90
    movwf Cmdtemp
    bra   s_seq2
s_seq1  movlw 0x93      ;ON response OPC
    movwf Cmdtemp
    bra   s_seq2
s_seq3  movlw 0x90      ;ON command OPC
    movwf Cmdtemp
s_seq2  movff Cmdtemp,Tx1d0 ;put in command byte
    clrf  Tx1d3
    movff Incount,Tx1d4
    incf  Tx1d4     ;start at 1
    btfsc InpVal,0    ;test input state
    incf  Tx1d0,F     ;off
    call  ev_match
    movwf,W
    bnz   s_seq4      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq4  incf  Incount     ;for next input
    
    btfsc WV_push,0   ;Push button toggle input?
    bra   stog0     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog0 movff Cmdtemp,Tx1d0
  
    btfsc InpVal,1
    incf  Tx1d0,F     ;off
    
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq5      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq5  incf  Incount     ;for next input
    btfsc WV_push,1   ;Push button toggle input?
    bra   stog1     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog1 movff Cmdtemp,Tx1d0
    
    btfsc InpVal,2
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq6      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq6  incf  Incount     ;for next input
    btfsc WV_push,2   ;Push button toggle input?
    bra   stog2     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog2 movff Cmdtemp,Tx1d0
  
    btfsc InpVal,3
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq7      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq7  incf  Incount     ;for next input
    btfsc WV_push,3   ;Push button toggle input?
    bra   stog3     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog3 movff Cmdtemp,Tx1d0
  
    btfsc InpVal,4
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq8      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq8  incf  Incount     ;for next input
    btfsc WV_push,4   ;Push button toggle input?
    bra   stog4     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog4 movff Cmdtemp,Tx1d0
  
    btfsc InpVal,5
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq9      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq9  incf  Incount     ;for next input
    btfsc WV_push,5   ;Push button toggle input?
    bra   stog5     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog5 movff Cmdtemp,Tx1d0
  
    btfsc InpVal,6
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq10     ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq10 incf  Incount     ;for next input
    btfsc WV_push,6   ;Push button toggle input?
    bra   stog6     ;Don't send
    call  sendTX
    call  dely
    clrf  Tx1d3
stog6 movff Cmdtemp,Tx1d0
  
    btfsc InpVal,7
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq11     ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq11
    btfsc WV_push,7   ;Push button toggle input?
    bra   stog7     ;Don't send
    call  sendTX      ;last input
stog7 clrf  TogLast     ;Reset toggle states
    return
    
route movf  Rx0d0,W
    movwf Cmdtemp
    movlw CMD_ON
    subwf Cmdtemp,W ;is it an ON?
    bz    route1
    movlw SCMD_ON
    subwf Cmdtemp,W
    bnz   route4
    movlw 0x90
    movwf Cmdtemp
    bra   route1
route4  movlw CMD_REQ   ;is it a long request?
    subwf Cmdtemp,W
    bnz   route2
    movlw 0x93    ;long response
    movwf Cmdtemp
    bra   route1
route2  movlw SCMD_REQ
    subwf Cmdtemp,W
    bz    route3
    goto  main2
route3  movlw 0x93    ;long response to a short trigger event
    movwf Cmdtemp
route1  movff Cmdtemp,Tx1d0   ;put in CMD byte
    movlw 1
    movwf Tx1d3   ;to distinguish it from input change
    movf  Iin_curr,W
    movwf Tx1d4   ;set event to switch inputs
    movlw 5
    movwf Dlc
    call  sendTX
    bcf   Tx1d3,0
    return 

ss_in movlw SCMD_REQ  ;only can poll bits with a short request
    subwf Rx0d0,W
    bz    ss_in3
    return
ss_in3  bcf   EVtemp,3  ;clear input flag bit
    movlw 1
    movwf In_roll   ;set for roll
        
ss_in1  movf  EVtemp,F
    bz    get_in
    decf  EVtemp,F
    rlncf In_roll,F
    bra   ss_in1
get_in  movf  In_roll,W
    andwf Iin_curr,W
    bz    ss_low
    movlw 0x9E
    movwf Tx1d0   ;an off state response
    bra   ss_in2
ss_low  movlw 0x9D    ;an on state response
    movwf Tx1d0
ss_in2  movff Rx0d3,Tx1d3 ;put device no in Tx buffer
    movff Rx0d4,Tx1d4
    movlw 5
    movwf Dlc
    
    
    
    
sendTX  movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2

sendTXa movf  Dlc,W       ;get data length
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

putNV movlw NV_NUM + 1    ;put new NV in EEPROM and the NV ram.
    cpfslt  Rx0d3
    bra   no_NV
    movf  Rx0d3,W
    bz    no_NV
    decf  WREG      ;NVI starts at 1
    addlw LOW NVstart
    movwf EEADR
    movf  Rx0d4,W
    movwf WV_ononly
    call  eewrite 
    call  wrack
    return
no_NV movlw .10       ;error invalid NV index
    call  errsub
    return

;************************************************************************

getNV movlw NV_NUM + 1    ;get NV from EEPROM and send.
    cpfslt  Rx0d3
    bz    no_NV1
    movf  Rx0d3,W
    bz    no_NV1
    decf  WREG      ;NVI starts at 1
    addlw LOW NVstart
    movwf EEADR
    call  eeread
    movwf Tx1d4     ;NV value
getNV1  movff Rx0d3,Tx1d3   ;NV index
getNV2  movff Rx0d1,Tx1d1
    movff Rx0d2,Tx1d2
    movlw 0x97      ;NV answer
    movwf Tx1d0
    movlw 5
    movwf Dlc
    call  sendTXa
    return

no_NV1  movlw .10     ;if not valid NV
    call  errsub
    return
;**************************************************************************

;   check if command is for this node

thisNN  movf  NN_temph,W
    subwf Rx0d1,W
    bnz   not_NN
    movf  NN_templ,W
    subwf Rx0d2,W
    bnz   not_NN
    retlw   0     ;returns 0 if match
not_NN  retlw 1
              
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
    btfsc Mode,1
    call  wrack
    return

;***********************************************************************
;   gets device number for switch input for a SOD sequence
;   ENcount has event number for that switch

dn_sod    movlw LOW ENstart
    movwf EEADR
    bcf STATUS,C      ;just in case
    rlncf ENcount,F   ;4 bytes per event
    rlncf ENcount,W
    addlw 2       ;just the last two event bytes are the DN
    addwf EEADR
    call  eeread
    movwf Tx1d3
    incf  EEADR
    call  eeread
    bsf   Tx1d0,3     ;for short events
    movwf Tx1d4   
    
    return

;************************************************************************



;**********************************************************************

getop movlw B'00001111'   ;get DIP switch setting for output
    andwf PORTA,W
    movwf EVtemp
    
    
    return

;*************************************************************************

;   read back all events in sequence

enread  clrf  Temp
    movlw LOW ENindex + 1
    movwf EEADR
    call  eeread
    movwf ENtemp1
    sublw 0
    bz    noens   ;no events set
    
    movlw 1
    movwf Tx1d7   ;first event
    movlw LOW ENstart
    movwf EEADR
    
  
enloop1 
;   movff NN_temph,Tx1d1
;   movff NN_templ,Tx1d2
    call  eeread
    movwf Tx1d3
    incf  EEADR,F
    call  eeread
    movwf Tx1d4
    incf  EEADR,F
    call  eeread
    movwf Tx1d5
    incf  EEADR,F
    call  eeread
    movwf Tx1d6
    incf  EEADR,F
    
ensend  movlw 0xF2
    movwf Tx1d0   ;OPC
    movlw 8
    movwf Dlc
    call  sendTX      ;send event back
    call  dely
    movf  Tx1d7,F
    bz    lasten
    incf  Temp,F
    movf  ENtemp1,W
    subwf Temp,W
    bz    lasten
    incf  Tx1d7
    bra   enloop1     ;next one
    
noens movlw 7
    call  errsub
    return
  
  
lasten  return  
;*************************************************************************

; send individual event by index

enrdi movlw LOW ENindex + 1
    movwf EEADR
    call  eeread
    movwf ENtemp1
    sublw 0
    bz    noens   ;no events set
    decf  Rx0d3 
    cpfslt  Rx0d3
    bra   noens   ;too many
    rlncf WREG
    rlncf WREG
    addlw LOW ENstart
    movwf EEADR
    call  eeread
    movwf Tx1d3
    incf  EEADR
    call  eeread
    movwf Tx1d4
    incf  EEADR
    call  eeread
    movwf Tx1d5
    incf  EEADR
    call  eeread
    movwf Tx1d6
    incf  Rx0d3
enrdi1  movff Rx0d3,Tx1d7
    movlw 0xF2
    movwf Tx1d0
    movlw 8
    movwf Dlc
    call  sendTX
    return


;************************************************************************
    
;   send number of events

evns2 movlw LOW ENindex+1
    movwf EEADR
    call  eeread
    movwf Tx1d3
    movlw 0x74
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return
              
;***********************************************************

;   send EVs by reference to EN index

evsend  movf  Rx0d3,W   ;get event index
    sublw 0
    bz    noens   ;can't be zero
    movlw LOW ENindex+1 ;get number of stored events
    movwf EEADR
    call  eeread
    movff Rx0d3,Temp
    decf  Temp
    cpfslt  Temp
    bra   noens   ;too many events in index
    
    movf  Temp,W
    mullw EV_NUM    ;PRODL has start of EVs
    movf  Rx0d4,W   ;get EV index
    sublw 0
    bz    notEV 
    movff Rx0d4, Temp1
    decf  Temp1
    movlw EV_NUM
    cpfslt  Temp1
    bra   notEV   ;too many EVs in index
    movf  Temp1,W
    addwf PRODL,W   ;get EV adress
    addlw LOW EVstart
    movwf EEADR
    call  eeread
    movwf Tx1d5   ;put in EV value
    movlw 0xB5
    movwf Tx1d0
    movff Rx0d3,Tx1d3
    movff Rx0d4,Tx1d4
    movlw 6
    movwf Dlc
    call  sendTX
    return

notEV movlw 6
    call  errsub
    return
    
;************************************************************
;   send node parameter bytes (7 maximum)

parasend  
    movlw 0xEF
    movwf Tx1d0
    movlw 8
    movwf TBLPTRH
    movlw LOW nodeprm
    movwf TBLPTRL
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
para1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   para1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return

;**************************************************************************
;   send module name - 7 bytes

namesend  
    movlw 0xE2
    movwf Tx1d0
    movlw LOW myName
    movwf TBLPTRL
    movlw HIGH myName
    movwf TBLPTRH   ;relocated code
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
name1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   name1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return
    
  
;**********************************************************

;   send individual parameter

;   Index 0 sends no of parameters

para1rd movf  Rx0d3,w
    sublw 0
    bz    numParams
    movlw PRMCOUNT
    movff Rx0d3, Temp
    decf  Temp
    cpfslt  Temp
    bra   pidxerr
    movlw 0x9B
    movwf Tx1d0
    movlw 7   ;FLAGS index in nodeprm
    cpfseq  Temp
    bra   notFlags      
    call  getflags
    movwf Tx1d4
    bra   addflags
notFlags    
    movlw LOW nodeprm
    movwf TBLPTRL
    movlw HIGH nodeprm
    movwf TBLPTRH   ;relocated code
    clrf  TBLPTRU
    decf  Rx0d3,W
    addwf TBLPTRL
    bsf   EECON1,EEPGD
    tblrd*
    movff TABLAT,Tx1d4
addflags            
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    call  sendTX
    return  
    
numParams
    movlw 0x9B
    movwf Tx1d0
    movlw PRMCOUNT
    movwf Tx1d4
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    call  sendTX
    return
    
pidxerr
    movlw .10
    call  errsub
    return
    
getflags    ; create flags byte
    movlw PF_COMBI
    btfsc Mode,1
    iorlw 4   ; set bit 2
    movwf Temp
    bsf   Temp,3    ;set bit 3, we are bootable
    movf  Temp,w
    return
    
;**********************************************************

; returns Node Number, Manufacturer Id, Module Id and Flags

whoami
    call  ldely   ;wait for other nodes
    movlw OPC_PNN
    movwf Tx1d0
    movlw MAN_NO  ;Manufacturer Id
    movwf Tx1d3
    movlw MODULE_ID   ; Module Id
    movwf Tx1d4
    call  getflags
    movwf Tx1d5
    movlw 6
    movwf Dlc
    call  sendTX
    return
        
;***********************************************************

; error message send

errmsg  call  errsub
    goto  main2 
errmsg1 call  errsub
    goto  l_out2
errmsg2 call  errsub
    goto  l_out1

errsub  movwf Tx1d3   ;main eror message send. Error no. in WREG
    movlw 0x6F
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return
;**********************************************************************

;
;   self enumeration as separate subroutine

self_en movff FSR1L,Fsr_tmp1Le  ;save FSR1 just in case
    movff FSR1H,Fsr_tmp1He 
    movlw B'11000000'
    movwf INTCON      ;start interrupts if not already started
    bsf   Datmode,1   ;set to 'setup' mode
    clrf  Tx1con      ;CAN ID enumeration. Send RTR frame, start timer
    movlw .14
    movwf Count
    lfsr  FSR0, Enum0
clr_en
    clrf  POSTINC0
    decfsz  Count
    bra   clr_en
    
    movlw B'10111111'   ;fixed node, default ID  
    movwf Tx1sidh
    movlw B'11100000'
    movwf Tx1sidl
    movlw B'01000000'   ;RTR frame
    movwf Dlc
    
    movlw 0x3C      ;set T3 to 100 mSec (may need more?)
    movwf TMR3H
    movlw 0xAF
    movwf TMR3L
    movlw B'10110001'
    movwf T3CON     ;enable timer 3

    movlw .10
    movwf Latcount
    
    call  sendTXa     ;send RTR frame
    clrf  Tx1dlc      ;prevent more RTR frames

self_en1    btfss PIR2,TMR3IF   ;setup timer out?
    bra   self_en1      ;fast loop till timer out 
    bcf   T3CON,TMR3ON  ;timer off
    bcf   PIR2,TMR3IF   ;clear flag


    clrf  IDcount
    incf  IDcount,F     ;ID starts at 1
    clrf  Roll
    bsf   Roll,0
    lfsr  FSR1,Enum0      ;set FSR to start
here1 incf  INDF1,W       ;find a space
    bnz   here
    movlw 8
    addwf IDcount,F
    incf  FSR1L
    bra   here1
here  movf  Roll,W
    andwf INDF1,W
    bz    here2
    rlcf  Roll,F
    incf  IDcount,F
    bra   here
here2 movlw .100        ;limit to ID
    cpfslt  IDcount
    bra   segful        ;segment full
    
here3 movlw LOW CANid   ;put new ID in EEPROM
    movwf EEADR
    movf  IDcount,W
    call  eewrite
    call  newid_f     ;put new ID in various buffers

      
    movff Fsr_tmp1Le,FSR1L  ;
    movff Fsr_tmp1He,FSR1H 
    return

segful  movlw 7   ;segment full, no CAN_ID allocated
    call  errsub
    setf  IDcount
    bcf   IDcount,7
    bra   here3

;*********************************************************
;   a delay routine
      
dely  movlw .10
    movwf Count1
dely2 clrf  Count
dely1 decfsz  Count,F
    goto  dely1
    decfsz  Count1
    bra   dely2
    return    
    
;****************************************************************

;   longer delay

ldely movlw .100
    movwf Count2
ldely1  call  dely
    decfsz  Count2
    bra   ldely1
    
    return
;************************************************************************   
  ORG 0xF00000      ;EEPROM data. Defaults
  
CANid de  B'01111111',0 ;CAN id default and module status (Modstat is address 1)
NodeID  de  0,0     ;Node ID
ENindex de  0,0   ;points to next available EN number (only lo byte used)
          ;value actually stored in ENindex+1

  ORG 0xF00006

ENstart 

    ORG 0xF00086
    
EVstart de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ;allows for 2 EVs per event.
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0



NVstart de  0,0       ;On Event Mask/Input Invert Mask
    ifdef CANTOTI
    de  B'11111111',.10 ;CANTOTI Delayed Input Mask/On time (100mS)
    de  .50,0     ;CANTOTI Off time (500mS)/Push Button Toggle Input
    else
    de  B'00000000',.10 ;CANACE8C Delayed Input Mask/On time (100mS)
    de  .10,0     ;CANACE8C Off time (100mS)/Push Button Toggle Input
    endif
    de  0,0       ;Expanded Mode (currently unused)
    de  0,0,0,0,0,0,0,0 ;Up to 16 NVs here if wanted
    
    ORG 0xF000FE
    de  0,0                 ;for boot.      
    end
