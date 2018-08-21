    TITLE   "Source for CANACE8C/CANTOTI/CANMIO node for CBUS"

MAJOR_VER   equ .2      ;Firmware major version (numeric)
MINOR_VER   equ "q"     ;Firmware minor version (alpha)
BETA_VER    equ .0    ;Firmware beta version (numeric, 0 = Release)
AUTOID      equ .0      ;Include automatic CAN ID enumeration (this may cause problems with CANCAN)

;Define CANTOTI for a TOTI module
;Define CANACE8MIO for CANMIO hardware

; Date    Rev   By  Notes
; 24-Dec-16 v2q   PJW v2q release
; 14-Dec-16 v2q3  PJW Combined v2q2 and v2p3 changes
;  9-Dec-16 v2p3  PJW Fix oddities with missing ONONLY events with fast changing inputs
;  5-Dec-16 v2p2  PJW Removed potential interrupt race condition
; 07-Aug-15 V2q2    PNB Update to use cbusdefs8j and support SLiM on CANMIO hardware
; 12-May-15 v2q1    PNB Add additional 8 (non configurable) inputs on CANMIO expansion connector when in FLiM
; 25-Sep-14 v2p1  PJW Fixed a few minor bugs with Route logic
; 20-Sep-14     PJW Rewritten output routines to correct ONONLY oddities
; 17-Jun-14 v2n   PJW v2n release
; 10-Jun-14 v2n11 PJW Input specific SOD inhibit now coded for new FCU.
;           Also returns physical PIC in node properties
; 25-May-14 v2n10 PJW SOD now won't now send OFF event for an ON only input
;         PJW "Expanded Mode" now inhibits SOD for input is bit is 1
; 23-May-14     PJW Test version to check for issues
; 22-May-14 v2n9  PJW Ignore input changes for the first 2 seconds after powerup
; 14-May-14     PJW Removed unwanted CANMIOTOTI option
; 22-Apr-14 v2n8  PW  Simplified hardware/software defines
; 21-Apr-14 v2n8  PB  Added CPUM_MICROCHIP
; 21-Apr-14 v2n8  PW  Synchronise with Pete Brownlows changes
; 19-Feb-14 v2n7  BV  Revised AUTOID logic
; 10-Feb-14 v2n6  PJW CANID enumeration now optional (see AUTOID)
;  8-Feb-14 v2n5  PJW Fixed bug introduced in 2k that caused spurious events on powerup
;  3-Feb-14     PJW Bit of a tidy up
;  1-Feb-14     PJW Added beta revision in parameter #20
; 27-Jan-14     PJW Synchronised various changes
; 27-Jan-14     RH  Updated CANID enumeration
; 24-Jan-14     PJW Now monitoring bus events to keep push button modes in step
; 24-Jan-14     PJW Updated to use cbusdefs8g.inc; hard coded opcodes now symbolic
;  3-Sep-13 v2m   BV  Added 'Route Event' on specific input change
;           Auto self enumerate on CANID Conflict
;           RH  NVNUM set to 8, Add Bob's change to CanId conflict test
;     v102m   RH  development version to test auto CAN Id enumeration
;           NV_NUM set to 7 for now so it works with FCU
;             change include file to cbusdefs8f.inc
; 26-Jul-13 v2k   PJW Added optional push button toggle input mode
;           This source file now builds CANACE8C and CANTOTI firmware
;           Requires FCU V1.4.7.9 or later to configure this mode
; 14-Oct-12 v2j   PJW Added optional input inversion and input delay controls
;           Corrected event initialisation & firmware ID
; 24-Jul-12 v2h     Changes for new self-enum as subroutine. new OpCodes 0x5D and 0x75 for self enum. 
;       v2g     Change parameters to new format
;       v2f     Correct error codes when reading events, fix bug in evsend
;       v2e     Add check for zero index reading parameters
;       v2d     Fix bug when reponding to SOD, Txld3 not cleared
;           leaving device ms byte in buffer for following no-device inputs
;       v2c     Allow QNN to work in SLiM mode
;       v2b     Change reply to QNN to OPC_PNN
;       v2a     First release build
;       102e    Change CAN initialisation code for extended frams bug
;       102d    set RXM0 in RXB0CON and RXB1CON
;       102c    remove fix for extended frames - test
;     102_a     First version wrt CBUS Developers Guide
;           Add code to support 0x11 (RQMN)
;           Add code to return 8th parameter by index - Flags
; 28-Jul-11 x     Polling inputs (short mode) gives a response of 0x9D for ON and 0x9E for OFF
;           Added WRACK to all EEPROM write sequences. Added error responses.
;     w     Now produces short events on a SOD if the inputs have a 'device number'.
;           same short events with a short poll (0x9A) to a device no.
;     v     Corrected bug so now responds to a RTR in SLiM mode.
;     u       Revert to allowing Boot and Read Params in SLiM mode with nodes NN
;  7-Mar-11 t       Only allow Boot with NN of zero. Allow read params by index in slim mode with NN of zero
; 19-Jan-11 s     Added short event output events if there is an allocated 'device number'. (Modes 8 to 15 set)
; 12-Dec-10 r     Added response to short events and also mode for individual event polling. 04/12/10
;           Modes 8 to 15 correspond to inputs 1 to 8
;           Short events taught with the mode set to 8 - 15 give a response matching inputs
;           1 to 8. Poll with event 0x9A using the lower two bytes for the 'device number'
;           Fix in 'route' routine for short mode events
;           Changes to state sequence and route routines for short event requests and responses.
;     q     Added clear of RXB overflow flags in COMSTAT
; 24-Mar-10 p     new enum scheme (no rev o)
; 17-Mar-10 n     Prevent eror messages in unset
;  2-Mar-10 m     Added facility to trigger mode 1 events using the PB in SLiM mode
;     k     Roger's mods to TXB2CON
;     j     Mods to RTR and Unlearn
; 27-Jan-10 h     Added NNACK for config, Block to non supported Events in SLiM
; 30-Dec-09       Mods to bootloader for LEDs and WDT
; 28-Nov-09 g     Changed read of EVs so it is not in read mode  
; 27-Nov-09 f     Added bootloader and other OPCs for full FLiM / SLiM version 
;           Incorporated NV for setting individual inputs to ON / OFF or ON only 
; 14-Nov-09       started changes for combined FLiM / SLiM
;           incorporated ON only for SLiM. NN range now 64

; Uses 4 MHz resonator and PLL for 16 MHz clock
; This is an 8 input FLiM producer node with readout.
; Has 8 switch / logic inputs with pullups.
; Uses a 6 way DIP switch to set base NN and CAN ID (unless set otherwise) and 
; for learning request events (learn + unlearn)
; Sends 8 ON / OFF events using the 32 bit EN protocol.
; Sends response event with the inputs as the LSbyte
; Additional input control for inversion, delays and push button input.

; This source file can also generate CANTOTI code by defining 'CANTOTI'

; The setup timer is TMR3. This should not be used for anything else
; CAN bit rate of 125 Kbits/sec, Standard frame only

; this code is for 18F2480

;DIL switch ettings

; 1 NN select LSB
; 2 NN select
; 3 NN select
; 4 NN select   MSB
; 5 Learn
; 6 Unlearn / reset
; 1 to 4 also select the response type when in learn mode. Must be put back to
; the NN after learning.

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
  include   "cbuslib/cbusdefs.inc"
  include   "cbuslib/mioSLiM.inc"
  ;definitions  Change these to suit hardware.
  
; Define module type code and name string for hardware and firmware variations

        ifdef   CANACE8MIO    ;CANMIO hardware
S_PORT  equ PORTA       ;Setup Switch Port
S_BIT equ 2         ;Setup Switch Bit
MODULE_ID   equ MTYP_CANACE8MIO
#define     MYNAME   "ACE8MIO"
        else          ;CANACE8c hardware
S_PORT  equ PORTB       ;Setup Switch Port
S_BIT   equ 0         ;Setup Switch Bit 
          ifdef CANTOTI
MODULE_ID   equ MTYP_CANTOTI
#define     MYNAME   "TOTI   "
            else
MODULE_ID   equ MTYP_CANACE8C
#define     MYNAME   "ACE8C  "
          endif
        endif

LPINT equ .10       ; Low Priority Interrupt time (mS)
TMR1CN  equ 0x10000-(.4000000*LPINT/.1000)  ;Timer 1 count (counts UP)

SUDELY  equ .2000 ;Startup delay (mS)

LEARN   equ 4   ;learn switch in port A
UNLEARN equ 5   ;unlearn switch in port A

EN_NUM  equ .32   ;number of allowed events
EV_NUM  equ 2   ;number of allowed EVs per event

MIN_TIME  equ 2       ;minimum time for input change (*10mS) + 1

MAN_NO      equ MANU_MERG     ;manufacturer number

EVT_NUM     equ EN_NUM        ; Number of events
EVperEVT    equ EV_NUM        ; Event variables per event

NV_NUM      equ .9        ; Number of node variables
NODEFLGS    equ PF_COMBI + PF_BOOT
CPU_TYPE    equ P18F2480

Modstat equ 1   ;address in EEPROM

; Modstat bit usages

;   0 - FLiM mode


;Self enumeration bits stored in Datmode
MD_NEWFRM equ 0 ;new frame received
MD_SETUP  equ 1 ;in setup mode
MD_NNWAIT equ 2 ;waiting for NN
MD_FLRUN  equ 3 ;FLiM Run mode
MD_EMSUP  equ 4 ;suppress error message
MD_EVULN  equ 5 ;unlearn event
MD_EVRD   equ 6 ;read event variable
MD_IDCONF equ 7 ;ID conflict detected

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

  IDcount   ;used in self allocation of CAN ID.
  Datmode   ;flag for data waiting and other states
  Count   ;counter for loading
  Count1
  Count2
  Keepcnt   ;keep alive counter
  Latcount  ;latency counter

  Temp    ;temps
  Temp1
  InputInt  ;Input from interrupt routine
  InpScan   ;Current inputs for scan
  TogLast   ;Last toggle state
  InpVal    ;Logical Input State (0=Active)
  InpChg    ;Changed inputs
  Inbit
  Incount
  InOnOnly
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
  Dummy1    ;Don't remove these otherwise odd things will happen
  Dummy2    ;Not sure why, PJW May14
  
  WV_ononly ;Working Variable: Bitmapped "on-only" flags
  WV_invt   ;Working Variable: Bitmapped Input invert flags
  WV_dlyd   ;Working Variable: Bitmapped Delayed input flags
  WV_ontm   ;Working Variable: On Time (MIN_TIME..255)
  WV_oftm   ;Working Variable: Off Time (MIN_TIME..255)
  WV_pbtg   ;Working Variable: Bitmapped "Push Button Toggle Input" flags
  WV_route  ;Working Variable: Bitmapped Input trigger Route event
  WV_isod   ;Working Variable: Inhibit SOD for input
  WV_mode   ;Working Variable: Expanded Mode (for future use)

; Variables for additional 8 inputs on CANMIO hardware

    inputs      ;Status of additional inputs
    dbinputs    ;Status of inputs during debounce
    rawinps     ;Raw inputs just read in
    dbcount     ;Debounce counter
    Op_fb       ;Feedback input number for SOD

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

  SUtimer       ;Startup timer


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

    org RESET_VECT
loadadr   
    nop           ;for debug
    goto  setup

    ORG   0808h
    goto  hpint     ;high priority interrupt
    
    ORG   0810h     
myName  db    MYNAME      ;module name (7 characters)
    
    ORG   0818h 
    goto  lpint     ;low priority interrupt

    ORG   0820h     ;Main parameters
nodeprm db  MAN_NO, MINOR_VER ;1-2
    db  MODULE_ID, EVT_NUM  ;3-4
    db  EVperEVT, NV_NUM  ;5-6
    db  MAJOR_VER,NODEFLGS  ;7-8
    db  CPU_TYPE,PB_CAN     ;9-10 
        dw  RESET_VECT        ;11-12 Load address for module code above bootloader
        dw  0               ;13-14 Top 2 bytes of 32 bit address not used
    dw  0         ;15-16 CPU Manufacturers ID low
    dw  0         ;17-18 CPU Manufacturers ID high
    db  CPUM_MICROCHIP,BETA_VER     ;19-20 CPU Manufacturers code, Beta revision
sparprm fill 0,prmcnt-$     ;Unused parameter space set to zero

PRMCOUNT equ sparprm-nodeprm ; Number of parameter bytes implemented

        ORG 0838h

prmcnt  dw  PRMCOUNT    ; Number of parameters implemented
nodenam dw  myName      ; Pointer to module type name
        dw  0 ; Top 2 bytes of 32 bit address not used


PRCKSUM equ MAN_NO+MINOR_VER+MODULE_ID+EVT_NUM+EVperEVT+NV_NUM+MAJOR_VER+NODEFLGS+CPU_TYPE+PB_CAN+HIGH myName+LOW myName+HIGH loadadr+LOW loadadr+PRMCOUNT+CPUM_MICROCHIP+BETA_VER

cksum   dw  PRCKSUM     ; Checksum of parameters

;*******************************************************************

    ORG   0840h     ;start of program
;
#ifdef   CANACE8MIO
  #include "cbuslib/canmio.asm" ; Subroutines for additional inputs and CANMIO SLiM
#endif

;
;   high priority interrupt. Used for CAN receive and transmit error.

hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
;   movff PCLATH,PCH_tempH    ;save PCLATH

  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H
    
;   movlw 8     ;for relocated code
        movlw   HIGH (cstatab)
    movwf PCLATH
    movf  TempCANSTAT,W     ;Jump table
  
    andlw B'00001110'
cstatab addwf PCL,F     ;jump
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
    btfsc Datmode,MD_SETUP  ;setup mode?
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
    iorwf CANCON,F
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
;   btfss Rx0sidl,3     ;ignore extended frames
    bsf   Datmode,MD_NEWFRM ;valid message frame
#if AUTOID    
    btfss Mode,1        ;FLim Mode?
    bra   back        ; dont do Can ID check if SLiM mode
    ;check for ID conflict
    movf  Rx0sidh,w   ;get received IDHi
    xorwf Tx1sidh,w   ;compare with our IDHi
    andlw 0x0f      ;Bob's update
    bnz   back      ;skip if not matched
    movf  Rx0sidl,w   ;get received IDLo
    xorwf Tx1sidl,w   ;compare with our IDLo
    andlw 0xe0      ;Bob's revised update
    bnz   back      ;skip if not matched
    bsf   Datmode,MD_IDCONF ;flag ID conflict   
#endif    
back  bcf   RXB0CON,RXFUL ;ready for next
  
back1 clrf  PIR3      ;clear all interrupt flags
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
    
isRTR btfsc Datmode,MD_SETUP;setup mode?
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
; inputs in InputInt for the foreground routine to send as CBUS events

lpint movwf W_tempL       ;Save registers
    movff STATUS,St_tempL
    movff BSR,Bsr_tempL

;   movff PCLATH,PCH_tempL  ;save PCLATH
;   clrf  PCLATH

    movlw LOW TMR1CN      ;Set low byte of interrupt rate
    movwf TMR1L       ;reset timer 1
    clrf  PIR1        ;clear all timer flags
    
    movf  SUtimer,W     ;check startup timer
    bz    lpint0        ;already zero
    decf  SUtimer
lpint0    

;   btg   PORTB,7       ;toggle green LED for now
    movf  PORTC,W       ;read inputs
    xorwf WV_invt,W     ;invert as required
    xorwf Iin_curr,W      ;compare with current bits
    bz    lpchk0        ;No change, just check timeouts

; One or more inputs in W have changed

    movwf Iin_delta     ;save changed bits
    xorwf Iin_curr,F      ;save current bits

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
    bcf   InputInt,0
    btfsc Iin_curr,0      ;Skip if input inactive
    bsf   InputInt,0
    
lpchk1  movf  Iin_count1,W    ;Get counter
    bz    lpchk2        ;Zero, nothing to do
    decfsz  Iin_count1      ;Decrement counter, skip if zero
    bra   lpchk2        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,1
    btfsc Iin_curr,1      ;Skip if input inactive
    bsf   InputInt,1
      
lpchk2  movf  Iin_count2,W    ;Get counter
    bz    lpchk3        ;Zero, nothing to do
    decfsz  Iin_count2      ;Decrement counter, skip if zero
    bra   lpchk3        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,2
    btfsc Iin_curr,2      ;Skip if input inactive
    bsf   InputInt,2
      
lpchk3  movf  Iin_count3,W    ;Get counter
    bz    lpchk4        ;Zero, nothing to do
    decfsz  Iin_count3      ;Decrement counter, skip if zero
    bra   lpchk4        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,3
    btfsc Iin_curr,3      ;Skip if input inactive
    bsf   InputInt,3
      
lpchk4  movf  Iin_count4,W    ;Get counter
    bz    lpchk5        ;Zero, nothing to do
    decfsz  Iin_count4      ;Decrement counter, skip if zero
    bra   lpchk5        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,4
    btfsc Iin_curr,4      ;Skip if input inactive
    bsf   InputInt,4
      
lpchk5  movf  Iin_count5,W    ;Get counter
    bz    lpchk6        ;Zero, nothing to do
    decfsz  Iin_count5      ;Decrement counter, skip if zero
    bra   lpchk6        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,5
    btfsc Iin_curr,5      ;Skip if input inactive
    bsf   InputInt,5
      
lpchk6  movf  Iin_count6,W    ;Get counter
    bz    lpchk7        ;Zero, nothing to do
    decfsz  Iin_count6      ;Decrement counter, skip if zero
    bra   lpchk7        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,6
    btfsc Iin_curr,6      ;Skip if input inactive
    bsf   InputInt,6
      
lpchk7  movf  Iin_count7,W    ;Get counter
    bz    lpchk8        ;Zero, nothing to do
    decfsz  Iin_count7      ;Decrement counter, skip if zero
    bra   lpchk8        ;Still counting
    ;Input has now actually changed, update master store
    bcf   InputInt,7
    btfsc Iin_curr,7      ;Skip if input inactive
    bsf   InputInt,7
      
lpchk8

lpend movff Bsr_tempL,BSR   ;End of low priority interrupt
    movf  W_tempL,W
    movff St_tempL,STATUS 
    retfie  

;*********************************************************************
; main waiting loop

main  
    btfsc Mode,1      ;is it SLiM?
    bra   mainf     ;no

mains             ;is SLiM

    btfss PIR2,TMR3IF   ;flash timer overflow?
    bra   nofl_s      ;no SLiM flash
    btg   PORTB,7     ;toggle green LED
    bcf   PIR2,TMR3IF
nofl_s  bra   noflash       ;main1
    
; here if FLiM mde

mainf
#if AUTOID    
    btfss Datmode,MD_IDCONF
    bra   mainf_1
    call  self_enA
    movlw B'00001000'   ;back to normal running
    movwf Datmode   
    
mainf_1
#endif
    btfss INTCON,TMR0IF   ;is it flash?
    bra   noflash
    btfss Datmode,MD_NNWAIT
    bra   nofl1
    
    btg   PORTB,6     ;flash yellow LED
    
nofl1 bcf   INTCON,TMR0IF
    btfss Datmode,MD_FLRUN  ;running mode
    bra   noflash
    decfsz  Keepcnt     ;send keep alive?
    bra   noflash
    movlw .10
    movwf Keepcnt
    movlw OPC_NNACK
;   call  nnrel     ;send keep alive frame (works OK, turn off for now)

noflash btfsc S_PORT,S_BIT  ;setup button?
    bra   main3
    movlw .100
    movwf Count
    clrf  Count1
    clrf  Count2
wait  decfsz  Count2
    goto  wait
    btfss Datmode,MD_NNWAIT
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
    movlw OPC_NNREL   ;send node release frame
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
    bcf   Datmode,MD_SETUP
    call  nnack       ;send request for NN
    bsf   Datmode,MD_NNWAIT
;   movlw Modstat       ;only if needed
;   movwf EEADR
;   movlw B'00000100'
;   call  eewrite       ;mode to wait for NN in EEPROM
    bra   main1


main4 ;btfss  Datmode,MD_FLRUN    
    ;bra    main3
    btfss Datmode,MD_NNWAIT
    bra   mset2
    bcf   Datmode,MD_NNWAIT
    bsf   PORTB,6     ;LED on
    movlw OPC_NNACK
    call  nnrel
    movlw Modstat
    movwf EEADR
    movlw B'00001000'
    movwf Datmode     ;normal
    call  eewrite
    bra   main3
    
mset2 bsf   Datmode,MD_NNWAIT
    call  self_en
    bcf   Datmode,MD_SETUP
    call  nnack
    bra   main1

main3 btfss Datmode,MD_SETUP    ;setup mode ?
    bra   main1
;   call  self_en

    bcf   Datmode,MD_SETUP    ;out of setup
    bsf   Datmode,MD_NNWAIT   ;wait for NN
;   call  nnack     ;send blank NN for config
  
;   bsf   PORTB,7     ;on light
    bra   main1     ;continue normally

go_FLiM bsf   Datmode,MD_SETUP    ;FLiM setup mode
    bcf   PORTB,7     ;green off
    bra   wait1
    
; common to FLiM and SLiM   

main1
#ifdef  CANACE8MIO
        call    chkinp          ; Check for any changes on the additional inputs
#endif
    btfsc Datmode,MD_NEWFRM   ;any new CAN frame received?
    bra   packet      ;yes
    bra   do        ;look for inputs

;********************************************************************

;   These are here as branch was too long

go_on_x goto  go_on

;********************************************************

unset ;bsf    Datmode,MD_EVULN    ;unlearn this event
    ;bra    go_on
    btfss   Datmode,MD_EMSUP
    bra   main2       ;prevent error messages on OPC 0x95
    bsf   Datmode,MD_EVULN
    bra   learn1

readEV  btfss Datmode,MD_EMSUP
    bra   main2     ;prevent error message
    bsf   Datmode,MD_EVRD   ;read back an EV
    bra   learn1

evns1 call  thisNN        ;read event numbers
    sublw 0
    bnz   notNNx
    call  evns2
    bra   main2
;evns3  goto  notNN

sendNN  btfss Datmode,MD_NNWAIT   ;in NN set mode?
    bra   main2     ;no
    movlw OPC_RQNN    ;send back NN
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
    btfss Datmode,MD_NNWAIT   ;only in setup mode
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
    
packet  movlw OPC_ACON
    subwf Rx0d0,W
    bz    go_on_x
    movlw OPC_ACOF
    subwf Rx0d0,W
    bz    go_on_x
    movlw OPC_AREQ
    subwf Rx0d0,W
    bz    go_on_x

    movlw OPC_ASON
    subwf Rx0d0,W
    bz    short
    movlw OPC_ASOF
    subwf Rx0d0,W
    bz    short
    movlw OPC_ASRQ
    subwf Rx0d0,W
    bz    short

    movlw OPC_BOOT      ;reboot
    subwf Rx0d0,W
    bz    reboot
    movlw OPC_RQNPN
    subwf Rx0d0,W
    bz    para1a      ;read individual parameters
    movlw OPC_QNN     ; QNN
    subwf Rx0d0,w
    bz    doQnn
    btfss Mode,1      ;FLiM?
    bra   main2
    movlw OPC_SNN     ;set NN on 0x42
    subwf Rx0d0,W
    bz    setNN
    movlw OPC_RQNP    ;read manufacturer
    subwf Rx0d0,W
    bz    paramsx     ;read node parameters
    movlw OPC_RQMN
    subwf Rx0d0,w
    bz    name      ;read module name   
    movlw OPC_NNLRN   ;set to learn mode on 0x53
    subwf Rx0d0,W
    bz    setlrn    
    movlw OPC_NNULN   ;clear learn mode on 0x54
    subwf Rx0d0,W
    bz    notlrn
    movlw OPC_NNCLR   ;clear all events on 0x55
    subwf Rx0d0,W
    bz    clrens
    movlw OPC_NNEVN   ;read number of events left
    subwf Rx0d0,W
    bz    rden
    movlw OPC_EVLRN   ;is it set event?
    subwf Rx0d0,W
    bz    chklrn      ;do it
    movlw OPC_EVULN   ;is it unset event
    subwf Rx0d0,W     
    bz    unset
    movlw OPC_REQEV   ;read event variables
    subwf Rx0d0,W
    bz    readEV
  
    movlw OPC_NVRD    ;read NVs
    subwf Rx0d0,W
    bz    readNV1
    movlw OPC_NVSET   ;set NV
    subwf Rx0d0,W
    bz    setNV
    movlw OPC_NERD    ;is it read events
    subwf Rx0d0,W
    bz    readEN1
    movlw OPC_NENRD
    subwf Rx0d0,W
    bz    readENi     ;read event by index
    movlw OPC_RQEVN
    subwf Rx0d0,W
    bz    evns
    movlw OPC_REVAL   ;read event variables by EN#
    subwf Rx0d0,W
    bz    reval
    movlw OPC_ENUM    ;re-enumerate
    subwf Rx0d0,W
    bz    enum1
    movlw OPC_CANID   ;force new CAN_ID
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
        
main2 bcf   Datmode,MD_NEWFRM
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
    movlw OPC_NNACK
    call  nnrel       ;acknowledge new CAN_ID
    goto  main2
    
setNN btfss Datmode,MD_NNWAIT   ;in NN set mode?
    bra   main2     ;no
    call  putNN     ;put in NN
    bcf   Datmode,MD_NNWAIT
    bsf   Datmode,MD_FLRUN
    bcf   PORTB,7     ;green LED off
    movlw .10
    movwf Keepcnt     ;for keep alive
    movlw OPC_NNACK
    call  nnrel     ;confirm NN set
    bsf   PORTB,6     ;LED ON
    bra   main2
    
rden  goto  rden1
    
setlrn  call  thisNN
    sublw 0
    bnz   notNN
    bsf   Datmode,MD_EMSUP
;   bsf   PORTB,6     ;LED on
    bra   main2

notlrn  call  thisNN
    sublw 0
    bnz   notNN
    bcf   Datmode,MD_EMSUP
notln1    ;leave in learn mode
    bcf   Datmode,MD_EVULN
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
  
    ;movlw  OPC_ACON    ;is it an ON or request event?
    ;subwf  Rx0d0,W     ;only here if it is.
    ;bnz  main2
go_on1  call  enmatch
    sublw 0
    bz    do_it
    bra   main2     ;not here
    
go_on_s btfss PORTA,LEARN
    bra   learn1      ;is in learn mode
    bra   go_on1

chklrn  btfsc Datmode,MD_EMSUP
    bra   learn1      ;is in learn mode
;   movlw 2       ;errror not in learn mode
;   call  errsub
    bra   main2 

readENi call  thisNN      ;read event by index
    sublw 0
    bnz   notNN
    call  enrdi
    bra   main2

params  btfsc Datmode,MD_NNWAIT   ;only in setup mode
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

do_it call  ev_set      ;do it
    bra   main2
    
rden1 call  thisNN
    sublw 0
    bnz   notNN
    movlw LOW ENindex+1   ;read number of events available
    movwf EEADR
    call  eeread
    sublw EN_NUM
    movwf Tx1d3
    movlw OPC_EVNLF
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    bra   main2

learn1  btfss Mode,1      ;FLiM?
    bra   learn2
    movlw OPC_EVLRN
    subwf Rx0d0,W     ;is it a learn command
    bz    learn2      ;OK
    movlw OPC_EVULN   ;is it unlearn
    subwf Rx0d0,W
    bz    learn2
    movlw OPC_REQEV
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
    
learn3  btfsc Datmode,MD_EVRD ;read EV?
    bra   rdbak1      ;not here
    btfsc Datmode,MD_EVULN  ;if unset and not here
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

isth1 btfss Datmode,MD_EVULN    ;FLiM unlearn?
    bra   mod_EV
    bra   unlearn

enum  call  thisNN
    sublw 0
    bnz   notNN1
    call  self_en
    movlw OPC_NNACK
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
  

shift4  movlw OPC_EVANS     ;readback of EVs
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
    btfsc Datmode,MD_EVRD     ;is it readback
    bra   rdbak
    movf  EVtemp2,W
    call  eewrite       ;put in
    call  wrack       ;write acknowledge
    bra   l_out2


      

l_out bcf   Datmode,MD_EMSUP
;   bcf   LED_PORT,LED2
l_out1  bcf   Datmode,MD_EVRD
l_out2  bcf   Datmode,MD_NEWFRM
    

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
un3   bcf   Datmode,MD_EVULN
    call  wrack
    bra   l_out
        

  
do    btfss Mode,1      ;in FLiM?
    bra   do2       ;no
    btfss Datmode,MD_FLRUN    ;ignore if not set up
    bra   do1
    btfsc Datmode,MD_NNWAIT   ;don't do if in setup   
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
    clrf  ECANCON     ;CAN mode 0 for now   
     
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

mskload lfsr  FSR0,RXM0SIDH   ;Clear masks, point to start
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
    movlw HIGH TMR1CN   ;Set interrupt rate
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
    movwf InpVal
    movwf Iin_curr
    movwf Iin_delta
    movwf InpScan
    movwf InputInt
    clrf  TogLast     ;Clear any toggle states
    clrf  InpChg
    movlw SUDELY/LPINT
    movwf SUtimer     ;Initialise startup timer
    ;   test for setup mode
    clrf  Mode
    movlw Modstat     ;get setup status
    movwf EEADR
    call  eeread
    movwf Datmode
    btfss Datmode,0       ;is SLiM mode?
    bra   slimset     ;No, set up in SLiM mode
                                ;Yes, set up id etc
    
setid bsf   Mode,1      ;flag FLiM
    call  newid_f     ;put ID into Tx1buf, TXB2 and ID number store
    
  
seten_f call  en_ram      ;put events in RAM
    call  nvcopy      ;Copy NV's to RAM
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,7
    bsf   PORTB,6     ;RUN LED on. (yellow for FLiM)
    bcf   Datmode,MD_NEWFRM
    goto  main

slimset

#ifdef CANACE8MIO        
        call    mioslimset
#else
        movlw B'00001111'   ;get DIP switch setting
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
#endif  
    
nwid    call  newid1      ;put ID into Tx1buf, TXB2 and ID number store
    
    ;test for clear all events
#ifdef  CANACE8MIO
        movlw   MIO_SLIM_MASK
        andwf   PORTA,w         ; Get learn switch bits
        sublw   MIOERASE        ; Erase mode set?
        bnz     seten           ; No, continue
#else
    btfss PORTA,LEARN   ;ignore the clear if learn is set
    goto  seten
    btfss PORTA,UNLEARN
#endif
    call  enclear     ;clear all events if unlearn is set during power up
seten call  en_ram      ;put events in RAM

#ifdef  CANACE8MIO
        call    inpset          ; Setup variables for addtional inputs
#endif
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
putNNt  movlw LOW NodeID      ; Entry point with NN already in NN_temp
    movwf EEADR
    movf  NN_temph,W
    call  eewrite
    incf  EEADR
    movf  NN_templ,W
    call  eewrite
    movlw Modstat
    movwf EEADR
    movlw B'00001001'   ;Module status has NN set
    call  eewrite
    return  

newid1  movwf CanID_tmp   ;put in stored ID SLiM mode 
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh,F
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh,F   ;leave priority bits alone
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
    iorwf TXB2SIDH,F    ;set priority
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
    andwf Tx1sidh,F
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh,F   ;leave priority bits alone
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
    iorwf TXB2SIDH,F    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0

    return
    
nnack movlw OPC_RQNN    ;request frame for new NN or ack if not virgin
nnrel movwf Tx1d0
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw 3
    movwf Dlc
    call  sendTX
    return

wrack movlw OPC_WRACK
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
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_ononly   ;Working Variable: Bitmapped "on-only" flags
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_invt     ;Working Variable: Bitmapped Input invert flags
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_dlyd     ;Working Variable: Bitmapped Delayed input flags
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_ontm     ;Working Variable: On Time (MIN_TIME..255)
;We need to add 1 to the on/off times due to the way the timers count
    infsnz  WV_ontm     ;Adjust time, skip if NZ
    decf  WV_ontm     ;Keep at maximum
    movlw MIN_TIME    ;Get minimum time 
    cpfsgt  WV_ontm     ;Skip if time OK
    movwf WV_ontm     ;Save minimum time
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_oftm     ;Working Variable: Off Time (MIN_TIME..255)
;We need to add 1 to the on/off times due to the way the timers count
    infsnz  WV_oftm     ;Adjust time, skip if NZ
    decf  WV_oftm     ;Keep at maximum
    movlw MIN_TIME    ;Get minimum time 
    cpfsgt  WV_oftm     ;Skip if time OK
    movwf WV_oftm     ;Save minimum time
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_pbtg     ;Working Variable: Bitmapped "Push Button Toggle Input" flags
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_route    ;Working Variable: Bitmapped Input trigger Route event
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_isod     ;Working Variable: Inhibit SOD for input
    incf  EEADR

    bsf   EECON1,RD
    movf  EEDATA,W
    movwf WV_mode     ;Working Variable: Expansion mode
    incf  EEADR

    clrf  Iin_count0    ;Clear counters
    clrf  Iin_count1
    clrf  Iin_count2
    clrf  Iin_count3
    clrf  Iin_count4
    clrf  Iin_count5
    clrf  Iin_count6
    clrf  Iin_count7
    clrf  IDcount
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
; InputInt is the conditioned inputs set by the 10mS timer interrupt
; Copy to InpScan and generate appropriate events for CBUS on any changes

scan  movff INTCON,TempINTCON ;Save interrupt state
    clrf  INTCON        ;disable interrupts
    movff InputInt,InpScan  ;Copy current state
    movff TempINTCON,INTCON ;reenable interrupts
    movf  InpScan,W     ;Get inputs to W
    xorwf InputLast,W   ;compare with last inputs
    bz    end_scan      ;nothing changed
    movwf InpChg        ;save changed bits
    xorwf InputLast,F     ;update last bits
    
;***************************************************************
; If we've got here, one or more conditioned inputs have changed
; Updated inputs are in InpScan, changed bits set in InpChg
; Check for any toggle modes
;   InpScan InpChg  WV_pbtg ->  InpVal  InpChg  TogLast
;   x   x   0     InpScan Same  Same
;   0   0   1     TogLast Same  Same
;   0   1   1     TogLast Same  Toggle
;   1   0   1     TogLast Same  Same
;   1   1   1     TogLast 0   Same

; TogLast ^= (~InpScan & WV_pbtg & InpChg)
    movf  InpScan,W
    comf  WREG
    andwf WV_pbtg,W
    andwf InpChg,W
    xorwf TogLast,F
    
; InpChg &= ~(InpScan & WV_pbtg & InpChg)
    movf  InpScan,W
    andwf WV_pbtg,W
    andwf InpChg,W
    comf  WREG
    andwf InpChg,F

; InpVal = (~TogLast & WV_pbtg)
    movf  TogLast,W
    comf  WREG
    andwf WV_pbtg,W
    movwf InpVal
  
; InpVal |= (InpScan & ~WV_pbtg)
    movf  WV_pbtg,W
    comf  WREG
    andwf InpScan,W
    iorwf InpVal,F
    
; In startup period?
    movf  SUtimer,W ;get startup timer
    bnz   end_scan  ;Still starting, ignore

; Do Route?
    movf  WV_route,W  ;get trigger flag
    bz    NoRoute   ;none so skip
    movlw OPC_ACON  ;on event opcode
    movwf Tx1d0   ;set up ready
    incf  WV_route,W  ;test for 0xFF
    bz    RouteAll  ;yes, do route always
    movf  WV_route,W  ;get trigger flag
    andwf InpChg,W  ;has trigger changed
    bz    NoRoute   ;no, so skip
    andwf InpVal,W  ;on or off?
    bz    RouteAll  ;on, so don't -
    incf  Tx1d0   ;inc opcode for off
RouteAll
    movf  InpScan,w ;get conditioned inputs
    movwf Tx1d4   ;save as ENLo
    movlw .2      ;triggered Route always 2
    movwf Tx1d3   ;for ENHi
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw   5
    movwf Dlc
    movlw .10
    movwf Latcount
    call  sendTX    ;send frame
    call  dely
NoRoute
    
; Now process code as normal
    clrf  Incount
    clrf  Inbit
    bsf   Inbit,0     ;rolling bit
    btfss Mode,2      ;mods by Brian W
    call  nvcopy      ;Copy NV's to RAM
    bsf   Mode,2    
    movff WV_ononly,InOnOnly;Copy on-only bits
    btfsc Mode,1      ;skip if SLIM
    bra   change1
    clrf  InOnOnly    ;clear ononly bits
    btfsc Mode,0      ;Skip if SLIM OnOff
    comf  InOnOnly,F    ;Invert
; Now InOnOnly contains bits set for each OnOnly input, SLiM or FLiM 
change1 bcf   STATUS,C
    rrcf  InpChg,F
    bc    this
    incf  Incount,F
    rlcf  Inbit,F     ;added by Roger
    bc    end_scan
    bra   change1

this  movf  Inbit,W
    call  ev_match    ;is it a device numbered switch?
    sublw 0
    bz    dn_out      ;yes so send as short event with DN
    movff Incount,Tx1d4 ;EN number lo byte
    incf  Tx1d4     ;ENs start at 1
    clrf  Tx1d3
    movlw OPC_ACON    ;Command byte
this1 movwf Tx1d0     ;set up event
    movf  InpVal,W
    andwf Inbit,W     ;what is the new state
    bz    this2     ;always send a 1
    movf  InOnOnly,W
    andwf Inbit,W     ;what is the new state
#if 1
    bnz   this3     ;this is non-ideal, but helps to stop
                ;occasional misreads of fast changing OnOnly inputs
#else
    bnz   change1     ;if OnOnly, then don't send an OFF
#endif
    bsf   Tx1d0,0     ;set to a 1 (off state)
this2 movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    
    movlw   5
    movwf Dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh,F   ;low priority
    movlw .10
    movwf Latcount
    call  sendTX      ;send frame
this3 incf  Incount,F
    rlcf  Inbit,F
    bc    end_scan
    call  dely
    bra   change1

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
    movlw OPC_ASON    ;set short event
    bra   this1
    
end_scan
    return
    

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
;Process event opcodes OPC_ACON/OPC_ACOF/OPC_ASON/OPC_ASOF/OPC_AREQ/OPC_ASRQ
;Arrives with opcode in Rx0d0 and event details in EVtemp and EVtemp2

ev_set
    btfsc EVtemp,3  ;Is it an input event?
    bra   ss_in   ;Yes, process
    movlw OPC_ACOF  ;Was it an OFF command?
    subwf Rx0d0,W
    bz    ev_setx   ;Ignore
    movlw OPC_ASOF  ;Was it an OFF command?
    subwf Rx0d0,W
    bz    ev_setx   ;Ignore
    movlw 0     ;what is EV?
    subwf EVtemp,W
    bz    state_seq ;send state sequence if EV = 0
    movlw 1
    subwf EVtemp,W
    bz    route_x   ;send event to set route
ev_setx return        ;no more yet

route_x   goto  route ;branch was too long

state_seq
    call  dely
    clrf  Incount   ;for switch number
    movlw 5
    movwf Dlc
    movf  Rx0d0,W
    movwf Cmdtemp
    movlw OPC_AREQ
    subwf Cmdtemp,W
    bz    s_seq1      ;a long request command   
    movlw OPC_ASRQ    ;is it a short request
    subwf Cmdtemp,W
    bnz   s_seq3      ;not a request
    movlw OPC_ACON
    movwf Cmdtemp
    bra   s_seq2
s_seq1  movlw OPC_ARON    ;ON response OPC
    movwf Cmdtemp
    bra   s_seq2
s_seq3  movlw OPC_ACON      ;ON command OPC
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
    
    btfsc WV_isod,0   ;Inhibit SOD?
    bra   stog0     ;Don't send
    btfsc WV_pbtg,0   ;Push button toggle input?
    bra   stog0     ;Don't send
    btfss WV_ononly,0   ;Skip if on only
    bra   stog0x
    btfsc InpVal,0
    bra   stog0
stog0x  call  sendTX
    call  dely
stog0 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
  
    btfsc InpVal,1
    incf  Tx1d0,F     ;off
    
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq5      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq5  incf  Incount     ;for next input
    btfsc WV_isod,1   ;Inhibit SOD?
    bra   stog1     ;Don't send
    btfsc WV_pbtg,1   ;Push button toggle input?
    bra   stog1     ;Don't send
    btfss WV_ononly,1   ;Skip if on only
    bra   stog1x
    btfsc InpVal,1
    bra   stog1
stog1x  call  sendTX
    call  dely
stog1 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
    
    btfsc InpVal,2
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq6      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq6  incf  Incount     ;for next input
    btfsc WV_isod,2   ;Inhibit SOD?
    bra   stog2     ;Don't send
    btfsc WV_pbtg,2   ;Push button toggle input?
    bra   stog2     ;Don't send
    btfss WV_ononly,2   ;Skip if on only
    bra   stog2x
    btfsc InpVal,2
    bra   stog2
stog2x  call  sendTX
    call  dely
stog2 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
  
    btfsc InpVal,3
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq7      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq7  incf  Incount     ;for next input
    btfsc WV_isod,3   ;Inhibit SOD?
    bra   stog3     ;Don't send
    btfsc WV_pbtg,3   ;Push button toggle input?
    bra   stog3     ;Don't send
    btfss WV_ononly,3   ;Skip if on only
    bra   stog3x
    btfsc InpVal,3
    bra   stog3
stog3x  call  sendTX
    call  dely
stog3 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
  
    btfsc InpVal,4
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq8      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq8  incf  Incount     ;for next input
    btfsc WV_isod,4   ;Inhibit SOD?
    bra   stog4     ;Don't send
    btfsc WV_pbtg,4   ;Push button toggle input?
    bra   stog4     ;Don't send
    btfss WV_ononly,4   ;Skip if on only
    bra   stog4x
    btfsc InpVal,4
    bra   stog4
stog4x  call  sendTX
    call  dely
stog4 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
  
    btfsc InpVal,5
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq9      ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq9  incf  Incount     ;for next input
    btfsc WV_isod,5   ;Inhibit SOD?
    bra   stog5     ;Don't send
    btfsc WV_pbtg,5   ;Push button toggle input?
    bra   stog5     ;Don't send
    btfss WV_ononly,5   ;Skip if on only
    bra   stog5x
    btfsc InpVal,5
    bra   stog5
stog5x  call  sendTX
    call  dely
stog5 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
  
    btfsc InpVal,6
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq10     ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq10 incf  Incount     ;for next input
    btfsc WV_isod,6   ;Inhibit SOD?
    bra   stog6     ;Don't send
    btfsc WV_pbtg,6   ;Push button toggle input?
    bra   stog6     ;Don't send
    btfss WV_ononly,6   ;Skip if on only
    bra   stog6x
    btfsc InpVal,6
    bra   stog6
stog6x  call  sendTX
    call  dely
stog6 clrf  Tx1d3
    movff Cmdtemp,Tx1d0
  
    btfsc InpVal,7
    incf  Tx1d0,F     ;off
    movff Incount,Tx1d4
    incf  Tx1d4
    call  ev_match
    movwf,W
    bnz   s_seq11     ;not a device numbered switch
    call  dn_sod      ;do a device numbered response
s_seq11
    btfsc WV_isod,7   ;Inhibit SOD?
    bra   stog7     ;Don't send
    btfsc WV_pbtg,7   ;Push button toggle input?
    bra   stog7     ;Don't send
    btfss WV_ononly,7   ;Skip if on only
    bra   stog7x
    btfsc InpVal,7
    bra   stog7
stog7x  call  sendTX      ;last input
stog7 clrf  TogLast     ;Reset toggle states

;   Do SOD response for additional inputs on CANMIO

#ifdef  CANACE8MIO
        call    xtrasod
#endif

    return
    
route movf  Rx0d0,W
    movwf Cmdtemp
    movlw OPC_ACON
    subwf Cmdtemp,W ;is it an ON?
    bz    route1
    movlw OPC_ASON
    subwf Cmdtemp,W
    bnz   route4
    movlw OPC_ACON
    movwf Cmdtemp
    bra   route1
route4  movlw OPC_AREQ    ;is it a long request?
    subwf Cmdtemp,W
    bnz   route2
    movlw OPC_ARON    ;long response
    movwf Cmdtemp
    bra   route1
route2  movlw OPC_ASRQ
    subwf Cmdtemp,W
    bz    route3
    goto  main2
route3  movlw OPC_ARON    ;long response to a short trigger event
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

; Process an incoming event for inputs 1..8
; Rx0d0 bit 0 is 1 if ON, 0 if OFF
; EVtemp contains input number in low three bits

ss_in movlw OPC_ASRQ  ;only can poll bits with a short request
    subwf Rx0d0,W
    bz    ss_in3
    movlw OPC_AREQ  ;Ignore
    subwf Rx0d0,W
    bz    ss_inx

;Update push button state from incoming event

    movlw 0x01
    movwf EVtemp2   ;Save bitmask
    movf  EVtemp,W  ;Get switch number
    andlw 0x07    ;Mask out unwanted stuff
    bz    do_sw   ;Switch 1
    rlncf EVtemp2   ;Next bitmask
    decf  WREG,w    ;Check
    bz    do_sw   ;Switch 2
    rlncf EVtemp2   ;Next bitmask
    decf  WREG,w    ;Check
    bz    do_sw   ;Switch 3
    rlncf EVtemp2   ;Next bitmask
    decf  WREG,w    ;Check
    bz    do_sw   ;Switch 4
    rlncf EVtemp2   ;Next bitmask
    decf  WREG,w    ;Check
    bz    do_sw   ;Switch 5
    rlncf EVtemp2   ;Next bitmask
    decf  WREG,w    ;Check
    bz    do_sw   ;Switch 6
    rlncf EVtemp2   ;Next bitmask
    decf  WREG,w    ;Check
    bz    do_sw   ;Switch 7
    rlncf EVtemp2   ;Next bitmask
do_sw btfsc Rx0d0,0   ;Skip if ON event
    bra   do_sw0    ;if OFF event

;Set switch state with bitmask in EVtemp2 to 1
    movf  EVtemp2,W ;Get mask
    iorwf TogLast,F ;Set bit
    return

;Set switch state with bitmask in EVtemp2 to 0
do_sw0  comf  EVtemp2,W ;Get inverted mask
    andwf TogLast,F ;Clear bit
ss_inx  return

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
    movlw OPC_ARSOF
    movwf Tx1d0   ;an off state response
    bra   ss_in2
ss_low  movlw OPC_ARSON ;an on state response
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
    iorwf Tx1sidh,F   ;low priority
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
    movlw OPC_NVANS   ;NV answer
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
    
ensend  movlw OPC_ENRSP
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
    movlw OPC_ENRSP
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
    movlw OPC_NUMEV
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
    movlw OPC_NEVAL
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
    movlw OPC_PARAMS
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
    movlw OPC_NAME
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
    movlw OPC_PARAN
    movwf Tx1d0
    movlw 7   ;FLAGS index in nodeprm
    cpfseq  Temp
    bra   notFlags      
    call  getflags
    movwf Tx1d4
    bra   addflags
notFlags  
    movlw .14
    cpfseq  Temp
    bra   nxtparam
    call  getId1
    movwf Tx1d4
    bra   addflags
nxtparam
    movlw .15
    cpfseq  Temp
    bra   paramrd
    call  getId2
    movwf Tx1d4
    bra   addflags
paramrd
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
    movlw OPC_PARAN
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
    
;***********************************************************
;
;getDevId returnd DEVID2 and DEVID1 in PRODH and PRODL

getId1  call  getProdId
    movf  PRODL,w
    return
  
getId2  call  getProdId
    movf  PRODH,w
    return

getProdId
    movlw 0x3F
    movwf TBLPTRU
    movlw 0xFF
    movwf TBLPTRH
    movlw 0xFE
    movwf TBLPTRL
    bsf   EECON1, EEPGD
    tblrd*+
    movff TABLAT, PRODL
    tblrd*
    movff TABLAT, PRODH
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
    movlw OPC_CMDERR
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return

;**********************************************************************
;
#if AUTOID
;   self enumeration as separate subroutine
self_enA
    bcf   Datmode,MD_IDCONF     ;clear calling flag
#endif

self_en movff FSR1L,Fsr_tmp1Le  ;save FSR1 just in case
    movff FSR1H,Fsr_tmp1He 
    movlw B'11000000'
    movwf INTCON      ;start interrupts if not already started
    bsf   Datmode,MD_SETUP    ;set to 'setup' mode
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

self_en1
    btfss PIR2,TMR3IF   ;setup timer out?
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
    #ifdef CANTOTI
    de  B'11111111',.10 ;CANTOTI Delayed Input Mask/On time (100mS)
    de  .50,0     ;CANTOTI Off time (500mS)/Push Button Toggle Input
    #else
    de  B'00000000',.10 ;CANACE8C Delayed Input Mask/On time (100mS)
    de  .10,0     ;CANACE8C Off time (100mS)/Push Button Toggle Input
    #endif
    de  0,0       ;Expanded Mode (currently unused)
    de  0,0,0,0,0,0,0,0 ;Up to 16 NVs here if wanted
    
    ORG 0xF000FE
    de  0,0                 ;for boot.      
    end
