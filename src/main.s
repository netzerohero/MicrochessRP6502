.export _init, _exit
.export __STARTUP__ : absolute = 1
;.export  SNDCHR, RCCHR
.export  syschout,  syskin 
.import  CHESS   ; import MICROCHESS startup vector

.include "rp6502.inc"

.segment "CODE"

; Entry point
_init:
    ; 6502 doesn't reset these
    ldx #$FF
    txs
    cld

; Print "Banner / Start-up Message" message
    ldx #0
@loop:
    lda message,x
    beq @done           ; If zero, we're done
@wait:
    bit RIA_READY       ; Waiting on UART tx ready
    bpl @wait
    sta RIA_TX          ; Transmit the byte
    inx
    bne @loop           ; Continue loop
@done:
    ; Jump to MICROCHESS startup 
    jmp CHESS 
    ; not-reached 

; Halts the 6502 by pulling RESB low
_exit:
    lda #RIA_OP_EXIT
    sta RIA_OP
    ; not-reached




; Get a character from simulated ACIA / keyboard
; Remove RTS at bottom to run into char-out to provide screen-echo;
;  or: change JMP to JSR in jump-table for same echo effect.
;
syskin:
;RCCHR:
;ACIAin:
      BIT   RIA_READY
;     BVC   ACIAin            ; loop until a valid key-press char-byte is rcvd
;     BVC   RCCHR             ; loop until a valid key-press char-byte is rcvd
      BVC   syskin            ; loop until a valid key-press char-byte is rcvd
      LDA   RIA_RX            ; get char-byte from simulated ACIA
                      ;consider masking hi-bit to ensure a valid-ASCII char
;     and #%01111111          ; Clear high bit to be valid ASCII
;
      RTS ; remove-me for local-echo and run into char-out below...


; Send a character out to simulated ACIA / screen-terminal
;
syschout:
;SNDCHR:
;ACIAout:
; First filter characters before sending to ACIA
    sta $FE                   ; Save the character to be printed
    cmp #$FF                  ; Check for a bunch of characters
    BEQ EXSC                  ; that we don't want to print to
    cmp #$00                  ; the terminal and discard them to
    beq EXSC                  ; clean up the output
    cmp #$91                  ;
    beq EXSC                  ;
    cmp #$93                  ;
    beq EXSC                  ;
    cmp #$80                  ;
    beq EXSC 
GETSTS:
    BIT   RIA_READY
    BPL   GETSTS              ; wait for FIFO
    lda   $FE                 ; Restore the character
    STA   RIA_TX              ; save byte to simulated ACIA - SEND CHARACTER
EXSC:
    rts                       ; Return



.segment "RODATA"

;
; The message blocks; must terminates with a 00.
;
message:
         .byte  $0C ; Formfeed - clear the screen
         .byte "Entering...", $0D, $0A
;        .byte 0

MBLK:
         .byte  "MICROCHESS - Copyright 1976-2005 Peter Jennings, www.benlo.com"
         .byte  $0D, $0A
         .byte  "RP6502 ver: 20251129_HHMM"
         .byte  $0D, $0A, 0


