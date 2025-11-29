;                          MICROCHESS

;    COPYRIGHT 1976, PETER JENNINGS, MICROCHESS,
;    1612-43 THORNCLIFFE PK DR, TORONTO, CANADA.
;    ALL RIGHTS RESERVED.  REPRODUCTION BY   ANY
;    MEANS, IN WHOLE OR IN PART, IS PROHIBITED.
;
; CHANGES FOR APPLE II CONVERSION --- BY JEFF ROSENGARDEN
; 17-SEP-2018 Modified for TASM v3.0.1 assembler
;
;   TASM command line assembly:  tasm -g3 -c -65 jchess.asm
;   TASM flags being used:  -g3 = standard binary object file output
;                                 (binary file can be used directly on)
;                                 (Apple with BLOAD and BRUN commands)
;                           -c  = contiguous assembly with $00's filling empty spaces
;                           -65 = use standard 6502 op codes
;                    jchess.asm = source file for assembly
;
;   BINTOMON command line:        bintomon -l 0x1000 -r 0x1000 jchess.obj > jchess.mon
;   BINTOMON flags being used:    -l 0x1000 = load address
;                                 -r 0x1000 = run address
;                                jchess.obj = binary object file to convert to WOZ load file
;                              > jchess.mon = name of WOZ load file to create
; 18-SEP-2018 Apple II
;         Added APPLE II Constants
;         Added replacement for KIM-1 *OUT ROM routine (APPLEOUT)
;         Added replacement for KIM-1 *GETKEY ROM routine (APPLEIN)
;         Removed 3 lines of debounce code - not needed for APPLE II
; 19-SEP-2018 Apple II
;         Added JSR CROUT in strategic locations for proper screen formatting
;         Changes to APPLEOUT for proper formatting during game play
; 20-SEP-2018 Apple II
;         Added in greeting/copyright messages
;         Added in run-time ability to use opening book or not (PLAYOPN)
;         Added in generic display subroutine (DSP)
; 21-SEP-2018 Apple II
;         Moved message block from $2000 to $1500 just to trim up overall pgm
;         (might need to move it back again as I add more features to the pgm)
;         NOTE:  Moved again on 28-SEP-18 to $2500 to accomodate code relocation
;
; 23-SEP-2018 Apple II
;         Added OPNMOV subroutine for use with user choice on book opening
;         If user chooses Book Opening 1 thru 5 this routine will then move
;         the appropriate bytes for that book opening into the std book opening
;         address of $C0 thru $DB.  If the user chooses "0" for No Opening BOOK
;         then nothing will be moved into the std book opening memory range
;         and the computer will "think" up it's moves.
;
;
; 24-SEP-2018 Apple II
;         Finished adding opening books.  All 5 are now available
;         User can now select Level of Play (Easy, Medium, Difficult)
;
; 28-SEP-2018 Apple II
;         Relocated code so that no ZP memory blocks are declared in the code
;         Still using ZP memory locations as declared but all code is assembled
;         into 1 contiguous block from $1000 to $28F7 as of this last assembly
;         All original addresses have been commented out in case somebody desires to
;         re-assemble the program at it's originally intended address blocks for use
;         on the Apple I / Briel Replica I
;
;         NOTE:  Make sure to create a new Apple .bin file if the the end of the
;                code changes from $28F7.  
;                     1st boot up the emulator and paste the code in from the .mon file
;                     2nd do a BSAVE JCHESS.BIN,A$1000,L$18F7 (or whatever the new length is)
;                     3rd use CiderPress to move the JCHESS.BIN file where needed
;               ALTERNATIVELY:  The .obj file produced by the TASM assembler can
;                               be directly copied to a DOS disk image with CiderPress
;
;         NOTE:  Works on APPLE II, APPLE II+, APPLE IIE and APPLE IIE Enhanced
;                Will work on Apple I as long as the APPLE II monitor code is loaded
;                (I prefer at $7500) and then adjust the Apple II monitor routines (below)
;                accordingly.
;
;         NOTE:  DOS is no longer getting clobbered once the program is loaded into memory
;                Although it does get a little 'strange' after running the game and then
;                using a 3D0G to get back to AppleSoft...but everything still works.
;                Prior to the code relocation AND creating a contiguous .OBJ file
;                DOS was getting totally clobbered.
;
;         NOTE: Assemble with the following command;  TASM -g3 -c -65 jchess.asm
;                   -g3           generates a standard .obj binary file
;                   -c            populates zeros into unused address locations so the resulting
;                                 binary is contiguous
;                   -65           uses the 6502 op codes
;                   jchess.asm    the source code file
;
;         NOTE: Create the .mon file with; BINTOMON -l 0x1000 -r 0x1000 jchess.obj > jchess.mon
;
;
; 28-SEP-2018 Apple II
;         Stubbed out code/routines to display std chess notation.
;         Set up flag (DSPSTDN) to turn it on and off as needed
;         Set up MSG Block 3 / MSGH to display the std notation (pre-loaded with startup msg)
;         NOTE:  pre-loaded startup msg should never be seen.  If it is..something is wrong.
;
;
; 02-OCT-2018 Apple II
;         Completed CALCN routine for conversion of Microchess notation to std (algebraic) notation
;         * TODO:Need to clean this up (compact it more if possbible)
;         
;
; 02-OCT-2018 Apple II
;         Removed carriage return after pressing "F" (which locks in user's move)
;         Added carriage return right after the PC's move is displayed
;           thus retaining a full list of PC's moves & user's moves
;
;
; 02-OCT-2018 Apple II
;         Added ability for user to turn on/off std chess notation for the entire game
;
;
; 03-OCT-2018 Apple II
;         Fixed problem where proper piece wasn't showing in std chess notation for
;         user's move.  
;         Also stopped re-displaying move when user lock's their move (F) and now
;         displaying nothing in the std chess notation area.
;         Has the effect of grouping each pair of moves together...easy to see
;
;
; 04-OCT-2018 Apple II
;         Added CHECKCAP routine to display an "X" in the std chess notation
;         instead of a "-" when the computer is capturing a piece
;         CHECKCAP is called by the MV2: routine 
;         NOTE:  Works properly for book opening moves and non-book opening moves
;
;
; 06-OCT-2018 Apple II
;         Added CHECKCAP to the APPLEOUT: routine
;         Now displaying 'X' when either the computer (white)
;         or the user (black) capture a piece on their move. 
;         EXAMPLE: 0B1625 --> P(G7)xF6
;         The 1st 6 characters are Microchess's normal notation
;         The characters after the --> are std (algebraic) chess notation
;         This example shows that the Pawn on G7 is capturing the piece on F6
;
; 08-OCT-2018 Apple II
;         added code to handle castling.  Appropriate rook is adjusted to
;         proper square and Std Chess Notation shows the castle move as follows
;         King Side Castle = 0-0   Queen Side Castle = 0-0-0
;
;         added code to insure display is cleared out during startup
;         so a bogus move isn't displayed before game actually starts
;
;         added basic instructions before game starts
;         (found lots of threads on internet where people couldn't)
;         (figure out how to use Microchess.  Guess they didn't have)
;         (a copy of Peter's manual at their disposal!  :-)  )
;
;
; 10-OCT-2018 Apple II
;         added end-game "-->CHECKMATE (OR STALEMATE)" message so
;         user doesn't just see "FFFFFF"
;
; TODO:   
;         * Add character based chess board and update every move
;         * Add ability to check for illegal moves from user ??

;
;
;
; Started with Peter Jennings ORIGINAL, UNMODIFIED, version
; of Microchess for the Kim-1 (used the following original manual for reference)
;           MICROCHESS
;         A Chess Playing PROGRAM
;        For The 6502 Microcomputer
;             By Peter Jennings
; 
; Made many, many, many enhancements for this Apple II Port
; Was originally just under 1K on KIM and is now 1.8K on Apple

;assembler directives
  .LOCALLABELCHAR "?"   ;since we're using the underscore (_) to denote original constants

;ORIGINAL CONSTANTS 
_BOARD  = $0050
_BK     = $0060
_SETW   = $2000         ;$0070      relocated to $2000
_MOVEX  = $201F         ;$008F      relocated to $201F
_POINTS = $2040         ;$00A0      relocated to $2040
_PIECE  = $00B0
_SQUARE = $00B1
_SP2    = $00B2
_SP1    = $00B3
_INCHEK = $00B4
_STATE  = $00B5
_MOVEN  = $00B6
_OMOVE  = $00DC       
_OPNING = $00DC       
_WCAP0  = $00DD
_COUNT  = $00DE
_BCAP2  = $00DE
_WCAP2  = $00DF
_BCAP1  = $00E0
_WCAP1  = $00E1
_BCAP0  = $00E2
_MOB    = $00E3
_MAXC   = $00E4
_CC     = $00E5
_PCAP   = $00E6
_BMOB   = $00E3
_BMAXC  = $00E4
_BCC    = $00E5
_BMAXP  = $00E6
_XMAXC  = $00E8
_WMOB   = $00EB
_WMAXC  = $00EC
_WCC    = $00ED
_WMAXP  = $00EE
_PMOB   = $00EF
_PMAXC  = $00F0
_PCC    = $00F1
_PCP    = $00F2
_OLDKY  = $00F3
_BESTP  = $00FB
_BESTV  = $00FA
_BESTM  = $00F9
_DIS1   = $00FB
_DIS2   = $00FA
_DIS3   = $00F9

;APPLE MONITOR ROUTINES & CONSTANTS 
;NOTE:  Adjust these addresses if running on Apple I/Briel Replica 1
;       To correspond to wherever you loaded the Apple II monitor port
;       (I use the version compiled for $7500.  See Jeff Trantor's GITHub page(s))
;       (The proper addresses for the $7500 assembly are at the right of the comment)                 
CH      = $0024         ;HORIZONTAL SCREEN POSITION                 $0024   
CV      = $0025         ;VERTICAL SCREEN POSITION                   $025
CROUT   = $FD8E         ;PRINT CR TO DISPLAY                        $7D8E
COUT    = $FDF0         ;PRINT BYTE IN ACCUM TO DISPLAY             $7DED
PRBYTE  = $FDDA         ;PRINT BYTE IN ACCUM AS 2 HEX DIGITS        $7FFA 
RDKEY   = $FD0C         ;READ CHARACTER FROM KEYBOARD               $7D0C
HOME    = $FC58         ;CLEAR ENTIRE APPLE SCREEN                  $7C58
VTAB    = $FC22         ;VERTICALLY TAB N LINES; N IN ACCUM         $7C22

;NEW CONSTANTS 
OPNBOOK = $C0           ;address of opening book moves
PLAYOPN = $00B7         ;0=NO OPENING BOOK, 1=USE OPENING BOOK  
MADD    = $00B8         ;MESSAGE BLOCK ADDRESS; $00B8=LSB, $00B9=MSB  
TMPADDL = $00BA         ;temp storage of a 16 bit address - LSB
TMPADDH = $00BB         ;temp storage of a 16 bit address - MSB
DSPSTDN = $00BC         ;display std chess notation flag 0=NO 1=YES 2=NEVER
                        ;NO/YES used by computer to control std chess notation display
                        ;NEVER set by user to turn std chess notation on/off for entire game

LVL1LOC = $138E         ;1st loc to set Easy, Medium or Difficult Level
LVL2LOC = $1277         ;2nd loc to set Easy, Medium or Difficult Level
                            ;NOTE: Above 2 addresses change based on any code
                            ;changes.  Remember to change these 2 addresses accordingly 
                            ;in a final assembly (look at .lst to see final addresses)
                            ;since we're changing code on-the-fly during runtime
                            ;LVL1LOC has a comment of CHECK CHECK to search for
                            ;LVL2LOC has a comment of IF STATE=FB to search for
PCMOV   = $00BD         ;PC's Turn-To-Move Flag.  0=NO 1=YES
CAPX    = $00BE         ;Capture executed flag for use with STDN (std chess notation display)
                            ;if CAPX=1 then an 'x' will be displayed instead of a '-'
CASTLE  = $00BF         ;CASTLE move occurred flag
;                       ; 0 = no castling move occurred
;                       ; 1 = King side castling move occurred
;                       ; 2 = Queen side castling move occurred

MATECK  = $00F4         ; 0 = no check condition
                        ; 1 = check condition
                        ; 2 = check mate condition          


;       EXECUTION BEGINS AT ADDRESS $1000
;
;         +++
    *= $1000        

;general initialization
          LDA #$00
          STA PLAYOPN           ;INITIAL VALUE OF PLAYOPEN = 0 (NO BOOK)
          STA MADD              ;LSB OF MESSAGE BLOCK @ $2500
          STA DSPSTDN           ;TURN OFF DISPLAY OF STD NOTATION AT START
          STA CAPX              ;CLEAR OUT CAPTURE FLAG AT START
          STA MATECK            ;CLEAR OUT MATE FLAG 
          STA _BESTM            ;CLEAR OUT MICROCHESS TO, FROM, PIECE AT START
          STA _BESTV
          STA _BESTP
          
          LDA #$25          
          STA MADD+1            ;switch to 1st block of messages
          LDA #$00
          STA TMPADDL           ;LSB of Temporary address
          LDA #$21              ;  being loaded with starting address (LSB)
                                ;  of opening book moves memory block ($2100)
          STA TMPADDH           ;MSB of Temporary Address
                                ;  being loaded with starting address (MSB)
                                ;  of opening book moves memory block ($2100)

          LDA #$00              ;clear out memory where book opening will be
          LDX #$1B              ;copied to if user selects a book opening
ZERO:     STA OPNBOOK,X
          DEX
          BPL ZERO
                    
          JSR RHOME             ;CLEAR SCREEN
      
;greeting messages
          LDY #$02 \ LDX #$02   ;MESSAGE IN YREG, CARRIAGE RETURNS IN XREG
          JSR DSP               ;CALL GENERIC DISPLAY ROUTINE
          LDY #$03 \ LDX #$01
          JSR DSP 
          LDY #$04 \ LDX #$02
          JSR DSP 
          LDY #$05 \ LDX #$01
          JSR DSP 
          LDY #$06 \ LDX #$09
          JSR DSP 

      
;ask user if they want to use one of the book openings
          LDY #$01 \ LDX #$02
          JSR DSP 
          
          LDA #$00
          STA MADD
          LDA #$26
          STA MADD+1            ;switch to 2nd block of messages
      
          LDY #$07 \ LDX #$01   
          JSR DSP           
          LDY #$08 \ LDX #$01
          JSR DSP 
          LDY #$09 \ LDX #$01
          JSR DSP 
          LDY #$0A \ LDX #$01
          JSR DSP 
          LDY #$0B \ LDX #$01
          JSR DSP
          LDY #$0C \ LDX #$01
          JSR DSP
WAIT1:    JSR APPLEIN           ;get user input
          CMP #$B0
          BMI WAIT1             ;any key < 0 is bad key
          BEQ FIN               ;key = 0 no book opening desired
                                ;note: PLAYOPN already equals 0
          CMP #$B6
          BPL WAIT1             ; any key > 5 is bad key
          ;valid key...move on
          JSR NUMERIC           ;convert to normal hex digit
          TAY
          LDA #$01
          STA PLAYOPN           ;set flag indicating using book opening
          JSR OPNMOV            ;move selected book opening into place ($C0)
FIN:      JSR RHOME             ;clear the screen

;ask user for level of play desired
          LDA #$00
          STA MADD
          LDA #$27
          STA MADD+1            ;switch to 3rd block of messages
      
          LDY #$0D \ LDX #$01
          JSR DSP
          LDY #$0E  \ LDX #$01
          JSR DSP
          LDY #$0F  \ LDX #$01
          JSR DSP
          LDY #$10  \ LDX #$01
          JSR DSP
WAIT2:    JSR APPLEIN
          CMP #$B0              ;any key 0 < is bad key
          BMI WAIT2
          CMP #$B3              ;any key > 3 is bad key
          BPL WAIT2
          JSR NUMERIC           ;convert to normal hex digit
          CMP #$00
          BNE LVL01
LVL0:     LDA #$00              ;set EASY mode
          STA LVL1LOC
          LDA #$FF
          STA LVL2LOC
          JMP FIN2
LVL01:    CMP #$01
          BNE LVL02
          LDA #$00              ;set MEDIUM mode
          STA LVL1LOC
          LDA #$FB
          STA LVL2LOC
          JMP FIN2
LVL02:    LDA #$08              ;set DIFFICULT mode
          STA LVL1LOC
          LDA #$FB
          STA LVL2LOC
FIN2:     JSR RHOME
;
;

;ask user if they want to see std chess notation during game
          LDA #$00
          STA MADD
          LDA #$27
          STA MADD+1            ;switch to 3rd block of messages
      
          LDY #$12 \ LDX #$01
          JSR DSP

WAIT3:    JSR APPLEIN
          CMP #$B1              ;any key < 1 is bad key
          BMI WAIT2
          CMP #$B3              ;any key > 2 is bad key
          BPL WAIT2
          JSR NUMERIC           ;convert to normal hex digit
          STA DSPSTDN
          JSR RHOME
          
;give basic instructions
          LDA #$00
          STA MADD
          LDA #$28
          STA MADD+1            ;switch to 4th block of messages
          
          LDY #$13 \ LDX #$01
          JSR DSP
          LDY #$14  \ LDX #$02
          JSR DSP
          LDY #$15  \ LDX #$01
          JSR DSP
          LDY #$16  \ LDX #$02
          JSR DSP
          LDY #$17  \ LDX #$02
          JSR DSP
          LDY #$18  \ LDX #$02
          JSR DSP
          LDY #$19  \ LDX #$02
          JSR DSP
          LDY #$1A  \ LDX #04
          JSR DSP
          JSR APPLEIN           ;wait for user to press a key
          JSR RHOME
      
;
;
      
;original code starts here at the CHESS: label
;with alterations to accommodate the Apple port
;   
CHESS:    CLD                      ;INITIALIZE
          LDX     #$FF             ;TWO STACKS
          TXS
          LDX     #$C8
          STX     _SP2

;
;       ROUTINES TO LIGHT LED
;       DISPLAY AND GET KEY
;       FROM KEYBOARD.


;
OUT:      JSR       APPLEOUT       ;DISPLAY AND 

          LDX       PCMOV          ;IS IT THE PC'S MOVE?
          BEQ       OUT1           ;NO
          JSR       CROUT          ;YES - ISSUE CARRIAGE RETURN TO SCREEN
          LDX       #$00           
          STX       PCMOV          ;SET PCMOV FLAG TO INDICATE USER'S TURN
              
OUT1:     JSR       APPLEIN        ;GET INPUT   
;
          CMP       #$C3           ;[C]       
          BNE       NOSET          ;SET UP
          LDX       DSPSTDN
          CPX       #$02
          BEQ       T1
          LDX       #$00           ;INSURE STD CHESS NOTATION IS TURNED OFF
          STX       DSPSTDN
T1:       LDX       #$1F           ;BOARD
WHSET:    LDA      _SETW,x         ;FROM
          STA      _BOARD,x        ;SETW
          DEX
          BPL       WHSET
          STX      _OMOVE
      
          LDA       #$CC
          BNE       CLDSP
;
NOSET:    CMP       #$C5           ;[E]     
          BNE       NOREV          ;REVERSE
          LDX       DSPSTDN
          CPX       #$02
          BEQ       NOSET1
          LDX       #$00           ;INSURE STD CHESS NOTATION IS TURNED OFF
          STX       DSPSTDN
NOSET1:   JSR       REVERSE        ;BOARD AS
          LDA       #$EE           ;IS
          BNE       CLDSP
;
NOREV:    CMP       #$D0           ;[P]     
          BNE       NOGO           ;PLAY CHESS
          LDX       DSPSTDN
          CPX       #$02
          BEQ       NOREV1
          LDX       #$01
          STX       DSPSTDN        ;PLAYING CHESS SO TURN ON STD CHESS NOTATION
NOREV1:   LDX       #$01
          STX       PCMOV          ;PC'S TURN SO SET FLAG ON
          JSR       CROUT      
          JSR       GO
;
CLDSP:    STA       _DIS1          ;DISPLAY
          STA       _DIS2          ;ACROSS
          STA       _DIS3          ;DISPLAY
          JSR       CROUT     
          BNE       CHESS
;
NOGO:     CMP       #$C6           ;[F]       
          BNE       NOMV           ;MOVE MAN
          JSR       MOVE           ;AS ENTERED
          JSR       CROUT
          JMP       DISP
NOMV:     JSR       NUMERIC
          JMP       INPUT
;
;       THE ROUTINE JANUS DIRECTS THE
;       ANALYSIS BY DETERMINING WHAT
;       SHOULD OCCUR AFTER EACH MOVE
;       GENERATED BY GNM
;
;
;         +++
;     *= $0100
JANUS:    LDX       _STATE
          BMI       NOCOUNT
;
;       THIS ROUTINE COUNTS OCCURRENCES
;       IT DEPENDS UPON STATE TO INDEX
;       THE CORRECT COUNTERS
;
COUNTS:   LDA       _PIECE
          BEQ       OVER           ;IF STATE=8
          CPX       #$08             ;DO NOT COUNT
          BNE       OVER           ;BLK MAX CAP
          CMP       _BMAXP         ;MOVES FOR
          BEQ       XRT            ;WHITE
;
OVER:     INC       _MOB,X         ;MOBILITY
          CMP       #$01           ;+ QUEEN
          BNE       NOQ            ;FOR TWO
          INC       _MOB,X
;
NOQ:      BVC       NOCAP
          LDY       #$0F             ;CALCULATE
          LDA       _SQUARE        ;POINTS
ELOOP:    CMP       _BK,y          ;CAPTURED
          BEQ       FOUN           ;BY THIS
          DEY                      ;MOVE
          BPL       ELOOP
FOUN:     LDA       _POINTS,Y
          CMP       _MAXC,X
          BCC       LESS           ;SAVE IF
          STY       _PCAP,X        ;BEST THIS
          STA       _MAXC,X        ;STATE
;
LESS:     CLC
          PHP                      ;ADD TO
          ADC       _CC,X          ;CAPTURE
          STA       _CC,X          ;COUNTS
          PLP
;
NOCAP:    CPX       #$04
          BEQ       ON4
          BMI       TREE           ;(=00 ONLY)
XRT:      RTS
;
;       GENERATE FURTHER MOVES FOR COUNT
;       AND ANALYSIS
;
ON4:      LDA       _XMAXC         ;SAVE ACTUAL
          STA       _WCAP0         ;CAPTURE
          LDA       #$00           ;STATE=0
          STA       _STATE
          JSR       MOVE           ;GENERATE
          JSR       REVERSE        ;IMMEDIATE
          JSR       GNMZ           ;REPLY MOVES
          JSR       REVERSE
;
          LDA       #$08           ;STATE=8
          STA       _STATE         ;GENERATE
          JSR       GNM            ;CONTINUATION
          JSR       UMOVE          ;MOVES
;
          JMP       STRATGY        ;FINAL EVALUATION
NOCOUNT:  CPX       #$F9
          BNE       TREE
;
;       DETERMINE IF THE KING CAN BE
;       TAKEN, USED BY CHKCHK
;
          LDA       _BK            ;IS KING
          CMP       _SQUARE        ;IN CHECK?
          BNE       RETJ           ;SET INCHEK=0
          LDA       #$00           ;IF IT IS
          STA       _INCHEK
RETJ:     RTS
;
;       IF A PIECE HAS BEEN CAPTURED BY
;       A TRIAL MOVE, GENERATE REPLIES &
;       EVALUATE THE EXCHANGE GAIN/LOSS
;
TREE:     BVC       RETJ           ;NO CAP
          LDY       #$07           ;(PIECES)
          LDA       _SQUARE
LOOPX:    CMP       _BK,Y
          BEQ       FOUNX
          DEY
          BEQ       RETJ           ;(KING)
          BPL       LOOPX          ;SAVE
FOUNX:    LDA       _POINTS,Y      ;BEST CAP
          CMP       _BCAP0,X       ;AT THIS
          BCC       NOMAX          ;LEVEL
          STA       _BCAP0,X
NOMAX:    DEC       _STATE
          LDA       #$FB           ;IF STATE=FB
          CMP       _STATE         ;TIME TO TURN
          BEQ       UPTREE         ;AROUND
          JSR       GENRM          ;GENERATE FURTHER
UPTREE:   INC       _STATE         ;CAPTURES
          RTS
;
;       THE PLAYER'S MOVE IS INPUT
;
INPUT:    CMP       #$08           ;NOT A LEGAL
          BCS       ERROR          ;SQUARE #
          JSR       DISMV
DISP:     LDX       #$1F
SEARCH:   LDA       _BOARD,X
          CMP       _DIS2
          BEQ       HERE           ;DISPLAY
          DEX                      ;PIECE AT
          BPL       SEARCH         ;FROM
HERE:     STX       _DIS1          ;SQUARE
          STX       _PIECE
ERROR:    JMP       CHESS
;
;       GENERATE ALL MOVES FOR ONE
;       SIDE, CALL JANUS AFTER EACH
;       ONE FOR NEXT STEP
;
;         +++
;   *= $0200
GNMZ:     LDX       #$10           ;CLEAR
GNMX:     LDA       #$00           ;COUNTERS
CLEAR:    STA       _COUNT,X
          DEX
          BPL       CLEAR
;
GNM:      LDA       #$10           ;SET UP
          STA       _PIECE         ;PIECE
NEWP:     DEC       _PIECE         ;NEW PIECE
          BPL       NEX            ;ALL DONE?
          RTS                      ;-YES
;
NEX:      JSR       RESET          ;READY
          LDY       _PIECE         ;GET PIECE
          LDX       #$08
          STX       _MOVEN         ;COMMON START
          CPY       #$08             ;WHAT IS IT?
          BPL       PAWN           ;PAWN
          CPY       #$06
          BPL       KNIGHT         ;KNIGHT
          CPY       #$04
          BPL       BISHOP         ;BISHOP
          CPY       #$01
          BEQ       QUEEN          ;QUEEN
          BPL       ROOK           ;ROOK
;
KING:     JSR       SNGMV          ;MUST BE KING!
          BNE       KING           ;MOVES
          BEQ       NEWP           ;8 TO 1
QUEEN:    JSR       LINE
          BNE       QUEEN          ;MOVES
          BEQ       NEWP           ;8 TO 1
;
ROOK:     LDX       #$04
          STX       _MOVEN         ;MOVES
AGNR:     JSR       LINE           ;4 TO 1
          BNE       AGNR
          BEQ       NEWP
;
BISHOP:   JSR       LINE
          LDA       _MOVEN         ;MOVES
          CMP       #$04           ;8 TO 5
          BNE       BISHOP
          BEQ       NEWP
;
KNIGHT:   LDX       #$10
          STX       _MOVEN         ;MOVES
AGNN:     JSR       SNGMV          ;16 TO 9
          LDA       _MOVEN
          CMP       #$08
          BNE       AGNN
          BEQ       NEWP
;
PAWN:     LDX       #$06
          STX       _MOVEN
P1:       JSR       CMOVE         ;RIGHT CAP?
          BVC       P2
          BMI       P2
          JSR       JANUS         ;YES
P2:       JSR       RESET
          DEC       _MOVEN        ;LEFT CAP?
          LDA       _MOVEN
          CMP       #$05
          BEQ       P1
P3:       JSR       CMOVE         ;AHEAD
          BVS       NEWP          ;ILLEGAL
          BMI       NEWP
          JSR       JANUS
          LDA       _SQUARE        ;GETS TO
          AND       #$F0           ;3RD RANK?
          CMP       #$20
          BEQ       P3             ;DO DOUBLE
          JMP       NEWP
;
;       CALCULATE SINGLE STEP MOVES
;       FOR K, N
;
SNGMV:    JSR       CMOVE          ;CALC MOVE
          BMI       ILL1           ;-IF LEGAL
          JSR       JANUS          ;-EVALUATE
ILL1:     JSR       RESET
          DEC       _MOVEN
          RTS
;
;       CALCULATE ALL MOVES DOWN A
;       STRAIGHT LINE FOR Q,B,R
;
LINE:     JSR       CMOVE          ;CALC MOVE
          BCC       OVL            ;NO CHK
          BVC       LINE           ;CH,NOCAP
OVL:      BMI       ILL            ;RETURN
          PHP
          JSR       JANUS          ;EVALUATE POSN
          PLP
          BVC       LINE           ;NOT A CAP
ILL:      JSR       RESET          ;LINE STOPPED
          DEC       _MOVEN         ;NEXT DIR
          RTS
;
;       EXCHANGE SIDES FOR REPLY
;       ANALYSIS
;
REVERSE:  LDX       #$0F
ETC:      SEC
          LDY       _BK,X          ;SUBTRACT
          LDA       #$77           ;POSITION
          SBC       _BOARD,X       ;FROM 77
          STA       _BK,X
          STY       _BOARD,X       ;AND
          SEC
          LDA       #$77           ;EXCHANGE
          SBC       _BOARD,X       ;PIECES
          STA       _BOARD,X
          DEX
          BPL       ETC
          RTS
;
;
;
;
;
;
;
;        CMOVE CALCULATES THE TO SQUARE
;        USING .SQUARE AND THE MOVE
;        TABLE.  FLAGS SET AS FOLLOWS:
;        N - ILLEGAL MOVE
;        V - CAPTURE (LEGAL UNLESS IN CH)
;        C - ILLEGAL BECAUSE OF CHECK
;        [MY THANKS TO JIM BUTTERFIELD
;        WHO WROTE THIS MORE EFFICIENT
;        VERSION OF CMOVE]
;
CMOVE:    LDA       _SQUARE        ;GET SQUARE
          LDX       _MOVEN         ;MOVE POINTER
          CLC
          ADC       _MOVEX,X       ;MOVE LIST
          STA       _SQUARE        ;NEW POS'N
          AND       #$88
          BNE       ILLEGAL        ;OFF BOARD
          LDA       _SQUARE
;
          LDX       #$20
LOOP:     DEX                      ;IS TO
          BMI       NO             ;SQUARE
          CMP       _BOARD,X       ;OCCUPIED?
          BNE       LOOP
;
          CPX       #$10           ;BY SELF?
          BMI       ILLEGAL
;
          LDA       #$7F           ;MUST BE CAP!
          ADC       #$01           ;SET V FLAG
          BVS       SPX            ;(JMP)
;
NO:       CLV                      ;NO CAPTURE
;
SPX:      LDA       _STATE         ;SHOULD WE
          BMI       RETL           ;DO THE
          CMP       #$08           ;CHECK CHECK?
          BPL       RETL
;
;        CHKCHK REVERSES SIDES
;       AND LOOKS FOR A KING
;       CAPTURE TO INDICATE
;       ILLEGAL MOVE BECAUSE OF
;       CHECK.  SINCE THIS IS
;       TIME CONSUMING, IT IS NOT
;       ALWAYS DONE.
;
          PHA                      ;STATE
          PHP
          LDA       #$F9
          STA       _STATE         ;GENERATE
          STA       _INCHEK        ;ALL REPLY
          JSR       MOVE           ;MOVES TO
          JSR       REVERSE        ;SEE IF KING
          JSR       GNM            ;IS IN
          JSR       RUM            ;CHECK
          PLP
          PLA
          STA       _STATE
          LDA       _INCHEK
          BMI       RETL           ;NO - SAFE
          SEC                      ;YES - IN CHK
          LDA       #$FF
          RTS
;
RETL:     CLC                      ;LEGAL
          LDA       #$00           ;RETURN
          RTS
;
ILLEGAL:  LDA       #$FF
          CLC                      ;ILLEGAL
          CLV                      ;RETURN
          RTS
;
;       REPLACE .PIECE ON CORRECT .SQUARE
;
RESET:    LDX       _PIECE         ;GET LOCAT.
          LDA       _BOARD,X       ;FOR PIECE
          STA       _SQUARE        ;FROM BOARD
          RTS
;
;
;
GENRM:    JSR       MOVE           ;MAKE MOVE
GENR2:    JSR       REVERSE        ;REVERSE BOARD
          JSR       GNM            ;GENERATE MOVES
RUM:      JSR       REVERSE        ;REVERSE BACK
;
;       ROUTINE TO UNMAKE A MOVE MADE BY
;                MOVE
;
UMOVE:    TSX                      ;UNMAKE MOVE
          STX       _SP1
          LDX       _SP2           ;EXCHANGE
          TXS                      ;STACKS
          PLA                      ;MOVEN
          STA       _MOVEN
          PLA                      ;CAPTURED
          STA       _PIECE         ;PIECE
          TAX
          PLA                      ;FROM SQUARE
          STA       _BOARD,X
          PLA                      ;PIECE
          TAX
          PLA                      ;TO SQUARE
          STA       _SQUARE
          STA       _BOARD,X
          JMP       STRV
;
;       THIS ROUTINE MOVES .PIECE
;       TO .SQUARE,  PARAMETERS
;       ARE SAVED IN A STACK TO UNMAKE
;       THE MOVE LATER
;
MOVE:     TSX
          STX       _SP1           ;SWITCH
          LDX       _SP2           ;STACKS
          TXS
          LDA       _SQUARE
          PHA                      ;TO SQUARE
          TAY
          LDX       #$1F
CHECK:    CMP       _BOARD,X       ;CHECK FOR
          BEQ       TAKE           ;CAPTURE
          DEX
          BPL       CHECK
TAKE:     LDA       #$CC
          STA       _BOARD,X
          TXA                      ;CAPTURED
          PHA                      ;PIECE
          LDX       _PIECE
          LDA       _BOARD,X
          STY       _BOARD,X       ;FROM
          PHA                      ;SQUARE
          TXA
          PHA                      ;PIECE
          LDA       _MOVEN
          PHA                      ;MOVEN
STRV:     TSX
          STX       _SP2           ;SWITCH
          LDX       _SP1           ;STACKS
          TXS                      ;BACK
          RTS
;
;       CONTINUATION OF SUB STRATGY
;       -CHECKS FOR CHECK OR CHECKMATE
;       AND ASSIGNS VALUE TO MOVE
;
CKMATE:   LDX       _BMAXC         ;CAN BLK CAP
          CPX       _POINTS        ;MY KING?
          BNE       NOCHEK
          LDA       #$00             ;GULP!
          BEQ       RETV           ;DUMB MOVE!
;
NOCHEK:   LDX       _BMOB          ;IS BLACK
          BNE       RETV           ;UNABLE TO
          LDX       _WMAXP         ;MOVE AND
          BNE       RETV           ;KING IN CH?
          LDA       #$FF           ;YES! MATE
;
RETV:     LDX       #$04           ;RESTORE
          STX       _STATE         ;STATE=4
;
;       THE VALUE OF THE MOVE (IN ACC)
;       IS COMPARED TO THE BEST MOVE AND
;       REPLACES IT IF IT IS BETTER
;
PUSH:     CMP       _BESTV         ;IS THIS BEST
          BCC       RETP           ;MOVE SO FAR?
          BEQ       RETP
          STA       _BESTV         ;YES!
          LDA       _PIECE         ;SAVE IT
          STA       _BESTP
          LDA       _SQUARE
          STA       _BESTM         ;FLASH DISPLAY
RETP:     RTS                      ;AND RTS     ORIGINAL CODE LINE:  JMP *OUT
;
;       MAIN PROGRAM TO PLAY CHESS
;       PLAY FROM OPENING OR THINK
;
GO:       LDA       PLAYOPN        ;PLAYOPN = 0;NO BOOK
          CMP       #$00           ;PLAYOPN = 1; USE BOOK
          BEQ       NOOPEN         ;
          LDX       _OMOVE         ;OPENING?
          BPL       NOOPEN         ;-NO
          LDA       _DIS3          ;-YES WAS
          CMP       _OPNING,X      ;OPPONENT'S
          BNE       END            ;MOVE OK?
          DEX
          LDA       _OPNING,X      ;GET NEXT
          STA       _DIS1          ;CANNED
          DEX                      ;OPENING MOVE
          LDA       _OPNING,X
          STA       _DIS3          ;DISPLAY IT
          DEX
          STX       _OMOVE         ;MOVE IT
          BNE       MV2            ;(JMP)
;
END:      STA       _OMOVE         ;FLAG OPENING
NOOPEN:   LDX       #$0C           ;FINISHED
          STX       _STATE         ;STATE=C
          STX       _BESTV         ;CLEAR BESTV
          LDX       #$14           ;GENERATE P
          JSR       GNMX           ;MOVES
;
          LDX       #$04           ;STATE=4
          STX       _STATE         ;GENERATE AND
          JSR       GNMZ           ;TEST AVAILABLE
;                                  ;MOVES
;
          LDX       _BESTV         ;GET BEST MOVE
          CPX       #$0F           ;IF NONE
          BCC       MATE           ;OH OH!
;
MV2:      JSR       CHECKCAP       ;SET CAPTURE FLAG (CAPX) IF
                                   ;IF COMPUTER CAPTURED A PIECE 
                                   ;ON THIS MOVE                               
          LDX       _BESTP         ;MOVE
          LDA       _BOARD,X       ;THE
          STA       _BESTV         ;BEST
          STX       _PIECE         ;MOVE
          LDA       _BESTM
          STA       _SQUARE        ;AND DISPLAY
          JSR       MOVE           ;IT
          JMP       CHESS
;
MATE:     LDA       #$02           ;SET MATECK FLAG to $02
          STA       MATECK         ;INDICATING CHECKMATE/STALEMATE CONDITION
          LDA       #$FF           ;RESIGN
          RTS                      ;OR STALEMATE
;
;       SUBROUTINE TO ENTER THE
;       PLAYER'S MOVE
;
DISMV:    LDX       #$04           ;ROTATE
ROL:      ASL       _DIS3          ;KEY
          ROL       _DIS2          ;INTO
          DEX                      ;DISPLAY
          BNE       ROL
          ORA       _DIS3
          STA       _DIS3
          STA       _SQUARE
          RTS
;
;       THE FOLLOWING SUBROUTINE ASSIGNS
;       A VALUE TO THE MOVE UNDER
;       CONSIDERATION AND RETURNS IT IN
;         THE ACCUMULATOR
;
;         +++
;   *= $1780  DON'T NEED TO SPLIT CODE ON APPLE II
STRATGY:  CLC
          LDA       #$80
          ADC       _WMOB          ;PARAMETERS
          ADC       _WMAXC         ;WITH WEIGHT
          ADC       _WCC           ;OF 0_25
          ADC       _WCAP1
          ADC       _WCAP2
          SEC
          SBC       _PMAXC
          SBC       _PCC
          SBC       _BCAP0
          SBC       _BCAP1
          SBC       _BCAP2
          SBC       _PMOB
          SBC       _BMOB
          BCS       POS            ;UNDERFLOW
          LDA       #$00             ;PREVENTION
POS:      LSR   A
          CLC                      ;**************
          ADC        #$40
          ADC       _WMAXC         ;PARAMETERS
          ADC       _WCC           ;WITH WEIGHT
          SEC                      ;OF 0.5
          SBC       _BMAXC
          LSR       A              ;************
          CLC
          ADC       #$90
          ADC       _WCAP0         ;PARAMETERS
          ADC       _WCAP0         ;WITH WEIGHT
          ADC       _WCAP0         ;OF 1.0
          ADC       _WCAP0
          ADC       _WCAP1
          SEC                      ;[UNDER OR OVER-
          SBC       _BMAXC         ;FLOW MAY OCCUR
          SBC       _BMAXC         ;FROM THIS
          SBC       _BCC           ;SECTION]
          SBC       _BCC
          SBC       _BCAP1
          LDX       _SQUARE        ;************
          CPX       #$33
          BEQ       POSN           ;POSITION
          CPX       #$34             ;BONUS FOR
          BEQ       POSN           ;MOVE TO
          CPX       #$22             ;CENTRE
          BEQ       POSN           ;OR
          CPX       #$25             ;OUT OF
          BEQ       POSN           ;BACK RANK
          LDX       _PIECE
          BEQ       NOPOSN
          LDY       _BOARD,X
          CPY       #$10
          BPL       NOPOSN
POSN:     CLC
          ADC       #$02
NOPOSN:   JMP       CKMATE         ;CONTINUE
;
;
;

;
;   THE FOLLOWING SUBROUTINE REPLACES 
;   THE KIM-1 ROM *OUT ROUTINE.
;   
APPLEOUT: PHA                 ;SAVE ACCUMULATOR TO STACK
      LDA CV
      PHA                     ;SAVE CV TO STACK
      LDA CH
      PHA                     ;SAVE CH TO STACK
      LDA #$00  
      STA CH                  ;SET CH TO $00
;     
;   MIMIC KIM-1 6SEG LED DISPLAY
      LDA _BESTP                    
      JSR PRBYTE        
      LDA _BESTV
      JSR PRBYTE
      LDA _BESTM
      JSR PRBYTE
  
      JSR CHKCSTLE            ;check for castle move.  Set castle flag & adjust rook's
                              ;location if castling move took place
      JSR CHECKCAP            ;check for capture.  Set capture flag if so

      JSR STDN                ;display standard chess notation 
                              ;right next to the Microchess notation
;
      PLA
      STA CH                  ;RESTORE CH FROM STACK
      PLA 
      STA CV                  ;RESTORE CV FROM STACK
      PLA                     ;RESTORE ACCUMULATOR FROM STACK
      RTS                     ;RETURN
;
;


;**************************************************************
;* The following are all new sub-routines that are needed to  *
;* support this Apple ][ port of Microchess.                  *
;* Created by Jeff Rosengarden                                *
;**************************************************************

;
;   THE FOLLOWING SUBROUTINE REPLACES 
;   THE KIM-1 ROM *GETKEY ROUTINE.
;     
APPLEIN:  JSR RDKEY
          RTS
;
;

;
;   THE FOLLOWING SUBROUTINE MASKS OFF
;   BITS 7 THRU 4 TO ACCOUNT FOR THE APPLE
;   KEYCODES BEING RETURNED IN ORDER TO END
;   UP WITH JUST THE HEX DIGIT STRCUK (0 THRU 9) 
;   IN THE ACCUMULATOR
;     
NUMERIC:  AND #$0F            ;mask off high nibble for board notation entry (1 thru 7)
          RTS
;
;

;   THE FOLLOWING SUBROUTINE CLEARS THE SCREEN    
;  
RHOME:    JSR HOME
          LDA #$17
          STA CV
          JSR VTAB
          RTS
          
;
;
      
      
;   Generic Message Display Routine
;   Entry:  Message# In Y Register
;           # Of Carriage Returns In X Register
;   Exit:   X, Y And A All Scrambled
; 
DSP:      CLD
          LDA #$00            ;start with 0 in accumulator
DSP1:     DEY
          CPY #$1A            ;end of 5th message block
          BEQ DSP2            ;Y=$1A
          CPY #$12            ;end of 4th message block
          BEQ DSP2            ;Y=$12?
          CPY #$0C            ;end of 3rd message block
          BEQ DSP2            ;Y=$0C?
          CPY #$06            ;end of 2nd message block?
          BEQ DSP2            ;Y=$06?
          CPY #$00            ;end of 1st message block
          BEQ DSP2            ;Y = $00?
          CLC
          ADC #$1E            ;NO - Add 1 message offset to accum
    
                              ;NOTE: only 8 messages per block so
                              ;we insure a carry won't occur and the
                              ;offset result will always be one byte
                              ;$1E * 8 = $F0
          JMP DSP1      
DSP2:     TAY                 ;YES - Set Y Register to message offset
DSP3:     LDA (MADD),Y        ;load accum with next character of message
                              ;   using indirect indexed mode for parsing 
                              ;   a table starting at a 16 bit address
          CMP #$0A            ; is it the end-of-line character (\n)??
          BEQ DSP4            ;YES
          ORA #$80            ;NO - Turn on bit 7 (kills inverse/flash)
          JSR COUT            ;display normalized character on screen
          INY
          JMP DSP3            ;go get next character
DSP4:     CPX #$00            ;all characters displayed now push out CR's as needed
          BEQ DSPFIN          ;no more CR's req'd - get out of dodge
          JSR CROUT           ;push a CR to the screen
          DEX                 ;decrement our CR counter
          JMP DSP4            ;go back to possibly display more CR's
DSPFIN:   RTS

;   Opening Book Move - Move subroutine
;   ENTRY:  Y Register contains Opening Book ID
;         1 = Guioco Piano Opening
;         2 = 4 Knights - Spanish Opening
;         3 = Queen's Indian Opening
;         4 = Ruy Lopez Opening
;         5 = French Defence Opening
;   EXIT: All registers scrambled
;
OPNMOV:   LDA #$00            ;initially clear accum 
          CLC                 ;clear carry flag before ADC
OFFSET:   DEY                 ;correct offset for desired opening
          BMI MOVOPN          ;Y Reg negative (done with offset adjust)?
          BEQ MOVOPN          ;Zero flag set (done with offset adjust)?
          ADC #$20            ;NO - add next book opening offset of $20
          JMP OFFSET          ;jump back to OFFSET and do it again
MOVOPN:   TAY                 ;move calculated offset back into Y Reg
          LDX #$00            ;clear X Reg to start xferring opening moves bytes
MOVIT:    LDA (TMPADDL),Y     ;load accum with next byte for selected opening book
          STA OPNBOOK,X       ;store it at the std book opening ($00C0) address+offset
          INY                 ;point to next byte in opening book
          INX                 ;point to next storage location offset from $C0 
          CPX #$1C            ;X Reg = $1C yet?  (Opening Books are each $1B bytes long)
          BNE MOVIT           ;NO - go move next opening book move
          RTS                 ;YES - get out of dodge
          

;   Standard Chess Notation Display
;   ENTRY:  Nothing 
;   EXIT: All registers scrambled
; 
STDN:     ;FROM & TO Squares = $00 then no move made (suppress bogus move @ start)
          LDA _BESTV            
          BNE STDN1             
          LDA _BESTM            
          BEQ DONE              ;no move to display - get out of dodge        
          
STDN1:    LDA DSPSTDN           ;Display std chess notation turned on yet?
          BEQ DONE              ;NO - get out of dodge
          
          CMP #$02              ;User indicated never show std chess notation?
          BEQ DONE              ;YES - get out of dodge
          
          LDA MATECK
          CMP #$02              ;checkmate or stalemate condition?
          BNE STDN2             ;NO - continue like normal
          
          LDA #$00
          STA MADD
          LDA #$29
          STA MADD+1            ;switch to 5th block of messages
          
          LDY #$1B              ;point to MSR (in block 5 of text messages)
          LDX #$04              ;push out $04 carriage returns
          JSR DSP               ;display it
          JMP DONE
          
STDN2:    LDA _BESTP
          CMP #$FF              ;Is piece = $FF?  (user locked his move with 'F')
          BEQ DONE              ;YES - so nothing new to display - get out of dodge 
          
          LDA #$00
          STA MADD
          LDA #$27
          STA MADD+1            ;switch to 3rd block of messages        
          
          JSR CLRM              ;clear outbound msg (holds std chess notation of move)
          JSR CALCN             ;routine to calculate std chess notation
                                ;from Microchess notation and load the text
                                ;into MSGH
                                
          LDY #$11              ;point to MSGH (in block 3 of text messages)
          LDX #$00              ;we want no add'l carriage returns
          JSR DSP               ;display it
          
DONE:     RTS

;   Calculate standard (algebraic) chess notation
;   ENTRY:  Nothing (all req'd info is in _BESTP, _BESTV, _BESTM)
;             _BESTP = Piece being moved
;             _BESTV = From square in Microchess notation
;             _BESTM = To square in Microchess notation
;   EXIT: MSG Block #3 / MSGH is loaded with the std chess notation text
; 
CALCN:    LDY _BESTP            ;get piece being moved
                            
                                ;always strip MSB so we only have
                                ;the actual piece (0-F) in Y Reg
          TYA                   ;temporarily move piece into accum
          AND #00001111b        ;strip off MSB (to insure it's 0)
          TAY                   ;move piece back to Y Reg
          
          LDA CMAN,Y            ;lookup std piece notation
          STA MSGH+5            ;stuff it into the outbound message
          
          LDA #$A8              ;load accum with '('
          STA MSGH+6            ;stuff it into the outbound message
          
          LDA _BESTV            ;get FROM square
          AND #11110000b        ;mask off Microchess column info
          CLC                   ;clear carry flag so our 4 ROR's don't get messed up
          ROR A
          ROR A
          ROR A
          ROR A                 ;shift Microchess row info into bit 3 thru bit 0
          TAY                   ;xfer the Microchess row info into the Y Reg
          LDA ROWINFO,Y         ;convert Microchess Row info to algebraic row info
          ADC #$B0              ;convert it to Apple's ascii 
          STA MSGH+8            ;stuff it into the outbound message
                                ;NOTE:  Row is displayed AFTER Col in std (algebraic) notation
          
          LDA _BESTV            ;get FROM square
          AND #00001111b        ;mask off Microchess row info             
          TAY                   ;xfer the Microchess column info into the Y Reg
          LDA COLINFO,Y         ;convert Microchess Column info to algebraic column info
          STA MSGH+7            ;stuff it into the outbound message
                                ;NOTE:  Col is displayed BEFORE Row in std (algebraic) notation
          
          LDA #$A9              ;load accum with ')'
          STA MSGH+9            ;stuff it into the outbound message

          LDA CAPX              ;get capture flag (CAPX)
          BEQ CALCN1            ;CAPX = 0 ???
          LDA #$00              ;NO
          STA CAPX              ;so reset capture flag to $00 for next move
          LDA #$D8              ;load accum with 'X'
          JMP CALCN2
CALCN1:   LDA #$AD              ;load accum with '-'
CALCN2:   STA MSGH+10           ;stuff it into the outbound message
          
          LDA _BESTM            ;get TO square
          AND #11110000b        ;mask off Microchess column info
          CLC                   ;clear carry flag so our 4 ROR's don't get messed up
          ROR A
          ROR A
          ROR A
          ROR A                 ;shift Microchess row infor into bit 3 thru bit 0
          TAY                   ;xfer the Microchess row info into the Y Reg
          LDA ROWINFO,Y         ;convert Microchess Row info to algebraic row info
          ADC #$B0              ;convert it to Apple's ascii
          STA MSGH+12           ;stuff it into the outbound message
          
          LDA _BESTM            ;get TO square
          AND #00001111b        ;mask off Microchess row infoo  
          TAY                   ;xfer the Microchess column info into the Y Reg
          LDA COLINFO,Y         ;convert Microchess Column info to algebraic column info
          STA MSGH+11           ;stuff it into the outbound message
          
          ;check for castle move here
          LDA CASTLE            ;check for castle move
          BEQ CALCNFIN          ;CASTLE = 0? then no castle move took place so exit
          CMP #$01              ;CASTLE = 1? then king side castle took place 
          BNE CHKQ              ;NO - then queen side castle took place         
          ;code for king side castle
          LDA #$B0              ;display King side castle notation; 0-0
          STA MSGH+14
          LDA #$AD
          STA MSGH+15
          LDA #$B0
          STA MSGH+16
          JMP CLRCSTLE          ;done 
CHKQ:     ;code for queen side castle
          LDA #$B0              ;display Queen side castle notation; 0-0-0
          STA MSGH+14
          LDA #$AD
          STA MSGH+15
          LDA #$B0
          STA MSGH+16
          LDA #$AD
          STA MSGH+17
          LDA #$B0
          STA MSGH+18
CLRCSTLE: LDA #$00
          STA CASTLE            ;clear castle flag
CALCNFIN: RTS
;
;


;   Clear Std Chess Notation Message out
;   ENTRY:  nothing
;   EXIT:   nothing
;           
CLRM:     LDA #$20              ;space character
          LDY #$05
CLRM1:    STA MSGH,Y
          INY
          CPY #$1C
          BNE CLRM1
          RTS
;
;


;   Check for a capture on the TO square (_BESTM)
;   ENTRY:  Nothing (all req'd info is in _BESTP, _BESTV, _BESTM, _BK and _BOARD)
;             _BESTP  = Piece being moved
;             _BESTV  = FROM square
;             _BESTM  = TO square
;             _BK     = starting address of black pieces and the square they occupy
;             _BOARD  = starting address of white pieces and the square they occupy
;
;             NOTE:   routine evaluates MSB of _BESTM
;                       if it equals 1 then the computer (white) is moving
;                       if it equals 0 then the user (black) is moving
;                       The appropriate pieces are then checked to see if a
;                       capture took place (TO square already occupied by opposite side)
;   EXIT: CAPX set to 1 (capture took place) or 0 (no capture took place)
;           
CHECKCAP: ;if _BESTP = to $CC, $EE, $00 or $FF no need to check for capture - exit 
          LDA #$CC
          CMP _BESTP
          BEQ FIN1
          LDA #$EE
          CMP _BESTP
          BEQ FIN1
          LDA #$00
          CMP _BESTP
          BEQ FIN1
          LDA #$FF
          CMP _BESTP
          BEQ FIN1
      
          ;getting setup for CHECKCAP main routine
          LDA #$00
          STA TMPADDH           ;MSB of $_BK or $_BOARD is always $00
          TAY                   ;Y Reg is offset into _BK so start it at $00
          LDA _BESTP            ;retrieve piece being moved
          AND #11110000b        ;strip off LSB leaving only 0(Black Moving) or 1(White Moving)
          BEQ LWHITE            ;is white moving?
          LDA #_BOARD           ;NO - load starting address of black piece squares
          JMP CMP0
LWHITE:   LDA #_BK              ;YES - load starting address of white piece squares
CMP0:     STA TMPADDL           ;and store into TMPADDL for (indirect),Y addressing

          ;main CHECKCAP routine
CMP1:     LDA (TMPADDL),Y       ;get square of next piece
          CMP _BESTM            ;compare it to square being moved TO
          BEQ DIDCAP            ;was there a capture (the TO square already occupied)?
          INY                   ;NO - increment our offset into the pieces
          CPY #$10              ;have we checked all $0F pieces???
          BNE CMP1              ;NO - go get next piece's square
          JMP FIN1              ;get out of dodge
DIDCAP:   LDA #$01              ;capture took place 
          STA CAPX              ;so set capture flag (CAPX) to $01
FIN1:     RTS
;
;


;   Check for a castling move
;   ENTRY:  Nothing (all req'd info is in _BESTP, _BESTV, _BESTM, _BK and _BOARD)
;             _BESTP  = Piece being moved
;             _BESTV  = FROM square
;             _BESTM  = TO square
;             _BK     = starting address of black pieces and the square they occupy
;             _BOARD  = starting address of white pieces and the square they occupy
;
;             NOTE:   routine checks to see if a castle move took place
;                     if so then the rook is adjusted appropriately and
;                     the castle flag (CASTLE) is set so STDN can
;                     display the castle move.
;                     CASTLE set to 0 = no castle move took place
;                     CASTLE set to 1 = King Side Castle took place
;                     CASTLE set to 2 = Queen Side Castle took place
;   EXIT: Rook adjusted appropriatly and castle flag (CASTLE) set appropriately
;           
CHKCSTLE: ;check for castle, adjust rook, set castle flag (CASTLE)

          ;1st check to see if king even involved in this move
          LDA _BESTP            ;get piece being moved
          AND #00001111b        ;strip off MSB so all that's left is the piece ID
          BNE CCASTLE           ;Piece ID <> 0 (KING) then exit 
          
          ;now check FROM square to make sure it's a castle move
          LDA _BESTV
          CMP #$73              ;is FROM square BLACK KING home?
          BEQ CHECKTO           ;YES - now check square it moved to
          CMP #$03              ;NO - is FROM square WHITE KING home?
          BEQ CHECKTO           ;YES - now check square it moved to
          JMP CCASTLE           ;NO - No Castle Move so exit
          
          ;now check TO square to make sure it's a castle move
CHECKTO:  LDA _BESTM
          CMP #$71              ;BLACK King side castle
          BNE C1
          ;black king side castle
          LDA #$72              ;adjust rook
          STA $62               ;for BLACK King side castle
          JMP KCASTLE           ;set CASTLE flag on way out
C1:       CMP #$76              ;BLACK Queen side castle
          BNE C2
          ;black queen side castle
          LDA #$75              ;adjust rook
          STA $63               ;for BLACK Queen side castle
          JMP QCASTLE           ;set CASTLE flag on way out
C2:       CMP #$01              ;WHITE King side castle
          BNE C3
          ;white king side castle
          LDA #$02              ;adjust rook
          STA $52               ;for WHITE King side castle
          JMP KCASTLE           ;set CASTLE flag on way out
C3:       CMP #$06              ;WHITE Queen side castle
          BNE CCASTLE
          ;white queen side castle
          LDA #$05              ;adjust rook
          STA $53               ;for WHITE Queen side castle
          JMP QCASTLE           ;set CASTLE flag on way out
          
          ;exit routines
CCASTLE:  LDA #$00              ;no castle - clear castle flag on way out
          STA CASTLE
          JMP FINC
KCASTLE:  LDA #$01              ;king side castle - set castle flag on way out
          STA CASTLE
          JMP FINC
QCASTLE:  LDA #$02              ;queen side castle - set castle flag on way out
          STA CASTLE    
FINC:     RTS                   ;done - get out of dodge



;ORIGINAL BLOCK DATA
  ;initial piece locations
; *= $0070
  *= $2000                    ;relocation address to avoid using ZP block (which clobbers DOS)

      .byte   $03, $04, $00, $07, $02, $05, $01, $06, $10, $17, $11, $16, $12,$15, $14, $13
      .byte   $73, $74, $70, $77, $72, $75, $71, $76, $60, $67, $51, $66, $52, $65, $64, $63
  
  ;table of move directions (_MOVEX)
; *= $0090
  *= $2020                    ;relocation address to avoid using ZP block (which clobbers DOS)

      .byte    $F0, $FF, $01, $10, $11, $0F, $EF, $F1, $DF, $E1, $EE, $F2, $12, $0E, $1F, $21
     
  ;table of piece values (_POINTS)   
; *= $00A0
  *= $2040                    ;relocation address to avoid using ZP block (which clobbers DOS)

      .byte    $0B, $0A, $06, $06, $04, $04, $04, $04, $02, $02, $02, $02, $02, $02, $02, $02
      
;NEW BLOCK DATA

  ;data for opening book move - Guioco Piano (_OPNING)
; *= $0100
  *= $2100                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
      .byte    $99, $25, $0B, $25, $01, $00, $33, $25, $07, $36, $34, $0D, $34, $34, $0E, $52
      .byte    $25, $0D, $45, $35, $04, $55, $22, $06, $43, $33, $0F, $CC

  ;data for opening book move - Four Knights Spanish (_OPNING)
; *= $0120
  *= $2120                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
      .byte   $99, $01, $00, $54, $23, $0C, $23, $23, $05, $55, $55, $04, $71, $24, $0E, $45 
      .byte   $46, $04, $52, $25, $07, $55, $22, $06, $43, $33, $0F, $CC

  ;data for opening book move - Queen's Indian (_OPNING)    
; *= $0140
  *= $2140                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
      .byte   $99, $25, $01, $25, $15, $01, $33, $25, $07, $71, $01, $00, $63, $11, $04, $66 
      .byte   $21, $0A, $56, $22, $06, $53, $35, $0D, $52, $34, $0E, $CC
      
  ;data for opening book move - Ruy Lopez (_OPNING)   
; *= $0160
  *= $2160                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
      .byte   $99, $20, $08, $71, $25, $0D, $54, $26, $04, $46, $03, $02, $63, $01, $00, $52
      .byte   $37, $04, $57, $46, $04, $55, $22, $06, $43, $33, $0F, $CC 

  ;data for opening book move - French Defence (_OPNING)    
; *= $0180
  *= $2180                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
      .byte   $99, $22, $06, $45, $32, $0C, $72, $14, $01, $63, $63, $05, $64, $43, $0F, $63
      .byte   $41, $05, $52, $25, $07, $44, $34, $0E, $53, $33, $0F, $CC

;
;     
;Chess Piece Conversion table (Apple specific ASCII - non flashing, non inverse)
;
;Microchess-  0    1    2    3    4    5    6    7    8     9     A     B     C     D     E    F
;             K    Q    R(K) R(Q) B(K) B(Q) N(K) N(Q) P(KR) P(QR) P(KN) P(QN) P(KB) P(QB) P(Q) P(K) 
CMAN  .byte   $CB, $D1, $D2, $D2, $C2, $C2, $CE, $CE, $D0,  $D0,  $D0,  $D0,  $D0,  $D0,  $D0, $D0
;
;

;Row conversion table from Microchess notation to Algebraic notation
;
;Microchess-    0    1    2    3    4    5    6    7    NOTE:  Row info is 1st digit of from/to
ROWINFO .byte $08, $07, $06, $05, $04, $03, $02, $01
;
;

;Col conversion table from Microchess notation to Algebraic notation
;
;Microchess-    0    1    2    3    4    5    6    7    NOTE:  Col info is 2nd digit of from/to
;Apple Ascii-   A    B    C    D    E    F    G    H
COLINFO .byte $C1, $C2, $C3, $C4, $C5, $C6, $C7, $C8
;
;
;EXAMPLES:    Microchess:  0F1333   =   P(D7)-D5
;             Microchess:  060122   =   N(B8)-C6
;             Microchess:  050541   =   B(F8)-B4
;
;
      
;
;
; Message Block 
; All Messages Have To Be $1E (30d) Characters Long
; For Generic Display Routine (DSP) To Work Properly
; All Strings Need To Be Terminated With \n (Counts As 1 Character)
; Each Block Can Only Have 8 Messages 
; Declare New Blocks At Even Pages; $2600, $2700, $2800, Etc.
;
; NOTE:   This would have been easier if TASM had a .lobyte/.hibyte assembler directive
;     so the message block could be directly loaded into MADD (LSB) and MADD+1 (MSB)
;     for each individual message instead of having to index thru the entire block.
;     the CC65 Assembler DOES have these assembler directives but I'm not going to
;     convert this to yet another assembler at this point!  
; *= $1500
  *= $2500                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
MSG1: .text "SELECT AN OPENING BOOK \n      "
MSG2: .text "MICROCHESS (C)\n               "
MSG3: .text "BY PETER JENNINGS\n            "
MSG4: .text "WWW.BENLO.COM\n                "
MSG5: .text "(MODS FOR THE APPLE II)\n      "
MSG6: .text "(BY JEFF ROSENGARDEN)\n        "

; *= $1600
  *= $2600                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
MSG7: .text "0 = NO OPENING BOOK \n         "
MSG8: .text "1 = GUIOCO PIANO OPENING \n    "
MSG9: .text "2 = 4 KNIGHTS - SPANISH \n     "
MSGA: .text "3 = QUEEN'S INDIAN OPENING \n  "
MSGB: .text "4 = RUY LOPEZ OPENING \n       "
MSGC: .text "5 = FRENCH DEFENCE OPENING \n  "

; *= $1700
  *= $2700                    ;relocation address to avoid using ZP block (which clobbers DOS)
  
MSGD: .text "SELECT LEVEL OF PLAY \n        "
MSGE: .text "0 = EASY \n                    "
MSGF: .text "1 = MEDIUM \n                  "
MSGG: .text "2 = DIFFICULT \n               "
MSGH: .text " --> STD CHESS NOTATION HERE \n"     ;this is the msg being used for standard
                                                  ;chess notation printed right alongside
                                                  ;Microchess's normal notation like this--
                                                  ;0F1333 --> P(KP2) - KP4
                                                  
MSGI: .text "DISPLAY STD CHESS NOTATION? (Y=1,2=N)\n"
;MSGI 'breaks' the rules by being too long but it's ok
;since there is plenty of room in MSG BLOCK 3 to keep it
;within $FF characters

; *= $1800
  *= $2800                    ;relocation address to avoid using ZP block (which clobbers DOS)

MSGJ: .text "PRESS C AT ANY TIME TO START \n"
MSGK  .text "A NEW GAME                   \n"
MSGL  .text "PRESS P TO INSTRUCT COMPUTER \n"
MSGM  .text "TO MOVE (WHITE MOVES 1ST)    \n"
MSGN  .text "PRESS F TO LOCK IN YOUR MOVE \n"
MSGO  .text "PRESS E TO SWITCH SIDES      \n"
MSGP  .text "COMPUTER=WHITE, PLAYER=BLACK \n"
MSGQ  .text "ANY KEY TO CONTINUE THEN C TO START \n"
;MSGQ 'breaks' the rules by being too long but it's OK
;since there is enough room in MSG BLOCK4 to keep it
;within $FF characters

; *= $1800
  *= $2900                    ;relocation address to avoid using ZP block (which clobbers DOS)

MSGR: .text " --> CHECKMATE (OR STALEMATE)\n"

      .END



; NOTE THAT 00B7 TO 00BF, 00F4 TO 00F8, AND 00FC TO 00FF ARE
; AVAILABLE FOR USER EXPANSION AND I/O ROUTINES.

;02-OCT-2018 USED ABOVE AVAILABLE ZP LOCATIONS 
;(00B7, 00B8 and 00B9, 00BA, 00BB, 00BC, 00BD, 00BE, 00BF, 00F4)
;
;