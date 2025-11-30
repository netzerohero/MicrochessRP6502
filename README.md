# Microchess for the RP6502 SBC

Peter Jennings' Microchess for the RP6502 SBC.

Reference: www.benlo.com/microchess/.

See the User Manual in the 'docs' sub-directory for more information.

## Quick-start information derived from Daryl Rictor's Aug-2002 Readme:

Microchess was developed in 1976 originally for the KIM-1. Later versions 
included 1.5 ported to the Commodore PET and Commodore Chessmate; and 2.0 
for the Apple II and the Tandy Color Computer series.  A pdf of a 
scanned copy of the original source code is included in the reference_code directory 
for those who wish to study the program's logic and strategy.


To play, load the program into the RP6502 SBC as any other executable.  The program's 
starting address's is $0200 and is just under 1.5k.  (Best to inspect the mapfile for
addresses just to be sure.)

On startup, you'll see a welcome-banner. Note, the board was designed
to be viewed using a white text on dark background.  Therefore, the empty squares
that are white will have ** in them, while the black squares are filled with 
spaces.  Hopefully, this won't confuse everyone.  If it does, then feel free to
alter the code just before label 'POUT25' to reverse the characters.

Commands are:
- C - clear and restart the game (when entered, CC CC CC will appear)
- E - toggle sides (when entered, EE EE EE will appear) 
- P - tell the computer to "Play Chess".  By default, the computer will play white.
    To play black, after clearing the game, press E to switch sides, enter your 
    move (as described below), and then press P.  
- [Enter] - give the computer YOUR move
- Q - Quit the Game (JMP's to address $E800 (the SBC monitor entry point)

You must clear (i.e. reset) the game when you play it for the first time 
in a session.  The game is not cleared automatically for you when you start 
the program. 

The computer will display its move in ax yy zz format, where 
'a' is the player (0 if computer, 1 if yours)
'x' is the chess piece (zero-page memory locations of your pieces and the computer's 
    pieces in parentheses): 

    0: King            (you, 0060; computer, 0050)
    1: Queen           (you, 0061; computer, 0051)
    2: King's Rook     (you, 0062; computer, 0052)
    3: Queen's Rook     :
    4: King's Bishop    :
    5: Queen's Bishop   :
    6: King's Knight    :
    7: Queen's Knight   :
    8: K R Pawn         :
    9: Q R Pawn         :
    A: K N Pawn         :
    B: Q N Pawn         :
    C: K B Pawn         :
    D: Q B Pawn         :
    E: Q Pawn           :
    F: K Pawn          (you, 006F; computer, 005F)

'yy' and 'zz' are the from and to square respectively. Each ranges from 
    00 to 77, with 00 being the computer's queen rook, and 77 being your 
    king rook.  

For example, the move 0F 13 33 means King's Pawn from King's Pawn 2 to King's 
Pawn 4 (white).  The computer may take some time to compute its move and will 
print dots (.) as it thinks.  

To enter your move, enter FROM and TO locations (piece is not needed). 
The computer verifies your piece by showing what is on that FROM square; 
if it is one of your men, it will start with a 1. (An FF shows no piece 
there.) For example, should you key 63 43 and the screen reads 1F 63 43, 
that means KIM thinks your King's Pawn is there and that you're advancing 
it two spaces. When ready, press <Enter> to enter the move, and then 'P' 
to tell the computer to play.  The legality of your moves is never verified, 
and you may make multiple moves on a single turn -- as long as you press 
<Enter> after each one -- before pressing 'P' (in fact, as you'll see 
below, for special moves you will have to). You may move the computer's 
men at any time as well. The computer also does not warn you if you are 
in check, and its strategy expects that you will move out of check when 
you are placed in it. 

Castling is accomplished by moving the king, then rook, then pressing 'P'. 
If the computer signals a castle by moving its king two spaces over, you 
will need to also move its rook for it. 

While you of course can capture en passant by making the appropriate 
lateral capture and moving forward, the computer does not know how and 
will not construct its strategy with it in mind. 

Queening pawns must be done manually by altering the game board image 
from the SBC monitor. Stop the game with 'Q', remove the queened pawn by 
entering CC in its location (see table above) and set your Queen at 0061 
to this queened pawn. After adjustment, restart the game. 
As only one Queen can be on the board at once, if you still have 
a Queen you must select some other captured piece and then move that as if 
it were a queen. The computer will also not autopromote its queened pawns, 
so you'll have to do that as well. 

The computer will resign if it ends up in checkmate or stalemate; the display 
will read 'ff ff ff'.  You are, of course, expected to show the same courtesy 
when you are checkmated, and restart the game. 

\[Edit: addresses below need updating for the RP6502 SBC; consult mapfile.\]

There are a few points of adjustment for skill level. By default, 08 at $11F5
 and FB at $10DE indicates normal mode with an average time per move of ~100s. 
If you are an impatient or poor player, try 00/FB for ~10 seconds "Blitz", 
or 00/FF for ~3 seconds "Super-Blitz".  Of course, the computer's ability 
to analyse moves will be progressively impaired. 

Openings can be loaded into locations $1541-$155C which Microchess will 
attempt to play from, as long as you do.  By default, the game will try 
to play the Giuoco Piano opening. 


## Reference - RP6502 VS Code Scaffolding for CC65 tool-chain:

This provides scaffolding for a new Picocomputer 6502 software project. Both
C and assembly examples of "Hello, world!" are included. Make sure
`CMakeLists.txt` points to your choice of `main.c` or `main.s`, then delete
the one you aren't using.

### Linux Tools Install:
 * [VS Code](https://code.visualstudio.com/) - This has its own installer.
 * A source build of [CC65](https://cc65.github.io/getting-started.html).
 * The following tools installed from your package managers:
    * `sudo apt install cmake python3 pip git build-essential`
    * `pip install pyserial`

### Windows Tools Install:
 * `winget install -e --id Microsoft.VisualStudioCode`
 * `winget install -e --id Git.Git`
 * `winget install -e --id Kitware.CMake`
 * `winget install -e --id GnuWin32.Make`
    Add `C:\Program Files (x86)\GnuWin32\bin` to your PATH.
 * The current snapshot of [CC65](https://cc65.github.io/getting-started.html) -
   Do not skip the step about adding the `bin` directory to your PATH.
 * Install Python by typing `python3` in a command prompt, which will launch
   the Microsoft Store where you can start the installation. If Python runs,
   this has already been done — exit Python with Ctrl-Z plus Enter.
 * `pip install pyserial`

### Getting Started:
Go to the [GitHub template](https://github.com/picocomputer/vscode-cc65) and
select "Use this template" then "Create a new repository". GitHub will create
a clean project for you to start with. Then you can clone the repository and
open the files.

```bash
$ git clone [path_to_github]
$ cd [to_where_it_cloned]
$ code .
```

Install the recommended extensions when VS Code prompts you, choosing the
default or obvious choice for any other prompts. The tools we use in VS Code
are constantly improving and changing making it too difficult to maintain
documentation.

"Start Debugging" (F5) will build your project and run it on a Picocomputer.
Connect with a USB cable plugged into the RP6502-VGA USB port.

If you get a Python error about the communications device not being found,
edit `.rp6502` in the project root. This file will be created the first time
you "Start Debugging" and will be ignored by git.

Once the program is running, a debug console becomes available on the terminal
tab. It will say "Python Debug Console" because the rp6502.py tool is Python.
Ctrl-A then X will exit. Ctrl-A then B will send a break.

Edit `CMakeLists.txt` to add new source and asset files. From here on, it's
standard C/assembly development for the 6502 platform.
