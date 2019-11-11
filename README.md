
# BoGL -- Board Game Language

A DSTL (Domain-Specific Teaching Language) for defining simple board games.
Implemented through a shallow embedding in Haskell.


## ASSUMPTIONS for LEVEL 1:
- 2 players
- rectangular board => cartesian indices
- territorial (= space occupation) game
   => board content = player (or empty)
  BUT: move is not necessarily = board position, see Connect Four
- 1 phase (each piece is placed once and never moved)


## BASIC USAGE

Define the initial board plus two functions: (1) gameOver for determining when the game is over and (2) outcome for reporting the result of the game. These values/functions are stored as a record components of the type Game.

    myGame :: Game
    myGame = game {
      initial  = ...,
      gameOver = \(b,p) -> ...,
      outcome  = \(b,p) -> ...
    }

The functions gameOver and outcome take as input the current state of the game,
which consists of the board and the player whose turn it is.

The definition should be put in a module that contains at the beginning the following import declaration.

    import BoGL_1

Then the game can be played by loading the module into GHCi and then executing  the following expression.

    play myGame

For concrete examples see the files TicTacToe.hs and ConnectFour.hs


## MORE DETAILS

The following type and function definitions are contained in the module
BoGL_1.hs.

### PREDEFINED TYPES

    Player   = A | B
    Board    = Array (Int,Int) Content
    State    = (Board,Player)
    Content  = Player | Empty
    Position = (Int,Int)
    Status   = Win Player | Tie | Turn Player

### PREDEFINED GENERIC VALUES & FUNCTIONS

    switch   :: Player -> Player
    content  :: Board -> Position -> Content
    isEmpty  :: Board -> Position -> Bool
    place    :: Player -> Board -> Board
    readMove :: Position

The following components are part of the record type Game and define the
semantics of a game. The first 3 components have to be defined, the last 4 can
be overwritten if needed to customize the game.

### GAME-DEPENDENT VALUES, FUNCTIONS & PREDICATES

    initial  :: State
    gameOver :: State -> Bool
    outcome  :: State -> Status

### OPTIONAL (functions are predefined but can be overwritten)

    input    :: State -> String -> Position
    output   :: Content -> String
    isValid  :: Position -> State -> Bool
    turn     :: Position -> State -> State
