module TicTacToe where


import BoGL_1

{-
REQUIRED FUNCTIONS & PREDICATES
  initial  :: State
  gameOver :: State -> Bool
  outcome  :: State -> Status
OPTIONAL (functions are predefined but can be overwritten)
  input    :: State -> String -> Position
  output   :: Content -> String
  isValid  :: Position -> State -> Bool
  turn     :: Position -> State -> State
-}


-- The basic Tic-Tac-Toe game
--
ticTacToe :: Game
ticTacToe = game {
  initial  = (board (3,3) Empty,A),
  gameOver = \(b,_) -> threeInARow b || isFull b,
  outcome  = \(b,p) -> if threeAInARow b then Win A else
                       if threeBInARow b then Win B else
                       if isFull b       then Tie   else Turn p
}

-- Allow input shortcuts
--
ttt :: Game
ttt = ticTacToe {
  input = \_ s -> if s `elem` (map (:[]) "LMR<c>lmr") then name2pos s
                                                      else read s
}
name2pos :: String -> Position
name2pos "L" = (3,1)
name2pos "M" = (3,2)
name2pos "R" = (3,3)
name2pos "<" = (2,1)
name2pos "c" = (2,2)
name2pos ">" = (2,3)
name2pos "l" = (1,1)
name2pos "m" = (1,2)
name2pos "r" = (1,3)


-- Change output to X and O (keep input shortcuts)
--
xo = ttt { output = showXO }
     where showXO Empty ="."
           showXO (Occupied A) = "X"
           showXO (Occupied B) = "O"


-- A simple variation with 4 columns (keep input shortcuts)
--
ttt4c = ttt { initial = (board (3,4) Empty,A) }


-- Game-Specific Properties
--
threeAInARow :: BoardProperty
threeAInARow = inARow 3 A

threeBInARow :: BoardProperty
threeBInARow = inARow 3 B

threeInARow :: BoardProperty
threeInARow = threeAInARow ||| threeBInARow
