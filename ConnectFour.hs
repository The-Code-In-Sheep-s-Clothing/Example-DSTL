module ConnectFour where

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


-- ConnectFour
--
connectFour = game {
  initial  = (board (6,7) Empty,A),
  gameOver = \(b,_) -> fourInARow b || isFull b,
  outcome  = \(b,p) -> if fourAInARow b then Win A else
                       if fourBInARow b then Win B else
                       if isFull b       then Tie   else Turn p,
  input = \(b,_) s -> col2pos b (read s)
}


-- Input support for Tic-Tac-Toe
--
col2pos :: Board -> Int -> Position
col2pos b x = (height b x,x)


-- Game-Specific Properties
--
fourAInARow :: BoardProperty
fourAInARow = inARow 4 A

fourBInARow :: BoardProperty
fourBInARow = inARow 4 B

fourInARow :: BoardProperty
fourInARow = fourAInARow ||| fourBInARow

height :: Board -> Int -> Int
height b x = 1 + length [c | c <- col b x, c /= Empty]
