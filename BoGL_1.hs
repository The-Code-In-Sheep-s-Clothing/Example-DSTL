module BoGL_1 where


import Prelude hiding (repeat,until)
import qualified Prelude (repeat)
import Data.Array
import Data.List (isInfixOf)
import System.IO.Unsafe (unsafePerformIO)


{-
PREDEFINED TYPES
  Player   = A | B
  Board    = Array (Int,Int) Content
  State    = (Board,Player)
  Content  = Player | Empty
  Position = (Int,Int)
  Status   = Win Player | Tie | Turn Player
-}

data Player   = A | B deriving (Eq,Show)
type Board    = Array (Int,Int) Content
type State    = (Board,Player)
data Content  = Occupied Player | Empty deriving Eq
type Position = (Int,Int)
data Status   = Win Player | Tie | Turn Player


instance Show Content where
  show Empty        = "."
  show (Occupied p) = show p

instance Show Status where
  show (Win p)  = show p++" wins"
  show Tie      = "State is a tie"
  show (Turn p) = "It\'s "++show p++"\'s turn"


{-
PREDEFINED GENERIC VALUES & FUNCTIONS
  switch   :: Player -> Player
  content  :: Board -> Position -> Content
  isEmpty  :: Board -> Position -> Bool
  place    :: Player -> Board -> Board
  readMove :: Position
-}

switch :: Player -> Player
switch A = B
switch B = A

content :: Board -> Position -> Content
content = (!)

isEmpty :: Board -> Position -> Bool
isEmpty b p = content b p == Empty

-- Unconditionally place a piece on a board
--
place :: Player -> Position -> Board -> Board
place a p b = b // [(p,Occupied a)]


--
-- Built-in Control Structure
--
type KW = String

until :: KW
until = "until"

repeat :: (State -> State) -> KW -> (State -> Bool) -> State -> State
repeat f kw p g | kw /= "until" = error "Keyword until expeted!"
                | p g'      = g'
                | otherwise = repeat f until p g'
                  where g' = f g

{-
GAME-DEPENDENT VALUES, FUNCTIONS & PREDICATES
  initial  :: State
  gameOver :: State -> Bool
  outcome  :: State -> Status
OPTIONAL (functions are predefined but can be overwritten)
  input    :: State -> String -> Position
  output   :: Content -> String
  isValid  :: Position -> State -> Bool
  turn     :: Position -> State -> State
-}

data Game = G {
     -- required
     initial  :: State,
     outcome  :: State -> Status,
     -- optional
     gameOver :: State -> Bool,
     input    :: State -> String -> Position,
     output   :: Content -> String,
     isValid  :: Position -> State -> Bool,
     turn     :: Position -> State -> State
}

-- Generic default game definition
--
game :: Game
game = G {
  -- required
  initial  = undefined,
  outcome  = undefined,
  -- optional
  input    = \_ -> read,   -- ignore state
  output   = show,
  gameOver = isFull,
  isValid  = \p (b,_) -> b!p == Empty,
  turn     = \p (b,x) -> (place x p b,switch x)
}


{-
GAME-DEPENDENT, DERIVED FUNCTION DEFINITIONS
  readMove :: State -> Position
  tryMove  :: State -> State
  loop     :: State -> State
  play     :: State -> Status
-}

readMoveIO :: Game -> State -> IO Position
readMoveIO g s@(b,p) = do printBoard g b
                          putStr ("Enter move for player "++show p++": ")
                          l <- getLine
                          return (input g s l)

-- readMove :: (Input a,Game state) => state -> a
readMove :: Game -> State -> Position
readMove g = unsafePerformIO . readMoveIO g

tryMove :: Game -> State -> State
tryMove g s | isValid g p s = turn g p s
            | otherwise     = s
              where p = readMove g s

-- Main game loop
--
loop :: Game -> State -> State
loop g = repeat (tryMove g) until (gameOver g)

play :: Game -> Status
play g = unsafePerformIO (printBoard g b >> return (outcome g final))
         where final@(b,_) = loop g (initial g)


--
-- Generic functions on boards
--
type Row = [Content]

-- Create a board from rows, using bottom-up numbering of rows
--
board :: (Int,Int) -> Content -> Board
board size c = listArray ((1,1),size) (Prelude.repeat c)

byRows :: [[Content]] -> Board
byRows rows = listArray ((1,1),size) (concat (reverse rows))
              where size = (length rows,length (head rows))

-- Board size
--
size :: Board -> (Int,Int)
size = snd . bounds

maxRow :: Board -> Int
maxRow = fst . size

maxCol :: Board -> Int
maxCol = snd . size

-- Extracting rows, columns, and diagonals
--
row :: Board -> Int -> Row
row b y = [b!(y,x) | x <- [1..maxCol b]]

rows :: Board -> [Row]
rows b = [row b r | r <- [1..maxRow b]]

col :: Board -> Int -> Row
col b x = [b!(y,x) | y <- [1..maxRow b]]

cols :: Board -> [Row]
cols b = [col b c | c <- [1..maxCol b]]

diagsUp :: Board -> (Int,Int) -> Row
diagsUp b (y,x) | y > maxRow b || x > maxCol b = []
               | otherwise = b!(y,x):diagsUp b (y+1,x+1)

diagsDown :: Board -> (Int,Int) -> Row
diagsDown b (y,x) | y < 1 || x < 1 = []
                 | otherwise = b!(y,x):diagsDown b (y-1,x-1)

diags :: Board -> [Row]
diags b = [diagsUp b (1,x) | x <- [1..maxCol b]] ++
          [diagsUp b (y,1) | y <- [2..maxRow b]] ++
          [diagsDown b (maxRow b,x) | x <- [1..maxCol b]] ++
          [diagsDown b (y,1) | y <- [1..maxRow b-1]]

allRows :: Board -> [Row]
allRows b = rows b ++ cols b ++ diags b


-- Printing a board
--
showRow :: Game -> Row -> String
showRow g = concatMap (output g)

maxRows :: Board -> Int
maxRows = fst . snd . bounds

showBoard :: Game -> Board -> String
showBoard g b = unlines [showRow g (row b r) | r <- reverse [1..maxRows b]]

printBoard :: Game -> Board -> IO ()
printBoard g = putStrLn . showBoard g


-- Conditions / board properties
--
type BoardProperty = Board -> Bool

(&&&) :: BoardProperty -> BoardProperty -> BoardProperty
p &&& q = \b -> p b && q b

(|||) :: BoardProperty -> BoardProperty -> BoardProperty
p ||| q = \b -> p b || q b

open :: Board -> [Position]
open g = [p | (p,v) <- assocs g, v==Empty]

isFull :: BoardProperty
isFull = null . open

-- (!!!) :: Board -> [Position] -> [Content]
-- g !!! ps = map (g!) ps

ofKind :: Int -> Player -> Row
-- ofKind n = map Occupied . replicate n
n `ofKind` p = map Occupied (replicate n p)

-- fourAs = 4 `ofKind` A
-- fourBs = 4 `ofKind` B

inARow :: Int -> Player -> BoardProperty
inARow n p = any (isInfixOf (n `ofKind` p)) . allRows
