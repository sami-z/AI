module Checkers.Moves where
import Checkers.Types

-- CODE PROVIDED BY PROFESSOR COCKETT AND VARIOUS TA's

-- Implement your code for moves function below

-- Generate a list of all possible jump moves first, if jump moves are available on the board
-- iff no jump moves exist, generate a list of all simpl emoves available on the board
-- if there are no jummp moves and no simple moves that can be made, return EndM, which we can use to signify that the game is over
moves:: GameState -> (SMorJM [Move])
moves st | jumpmoves /= [] = JM (jump_moves st)
         | simplemoves /= [] = SM (simple_moves st)
         | otherwise = EndM
         where
             jumpmoves = jump_moves st
             simplemoves = simple_moves st

-- Pass in a game state and depending on who the current player is,
-- generate a list of simple king moves and append to that list a list of simple piece moves 
simple_moves:: GameState -> [Move]
simple_moves st | status st == RedPlayer = (simpleKing (redKings st) st) ++ (simplePiece (redPieces st) st)
                | status st == BlackPlayer = (simpleKing (blackKings st) st) ++ (simplePiece (blackPieces st) st)
                | otherwise = []

-- determine all possible pawn moves. Pass in the respective player's pawn piecestate from game state and the current game state 
-- 1. start position needs to be the position of an existing piece (from p)
-- 2. end position is one square diagonal - so [x' == x+1 || x-1] and y' depends on color. if it is red, then y' == y-1, if black then y' == y+1
-- 3. cannot move on top of another piece (space being moved to must be unoccupied)
-- 4. cannot move outside of the board
-- 5. if a pawn moves into the last row, they become a king K. if y' == 0 or y' == 7, they become a king 
simplePiece :: PieceState  -> GameState -> [ [PorK Coord] ]
simplePiece p st = [ [P (x, y), (if y' == 0 || y' == 7 then K else P) (x', y')]  | (x, y) <- p, (x', y') <- [(x + 1, y + (dir st)), (x - 1, y + (dir st))], onBoard (x', y'), emptySpace (x', y') st]

-- determine all possible king moves. Pass in the respective player's king piecestate from game state and the current game state 
-- 1. start position needs to be the position of an existing piece (from p)
-- 2. end position is one square diagonal IN ANY DIRECTION BC KING PIECE - 
-- 3. cannot move on top of another piece (space being moved to must be unoccupied)
-- 4. cannot move outside of the board
--5. simpleKing CANNOT CAUSE A REPEATED STATE!!!! 
simpleKing :: PieceState  -> GameState -> [ [PorK Coord] ]
simpleKing p st = [ [K (x, y), K (x', y')] | (x, y) <- p, (x', y') <- [(x + 1 , y + 1), (x - 1, y + 1), (x - 1, y - 1), (x + 1, y - 1)], onBoard (x', y'), emptySpace (x', y') st,  not (repeatedState ([[K (x,y), K (x', y')]] ++ history st) st)]

-- determine what y direction the piece is going - blacks move down the board (so + 1 for y) and reds move up the board (so -1 for y)
dir :: GameState -> Int
dir st |status st == RedPlayer = -1
       |otherwise = 1

-- determines if the position you wish to move to is on the board i.e between (0,0) and (7,7)
onBoard :: Coord -> Bool
onBoard (x,y) |(x >= 0 && x <= 7 && y >= 0 && y <= 7) = True
              |otherwise = False

-- determines if a passed in coordinate is empty or has a piece occupying it already       
emptySpace :: Coord -> GameState -> Bool
emptySpace c st |c `elem` (redPieces st) = False    -- determines if a given position c is inside the redPieces list 
                |c `elem` (blackPieces st) = False  -- determines if a given position c is inside the blackPieces list 
                |c `elem` (redKings st) = False     -- determines if a given position c is inside the redKings list 
                |c `elem` (blackKings st) = False   -- determines if a given position c is inside the blackKings list 
                |otherwise = True                   -- if the position is not filled by any type of piece of either colour, then the space is empty

-- Pass in a game state and depending on who the current player is,
-- generate a list of jump king moves and append to that list a list of jump piece moves 
jump_moves:: GameState -> [Move]
jump_moves st   | status st == RedPlayer = (jumpKing (redKings st) st) ++ (jumpPiece (redPieces st) st)
                | status st == BlackPlayer = (jumpKing (blackKings st) st) ++ (jumpPiece (blackPieces st) st)
                | otherwise = []

-- TODO
-- a pawn can become a king 
-- a pawn only moves in one direction

jumpPiece :: PieceState -> GameState -> [ [PorK Coord] ]
jumpPiece p st = [  P (x, y) : ys | (x, y) <- p, ys <- jumpPiece' (x, y) [] (x, y) st ]

jumpPiece' :: Coord -> PieceState -> Coord -> GameState -> [ [PorK Coord] ]
jumpPiece' start rem (x, y) st = [ (if y'' == 0 || y'' == 7 then K else P) (x'', y''):ys
    | ((x', y'), (x'', y'')) <- [((x+1,y+ (dir st)),(x+2,y+(2 * dir(st)))),((x-1,y+ (dir st)),(x-2,y+(2* dir(st))))]
    , not ((x', y') `elem` rem)
    , opponent_occupied (x', y') st
    , onBoard (x'', y'')
    , (emptySpace (x'', y'') st) || ((x'', y'') == start)
    , ys <- jump_over (if (y'' == 0 || y'' == 7) then jumpKing' start ((x', y'):rem) (x'', y'') st else  (jumpPiece' start ((x', y'):rem) (x'', y'') st))]



jumpKing :: PieceState -> GameState -> [ [PorK Coord] ]
jumpKing p st =  [ K (x, y) : ys | (x, y) <- p, ys <- jumpKing' (x, y) [] (x, y) st]




{- 
@Param: start: original start position, record it as we can jump back to "start"
        rem: list of "eaten" Coords so that we do not attempt to jump over again 
        (x,y): coord of the current start position
        st: game state
@Return: list of king jump moves

Conditions:
--  1. (x', y') and (x'', y'') are computed from moving the king in four directions
--  2. eaten piece should not be in rem
--  3. eaten pieace should be occupied by the opponent
--  4. landing position should be on board
--  5. landing position should be notOccupied, or it's the "start"
--  6. the rest of the jumps are from recursive calls of jumpKing'

Notation: 
Each jump gives us two pairs of coordinates: 
      (x', y'): the eaten piece
      (x'', y''): the landing position

-- ys is recursively called to find all other subsequent jumps

Note: This code was discuessed in tutorial
-}
jumpKing' :: Coord -> PieceState -> Coord -> GameState -> [ [PorK Coord] ]
jumpKing' start rem (x, y) st = [K (x'', y''):ys
    | ((x', y'), (x'', y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
    , not ((x', y') `elem` rem)
    , opponent_occupied (x', y') st
    , onBoard (x'', y'')
    , (emptySpace (x'', y'') st) || ((x'', y'') == start)
    , ys <- jump_over (jumpKing' start ((x', y'):rem) (x'', y'') st)]

-- checck that there is a piece is the space we are trying to jump over - return True if a piece is there
opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied (x, y) st |status st == RedPlayer = (x ,y) `elem` blackPieces st || (x, y) `elem` blackKings st
                            |status st == BlackPlayer = (x, y) `elem` redPieces st || (x, y) `elem` redKings st
                            |otherwise = False

-- return a list containing the empty list to end the recursive call on ys, which finds the subsequent jumps.  
-- If the list is not empty we just return it without changing it (i.e. identity function)
jump_over :: [[a]] -> [[a]]
jump_over [] = [[]]
jump_over z = z

-- remove a coordinate from a list 
removeMove :: Eq a => a -> [a] -> [a]
removeMove c ps = filter (/= c) ps

-- replace a coordinate with another in the passed in list 
replaceMove :: Eq b => b -> b -> [b] -> [b]
replaceMove x y list = map (\a -> if (x == a) then y else a) list


-- custom data type being used to keep track if a move in history is for a red king or black king
data RRorBB a = RR a | BB a  deriving (Show,Eq, Read)

{-
repeatedStates: 
Given a list of moves and a game state, determine which turn in history is "first" by checking 
the status of the game state. Assign the custom data type above to the move, along with the int 2
if the initial move was red or the int 1 if the initial move was black. Lastly call repeatedStates' 
with the previous parameters and the rest of the history list (xs).

repeatedStates': 
this function is a mutually recursive algorithm that switches between red and black moves through the history list,
upadating a vacant and moving list along the way. When the vacant list is empty, we have encountered a repeated state.

vacant list -> list of "vacant" spots on the board (previously contained a piece)
moving list -> which pieces have already moved from their vacant spot 

a piece is added to moving once they first move

-}
repeatedState :: [ [PorK Coord] ] -> GameState -> Bool
repeatedState ([K (x,y), K (x2,y2)]:xs) st |status st == RedPlayer = repeatedState' [RR (x2,y2)] [RR (x,y)] 2 xs
                                           |status st == BlackPlayer = repeatedState' [BB (x2,y2)] [BB (x,y)] 1 xs

repeatedState' :: [RRorBB Coord] -> [RRorBB Coord] -> Int -> [Move] -> Bool
repeatedState' [] _ _ _ = True -- if vacant list is empty there is a repeated state -> return true
repeatedState' vacant moving 1 ([K (x,y), K (x2,y2)]:xs) |(RR (x2,y2)) `elem` moving && (RR (x,y)) `elem` vacant = repeatedState' (removeMove (RR (x,y)) vacant) (removeMove (RR (x2,y2)) moving) 2 xs -- if red piece is moving and moved into vacant spot, remove piece from moving and vacant lists
                                                         |(RR (x2,y2)) `elem` moving && not ((RR (x,y)) `elem` vacant) = repeatedState' vacant (replaceMove (RR (x2,y2)) (RR (x,y)) moving) 2 xs -- if red piece is moving and moved into a spot that's not in vacant list, update position of moving piece
                                                         |not ((RR (x2,y2)) `elem` moving) = repeatedState' ((RR (x2,y2)):vacant) ((RR (x,y)):moving) 2 xs -- if red piece was not moving add piece to moving and previous position to vacant
repeatedState' vacant moving 2 ([K (x,y), K (x2,y2)]:xs) |(BB (x2,y2)) `elem` moving && (BB (x,y)) `elem` vacant = repeatedState' (removeMove (BB (x,y)) vacant) (removeMove (BB (x2,y2)) moving) 1 xs -- if black piece is moving and moved into vacant spot, remove piece from moving and vacant lists
                                                         |(BB (x2,y2)) `elem` moving && not ((BB (x,y)) `elem` vacant) = repeatedState' vacant (replaceMove (BB (x2,y2)) (BB (x,y)) moving) 1 xs -- if black piece is moving and moved into a spot that's not in vacant list, update position of moving piece
                                                         |not ((BB (x2,y2)) `elem` moving ) = repeatedState' ((BB (x2,y2)):vacant) ((BB (x,y)):moving) 1 xs -- if black piece was not moving add piece to moving and previous position to vacant
repeatedState' _ _ _ _ = False  -- catches cases where there is a double jump, pawn move, etc. bc they cant result in repeated state