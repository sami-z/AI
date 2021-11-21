module Checkers.ApplyMove where
import Checkers.Moves
import Checkers.Types
-- CODE PROVIDED BY PROFESSOR COCKETT AND VARIOUS TA's

------------------------------------------------
------------------------------------------------
--
--  APPLYING A MOVE to a state
--
------------------------------------------------
------------------------------------------------

--OVERALL: we need to actually apply a move to a game state after checking if it 
-- is a valid move to make 

-- first we must determine if it is a simple move or a jump move -> cases
-- then we need to see if a P or K is moving. If the move we are checking is an illegal move, display a message to the user
-- that the move checked is invalid and don't change the game state. If there are no mroe moves that can be generated, change the 
-- status of the game state to GameOver. 
apply_move:: Move -> GameState -> GameState
apply_move mv st |moves (st) == EndM = st{message = "Game Over", status = GameOver} -- dont set to game over?
                 |moves (st) == JM (jump_moves st) && (mv `elem` jump_moves st) = (make_jump_move (stripPorK mv) st) {status = change_player st, history = mv:history st} --exhaust jump moves first
                 |moves (st) == SM (simple_moves st) && (mv `elem` simple_moves st) = if jump_moves st /= [] then st{message = "Illegal move!! There are jump moves: " ++ (show (jump_moves st))} else make_simple_move (stripPorK mv) st --error check? check that there are no jump moves available, orr is this already caught above  
                 |otherwise = st{message = "Illegal move!!"}

-- function to remove the PorK data from each coordiante in a list of PorK Coord
stripPorK :: Move -> PieceState
stripPorK [] = []
stripPorK (P x:xs) = x: stripPorK xs
stripPorK (K x:xs) = x: stripPorK xs

{- 

Given a list of coordinates and a game state, make a simple move depending on 
what type of piece "start" is. Update the game state accordingly, and write the move
to history and change status to the next player's turn.

6 cases of make_simple_move
1. red king makes simple move
2. black king makes simple move
3. red pawn makes simple move AND DOES NOT PROMOTE
4. black pawn makes simple move AND DOES NOT PROMOTE
5. red pawn makes simple move AND DOES PROMOTE
6. black pawn makes simple move AND DOES PROMOTE
-}
make_simple_move :: PieceState  -> GameState -> GameState  
make_simple_move [start,end] st
       | status st == RedPlayer && start `elem` (redKings st)   --redKing move 
               = st{redKings = replace start end (redKings st)
                     , status = (change_player st)
                     , message = ""
                     , history = [K start, K end]: history st}
       | status st == BlackPlayer  && start `elem` (blackKings st) --blackKing move 
               = st{blackKings = replace start end (blackKings st)
                     , status = (change_player st)
                     , message = ""
                     , history = [K start, K end]: history st}
       | status st == RedPlayer && start `elem` (redPieces st) && not (promotion end)--redPiece move   -- NOT PROMOTED
                = st{redPieces = replace start end (redPieces st)
                     , status = (change_player st)
                     , message = ""
                     , history = [P start, P end]: history st}
       | status st == BlackPlayer && start `elem` (blackPieces st) && not (promotion end)--blackPiece move -- NOT PROMOTED
                = st{blackPieces = replace start end (blackPieces st)
                     , status = (change_player st)
                     , message = ""
                     , history = [P start, P end]: history st}
        | status st == RedPlayer && start `elem` (redPieces st) && (promotion end)--redPiece move   -- PROMOTED
                = st{redPieces = remove start (redPieces st)
                     , redKings = end : (redKings st)
                     , status = (change_player st)
                     , message = ""
                     , history = [P start, K end]: history st}
       | status st == BlackPlayer && start `elem` (blackPieces st) && (promotion end)--blackPiece move -- PROMOTED
                = st{blackPieces = remove start (blackPieces st)
                     , blackKings = end : (blackKings st)
                     , status = (change_player st)
                     , message = ""
                     , history = [P start, K end]: history st}
       |otherwise = st{message = "Illegal make simple move!!"}

      
-- replace: takes 3 params, piece to replace, piece to replace with, and a piecestate 
-- and replaces the appropriate piece in the piecestate list passed in
replace :: Coord -> Coord -> PieceState -> PieceState
replace x y list = map (\a -> if (x == a) then y else a) list

{-

Given a list of coordinates and a game state, make a jump move depending on 
what type of piece "start" is. Update the game state accordingly, and write the move
to history and change status to the next player's turn.

6 cases of make_jump_move
1. red king jumps a piece of opposite color
2. black king jumps a piece of opposite color
3. red pawn jumps a piece of opposite color AND DOES NOT PROMOTE
4. black pawn jumps a piece of opposite color AND DOES NOT PROMOTE
5. red pawn jumps a piece of opposite color AND DOES PROMOTE
6. black pawn jumps a piece of opposite color AND DOES PROMOTE
-}
make_jump_move :: PieceState -> GameState -> GameState 
make_jump_move [start] st = st 
make_jump_move (start:(next:rest)) st
       | status st == RedPlayer && start `elem` (redKings st) -- redKing eats piece 
                = make_jump_move (next:rest)
                            (st{blackKings = remove (jumped start next) (blackKings st)
                               , blackPieces = remove (jumped start next) (blackPieces st)
                               , redKings = next:(remove start (redKings st))
                               , message = ""})
       | status st == BlackPlayer  && start `elem` (blackKings st) -- blackKing eats piece
                = make_jump_move (next:rest)
                            (st{redKings = remove (jumped start next) (redKings st)
                               , redPieces = remove (jumped start next) (redPieces st)
                               , blackKings = next:(remove start (blackKings st))
                               , message = ""})
       | status st == RedPlayer  && start `elem` (redPieces st) &&  not (promotion next) -- redPiece eats piece - no promote
                = make_jump_move (next:rest)
                            (st{blackKings = remove (jumped start next) (blackKings st)
                               , blackPieces = remove (jumped start next) (blackPieces st)
                               , redPieces = next:(remove start (redPieces st))
                               , message = ""})
       | status st == BlackPlayer  && start `elem` (blackPieces st) && not (promotion next) -- blackPiece eats piece - no promote
                = make_jump_move (next:rest)
                            (st{redKings = remove (jumped start next) (redKings st)
                               , redPieces = remove (jumped start next) (redPieces st)
                               , blackPieces = next:(remove start (blackPieces st))
                               , message = ""})
        | status st == RedPlayer  && start `elem` (redPieces st) && (promotion next) -- redPiece eats piece - PROMOTES
                 = make_jump_move (next:rest)
                            (st{blackKings = remove (jumped start next) (blackKings st)
                               , blackPieces = remove (jumped start next) (blackPieces st)
                               , redPieces = (remove start (redPieces st))
                               , redKings = next: (redKings st) 
                               , message = ""})
        | status st == BlackPlayer  && start `elem` (blackPieces st) && (promotion next) -- blackPiece eats piece - PROMOTE
                 = make_jump_move (next:rest)
                            (st{redKings = remove (jumped start next) (redKings st)
                               , redPieces = remove (jumped start next) (redPieces st)
                               , blackPieces = (remove start (blackPieces st))
                               , blackKings = next : (blackKings st)
                               , message = ""})
       | otherwise = st{message = "Illegal make jump move!!"} --needed???

-- determine if the coordinates passed in will result in a piece promoting .
-- a y coordinate in 0 or 7 will mean a piece is promoting, so return True.
promotion :: Coord -> Bool
promotion (x, y) |y == 0 || y == 7 = True
                 |otherwise = False

-- removes jumped piece coordinates from board 
remove :: Coord -> PieceState -> PieceState
remove c ps = filter (/= c) ps

--returns the coordinates of the piece that is being jumped over - midpoiont formula 
jumped :: Coord -> Coord -> Coord
jumped (x, y) (x', y') = ( (x + x') `div` 2 , (y + y') `div` 2 )

-- given a game state, change the "status" field to the opposite player's turn
change_player :: GameState -> Status 
change_player st |status st == BlackPlayer = RedPlayer 
                 |otherwise = BlackPlayer 


