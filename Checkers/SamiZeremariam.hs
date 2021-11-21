module Checkers.SamiZeremariam(moves, apply_move, red_ai, black_ai) where 

import Checkers.Types
import Checkers.Moves
import Checkers.Heuristic
import Checkers.ApplyMove



-- Note: Base of this AI follows TA tutorial from previous semester => https://pages.cpsc.ucalgary.ca/~niran.pon1/

-- Initial value(s) used for the "top" and "bottom" of our minmax search 
infinity :: Float
infinity = 1/0


-- The AI for the red AI. Calls the minimax function with a depth of 6 and returns the chosen move after evaluation
red_ai :: GameState -> Move
red_ai st = strip st (snd $ minimax st 6)

-- The AI for the black AI. Calls the minimax function with a depth of 6 and returns the chosen move after evaluation
black_ai :: GameState -> Move
black_ai st = strip st (snd $ minimax st 6)


-- Custom function to strip the Maybe data type from our return of minimax 
strip :: GameState -> (Maybe Move) ->  Move 
strip st (Just x) = x 
strip st Nothing = [] 


-- Function to call our heurisitc, based on the current AI's turn 
heuristic :: GameState -> Float
heuristic st | status st == RedPlayer = fromIntegral (red_heuristic st)
             | otherwise = fromIntegral (black_heuristic st)

-- Function that calls our alpha beta pruning algorithm. The boolean determines whether we are 
-- maximizing or not 
minimax :: GameState -> Int -> (Float, Maybe Move)
minimax st depth = alphabeta st depth (-infinity) (infinity) True

-- Custom function to determine if the status of the game is GameOver 
gameOver :: GameState -> Bool
gameOver st |status st == GameOver = True
            |otherwise = False

-- Function that returns a list of all possible moves given the game state
getallmoves :: GameState -> [Move]
getallmoves st | jump_moves st /= [] = jump_moves st
               | simple_moves st /= [] = simple_moves st
               | otherwise  = []

-- returns (value of the highest move, move)
alphabeta :: GameState -> Int -> Float -> Float -> Bool -> (Float, Maybe Move)
alphabeta st depth alpha beta maximizing  | gameOver st = (heuristic st, Nothing) -- if the game is over return Nothing because the game is over
                                          | depth <= 0 = (heuristic st, Nothing)  -- -- if the depth is <= 0, return Nothing because we cannot go past depth 0
                                          | maximizing = let
                                            -- parameters: move list, (alpha, (value, move))
                                            -- returns:    (highest alpha, (highest value move, move))
                                            -- maximize :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
                                            maximize [] ret = ret
                                            maximize (m:ms) (prevAlpha, (prevValue, prevMove))
                                                |  alpha >= beta = (alpha, (value, move))
                                                | otherwise = maximize ms (alpha, (value, move))
                                                where
                                                    (eval, _) = alphabeta (apply_move m st) (depth - 1)  prevAlpha beta False -- decrement depth by 1 and call alphabeta again with the new game state after applying the move.
                                                    value = max prevValue eval
                                                    alpha = max prevAlpha value
                                                    move = if prevValue == value then prevMove else Just m
                                            in snd $ maximize (getallmoves st) (alpha, (-infinity, Nothing))
                                          | otherwise = snd $ minimize (getallmoves st) (beta, (infinity, Nothing)) -- minimizing 
                                                    where
                                                        -- parameters: move list, (alpha, (value, move))
                                                        -- returns:    (highest alpha, (highest value move, move))
                                                        minimize :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
                                                        minimize [] ret = ret
                                                        minimize (m:ms) (prevBeta, (prevValue, prevMove))
                                                            | beta <= alpha  = (beta, (value, move))
                                                            | otherwise = minimize ms (beta, (value, move))
                                                            where
                                                                (eval, _) = alphabeta (apply_move m st) (depth - 1) alpha prevBeta True -- decrement depth by 1 and call alphabeta again with the new game state after applying the move.
                                                                value = min prevValue eval
                                                                beta = min prevBeta value
                                                                move = if prevValue == value then prevMove else Just m


