module Checkers.Heuristic where 
import Checkers.Types


black_heuristic :: GameState -> Int
black_heuristic st = length (blackPieces st) - length (redPieces st) + 2 * (length (blackKings st) - length (redKings st))



red_heuristic :: GameState -> Int 
red_heuristic  st = length (redPieces st) - length (blackPieces st) + 2 * (length (redKings st) - length (blackKings st))


test1 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(0,1)]
                  , redKings = [(0,3),(2,3)]
                  , status = RedPlayer
                  , message = "" 
                  , history = []}
