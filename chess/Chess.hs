module Chess where

import Test.HUnit
import Test.QuickCheck

type Board = [[Square]]

initialBoardStr = unlines ["rnbqkbnr"
                          ,"pppppppp"
                          ,"        "
                          ,"        "
                          ,"        "
                          ,"        "
                          ,"RNBQKBNR"
                          ,"PPPPPPPP"
                          ]

readBoard :: String -> Board
readBoard = map readRow . lines
   where readRow = map readSquare


type Square = Maybe Piece

data Piece = Piece PColor PType
data PColor = White | Black
data PType = Pawn | Knight | Bishop | Rook | Queen | King

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c   = Just (readPiece c)

-- Shows a piece using FEN notation.
--
-- White pieces use "PNBRQK"
-- Black pieces use "pnbrqk"
showPiece :: Piece -> Char
showPiece (Piece White Pawn)   = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook)   = 'R'
showPiece (Piece White Queen)  = 'Q'
showPiece (Piece White King)   = 'K'
showPiece (Piece Black Pawn)   = 'p'
showPiece (Piece Black Knight) = 'n'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook)   = 'r'
showPiece (Piece Black Queen)  = 'q'
showPiece (Piece Black King)   = 'k'

-- Shows a piece using FEN notation.
--
-- White pieces use "PNBRQK"
-- Black pieces use "pnbrqk"
readPiece :: Char -> Piece
readPiece 'P' = Piece White Pawn
readPiece 'N' = Piece White Knight
readPiece 'B' = Piece White Bishop
readPiece 'R' = Piece White Rook
readPiece 'Q' = Piece White Queen
readPiece 'K' = Piece White King
readPiece 'p' = Piece Black Pawn
readPiece 'n' = Piece Black Knight
readPiece 'b' = Piece Black Bishop
readPiece 'r' = Piece Black Rook
readPiece 'q' = Piece Black Queen
readPiece 'k' = Piece Black King

-- Tests

tests = TestList $ map TestCase
   [assertEqual "add tests here" 1 1
   ]

prop_empty c1 = (c1::Int) == c1

runTests = do
   runTestTT tests
   quickCheck prop_empty

-- Main runs tests
main :: IO ()
main = runTests
