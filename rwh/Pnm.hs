module Pnm where

import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Lazy as BS
import Data.Char (isSpace)

data Greymap = Greymap {
   greyWidth  :: Int,
   greyHeight :: Int,
   greyMax    :: Int,
   greyData   :: BS.ByteString
} deriving (Eq)

instance Show Greymap where
   show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++
                            show m

matchHeader :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
matchHeader = undefined

getNum :: BS.ByteString -> Maybe (Int, BS.ByteString)
getNum = undefined

getBytes :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
getBytes = undefined

parseP5 :: BS.ByteString -> Maybe (Greymap, BS.ByteString)
parseP5 s = case matchHeader (BS8.pack "P5") s of
   Nothing -> Nothing



