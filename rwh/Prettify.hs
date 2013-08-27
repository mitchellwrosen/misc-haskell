module Prettify (
   Doc,
   (<>),
   enclose,
   surround,
   hcat,
   fsep,
   series,
   punctuate,
   oneChar,
   empty,
   char,
   string,
   text,
   double,
   compact
) where

import Data.Char
import Numeric

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Eq, Show)

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

-- enclose l r x surrounds x with characters l and r
enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

-- surround c x surrounds x with two cs
surround :: Char -> Doc -> Doc
surround c = enclose c c

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f = enclose open close . fsep . punctuate (char ',') . map f

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
               Just r -> text r
               Nothing
                  | mustEscape c -> hexEscape c
                  | otherwise    -> char c
   where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
   where ch x y = (x, ['\\', y])

hexEscape :: Char -> Doc
hexEscape c
   | d < 0x10000 = smallHex d
   | otherwise   = astral (d - 0x10000)
   where d = ord c

smallHex :: Int -> Doc
smallHex n = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
   where h = showHex n ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
   where a = (n `shiftR` 10) .&. 0x3ff
         b = n .&. 0x3ff

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

string :: String -> Doc
string = surround '"' . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double = text . show

line :: Doc
line = Line

compact :: Doc -> String
compact x = transform [x]
   where transform [] = ""
         transform (d:ds) =
            case d of
               Empty        -> transform ds
               Char c       -> c : transform ds
               Text s       -> s ++ transform ds
               Line         -> '\n' : transform ds
               a `Concat` b -> transform (a:b:ds)
               _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
   where best col (d:ds) =
            case d of
               Empty        -> best col ds
               Char c       -> c : best (col+1) ds
               Text s       -> s ++ best (col + length s) ds
               Line         -> '\n' : best 0 ds
               a `Concat` b -> best col (a:b:ds)
               a `Union` b  -> nicest col (best col (a:ds)) (best col (b:ds))
         best _ _ = ""

         nicest col a b
            | (width - least) `fits` a = a
            | otherwise                = b
            where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_ `fits` ""        = True
_ `fits` ('\n':_)  = True
w `fits` (_:cs)    = (w-1) `fits` cs
