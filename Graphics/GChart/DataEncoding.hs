module Graphics.GChart.DataEncoding where

import Graphics.GChart.Types

import Data.List(intercalate)
import Data.Char (chr, ord)
import Numeric (showHex)


-- Data Encodings

encodeSimple :: [[Int]] -> String
encodeSimple datas = "s:" ++ intercalate "," (map (map enc) datas) where
    enc :: Int -> Char
    enc i | i >= 0  && i <= 25 = chr (ord 'A' + i)
          | i >= 26 && i <= 51 = chr (ord 'a' + (i - 26))
          | i >= 52 && i <= 61 = chr (ord '0' + (i - 52))
          | otherwise          = '_'

encSimpleReverse :: Char -> Int
encSimpleReverse c | ord c >= ord 'A' && ord c <= ord 'Z' = ord c - ord 'A'
                   | ord c >= ord 'a' && ord c <= ord 'z' = 26 + (ord c - ord 'a')
                   | ord c >= ord '0' && ord c <= ord '9' = 52 + (ord c - ord '0')
                   | otherwise = -1

-- FIXME This assumes all data is in range.
encodeText datas = "t:" ++ intercalate "|" (map encData datas) where
    encData = intercalate "," . map showDecimal

showDecimal :: Float -> String
showDecimal i | makeFloat (truncate i) - i == 0  = show $ truncate i
              | otherwise                            = show (fromIntegral (round (i * 10.0)) / 10.0)

makeFloat i = fromIntegral i :: Float

encodeExtended datas = "e:" ++ intercalate "," (map (concatMap encDatum) datas) where
    encDatum i | i >= 0 && i < 4096 = let (a, b) = i `quotRem` 64 in
                                      [encChar a, encChar b]
               | otherwise          = "__"
    encChar i | i >= 0  && i <= 25 = chr (ord 'A' + i)
              | i >= 26 && i <= 51 = chr (ord 'a' + (i - 26))
              | i >= 52 && i <= 61 = chr (ord '0' + (i - 52))
              | i == 62            = '-'
              | i == 63            = '.'


-- | URL-encode a string.
urlEnc str = concatMap enc str where
  enc c | c >= 'A' && c <= 'Z' = [c]
        | c >= 'a' && c <= 'z' = [c]
        | c >= '0' && c <= '9' = [c]
        | c `elem` safe        = [c]
        | c == ' '             = "+"
        | otherwise  = '%': showHex (ord c) ""
  -- Argh, different resources differ on which characters need escaping.
  -- This is likely wrong.
  safe = "$-_.!*'(),|:"
