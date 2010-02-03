{-# LANGUAGE TypeSynonymInstances, NoMonomorphismRestriction #-}
module ChartItems where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Char (chr, ord)
import Numeric (showHex)
import Types
import Prelude hiding (Right, Left)
-- Setting/Encoding Chart Data

updateChart u = do chart <- get
                   put $ u chart

asList a = [a]


-- size
instance ChartItem ChartSize where
    set size = updateChart $ \chart -> chart { chartSize = size }

    encode size =  asList ("chs", show width ++ "x" ++ show height) where
                   Size width height = size


-- type
instance ChartItem ChartType where
    set cType = updateChart $ \chart -> chart { chartType = cType }

    encode cType =  asList ("cht",t)
                    where t = case cType of
                                    Line       -> "lc"
                                    LineXY     -> "lxy"
                                    Sparklines -> "ls"

-- title
instance ChartItem ChartTitle where
    set title = updateChart $ \chart -> chart { chartTitle = Just title }

    encode title = asList ("chl", title)


-- data
-- FIXME just a placeholder for now
instance ChartItem ChartData where
    set cData = updateChart $ \chart -> chart { chartData = cData }

    encode (D cData) = asList ("chd", encodeSimple cData)


addDataToChart :: [Int] -> ChartM ()
addDataToChart d = do c <- get
                      let D old = chartData c
                          new = D (old ++ [d])
                      set new


-- color

instance ChartItem ChartColors where
    set colors = updateChart $ \chart -> chart { chartColors = Just colors }

    encode colors = asList ("chco", intercalate "," colors)



addColorToChart color = do chart <- get
                           let old = fromMaybe [] $ chartColors chart
                               new = old ++ [color]
                           set new

-- fill

instance ChartItem ChartFills where
    set fills = updateChart $ \chart -> chart { chartFills = Just fills }

    encode fills = asList ("chf",intercalate "|" $ map encodeFill fills)


encodeFill (Fill kind fType) = case kind of
                                 Solid color -> intercalate "," [fillType,"s",color]

                                 LinearGradient angle offsets -> intercalate "," [fillType,
                                                                                  "lg",
                                                                                  show angle,
                                                                                  intercalate "," $ map  (\(c,o) -> c ++ "," ++ show o ) offsets]

                                 LinearStripes angle widths ->   intercalate "," [fillType,
                                                                                  "ls",
                                                                                  show angle,
                                                                                  intercalate "," $ map (\(c,w) -> c ++ "," ++ show w) widths]
                                 where fillType = case fType of
                                                    Background  -> "bg"
                                                    Area        -> "c"
                                                    Transparent -> "a"


addFillToChart fill = do chart <- get
                         let fills = fromMaybe [] $ chartFills chart
                             newFills = fills ++ asList fill
                         set newFills


-- legend

instance ChartItem ChartLegend where
    set legend = updateChart $ \chart -> chart { chartLegend = Just legend }

    encode (Legend labels position) = [encodeTitle] ++ encodePosition position where
                               encodeTitle = ("chdl", intercalate "|" labels)
                               encodePosition Nothing = []
                               encodePosition (Just p) = let pos = case p of
                                                                    Bottom  -> "b"
                                                                    Top     -> "t"
                                                                    VBottom -> "bv"
                                                                    VTop    -> "tv"
                                                                    Right   -> "r"
                                                                    Left    -> "l"
                                                        in  asList ("chdlp",pos)


-- URL Conversion
-- FIXME : too much boilerplate. Can it be reduced?
encodeMaybe Nothing = [("","")]
encodeMaybe (Just x)  = encode x

getParams chart =  filter (/= ("","")) $ concat [encode $ chartType chart,
                                                         encode $ chartSize chart,
                                                         encode $ chartData chart,
                                                         encodeMaybe $ chartTitle  chart,
                                                         encodeMaybe $ chartColors chart,
                                                         encodeMaybe $ chartFills  chart,
                                                         encodeMaybe $ chartLegend chart]

convertToUrl chart = baseURL ++ intercalate "&" urlparams where
    baseURL = "http://chart.apis.google.com/chart?"
    urlparams = [urlEnc a ++ "=" ++ urlEnc b | (a,b) <- getParams chart]


-- Data Encodings

encodeSimple :: [[Int]] -> String
encodeSimple datas = "s:" ++ intercalate "," (map (map enc) datas) where
    enc :: Int -> Char
    enc i | i >= 0  && i <= 25 = chr (ord 'A' + i)
          | i >= 26 && i <= 51 = chr (ord 'a' + (i - 26))
          | i >= 52 && i <= 61 = chr (ord '0' + (i - 52))
          | otherwise          = '_'


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


-- smart constructors

solid = Fill . Solid

legend labels = Legend labels Nothing

legendWithPosition labels position = Legend labels (Just position)

-- helper functions

setChartSize w h = set (Size w h)

setChartType = set

setChartTitle = set

addChartData = addDataToChart

addColors = set

addColor  = addColorToChart

addFill = addFillToChart

addLegend = set

-- API Functions

getChartData m = execState m defaultChart

getUrl =  convertToUrl . getChartData

debugChart = getUrl $ do setChartSize 640 400
                         setChartType Line
                         setChartTitle "Test"
                         addChartData  [1,2,3,4,5]
                         addChartData  [3,4,5,6,7]
                         -- can use addColors also
                         addColor "FF0000"
                         addColor "00FF00"
                         addFill $ solid "0000FF" Background
                         addLegend $ legendWithPosition ["t1","t2"] VBottom
