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

    encode (Legend labels position) = encodeTitle : encodePosition position where
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

-- AXIS
instance ChartItem ChartAxes where
    set axes = updateChart $ \chart -> chart { chartAxes = Just axes }

    encode axes = filter (/= ("","")) $ map (\f -> f axes) [encodeAxesTypes,
                                                            encodeAxesLabels,
                                                            encodeAxesPositions,
                                                            encodeAxesRanges,
                                                            encodeAxesStyles]

addAxisToChart axis = do chart <- get
                         let old = fromMaybe [] $ chartAxes chart
                             new = old ++ [axis]
                         set new


convertFieldToString encoder field = intercalate "|" .
                                     map encoder .
                                     filter (\(_,maybeField) -> maybeField /= Nothing) .
                                     indexed . map field

indexed = zip [0..]

encodeFieldToParams fieldParam fieldStr | fieldStr == "" = ("","")
                                        | otherwise = (fieldParam, fieldStr)

encodeAxesTypes axes = ("chxt",a) where
                       a = intercalate "," $ map toParam axes
                       toParam axes = case axisType axes of
                                        AxisBottom -> "x"
                                        AxisTop    -> "t"
                                        AxisLeft   -> "y"
                                        AxisRight  -> "r"

-- axis labels
strAxisLabels (idx,Just labels) = show idx ++ ":|" ++ intercalate "|" labels

strAxesLabels = convertFieldToString strAxisLabels axisLabels

encodeAxesLabels = encodeFieldToParams "chxl" . strAxesLabels

-- axis positions
strAxisPositions (idx, Just positions) = show idx ++ "," ++ intercalate "," (map show positions)

strAxesPositions = convertFieldToString strAxisPositions axisPositions

encodeAxesPositions = encodeFieldToParams "chxp" . strAxesPositions

-- axis ranges
strAxisRange (idx, Just range) = show idx ++ "," ++ intercalate "," (encodeRange range)
                                 where encodeRange (Range (start,end) interval) | interval == Nothing = [show start, show end]
                                                                                | otherwise = [show start, show end, show (fromJust interval)]

strAxesRanges = convertFieldToString strAxisRange axisRange

encodeAxesRanges = encodeFieldToParams "chxr" . strAxesRanges

-- axis style
strAxisStyle (idx, Just style) = show idx ++ "," ++ intercalate "," (catMaybes (encodeStyle style))
                                    where encodeStyle (Style a b c d e) = [Just a,
                                                                           liftM show b,
                                                                           liftM encodeAlign c,
                                                                           liftM encodeDrawingControl d,
                                                                           liftM id e]
                                          encodeAlign c = case c of
                                                            AxisStyleLeft -> "-1"
                                                            AxisStyleCenter -> "0"
                                                            AxisStyleRight -> "1"
                                          encodeDrawingControl d = case d of
                                                                     DrawLines -> "l"
                                                                     DrawTicks -> "t"
                                                                     DrawLinesTicks -> "lt"

strAxesStyles = convertFieldToString strAxisStyle axisStyle

encodeAxesStyles = encodeFieldToParams "chxs" . strAxesStyles


-- GRID
instance ChartItem ChartGrid where
    set grid = updateChart $ \chart -> chart { chartGrid = Just grid }

    encode grid = asList ("chg", encodeGrid grid) where
                  encodeGrid (ChartGrid a b c d e f)= intercalate "," $ catMaybes [Just (show a),
                                                                                   Just (show b),
                                                                                   liftM show c,
                                                                                   liftM show d,
                                                                                   liftM show e,
                                                                                   liftM show f]

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
                                                 encodeMaybe $ chartLegend chart,
                                                 encodeMaybe $ chartAxes   chart,
                                                 encodeMaybe $ chartGrid   chart]

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

makeGrid = defaultGrid

-- helper functions

setChartSize w h = set (Size w h)

setChartType = set

setChartTitle = set

addChartData = addDataToChart

addColors = set

addColor  = addColorToChart

addFill = addFillToChart

addLegend = set

addAxis = addAxisToChart

addGrid = set

-- API Functions

getChartData m = execState m defaultChart

getUrl =  convertToUrl . getChartData

debugChart = getUrl $ do setChartSize 640 400
                         setChartType Line
                         setChartTitle "Test"
                         addChartData  [1,2,3,4,5]
                         addChartData  [3,4,5,6,7]
                         addColor "FF0000"
                         addColor "00FF00"
                         addFill $ solid "DD00DD" Background
                         addLegend $ legendWithPosition ["t1","t2"] VBottom
                         addAxis $ defaultAxis { axisStyle = Just defaultAxisStyle }
                         addGrid $ makeGrid { lineSegmentLength = Just 5 }
