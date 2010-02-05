{-# LANGUAGE TypeSynonymInstances, NoMonomorphismRestriction #-}
module ChartItems where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Char (chr, ord)
import Numeric (showHex)
import Types

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
                                    Line                 -> "lc"
                                    LineXY               -> "lxy"
                                    Sparklines           -> "ls"
                                    Pie                  -> "p"
                                    Pie3D                -> "p3"
                                    PieConcentric        -> "pc"
                                    BarHorizontalStacked -> "bhs"
                                    BarVerticalStacked   -> "bvs"
                                    BarHorizontalGrouped -> "bhg"
                                    BarVerticalGrouped   -> "bvg"
                                    Venn                 -> "v"
                                    ScatterPlot          -> "s"
                                    Radar                -> "r"
                                    GoogleOMeter         -> "gom"

-- title
instance ChartItem ChartTitle where
    set title = updateChart $ \chart -> chart { chartTitle = Just title }

    encode title = asList ("chtt", title)


-- data
-- FIXME just a placeholder for now
instance ChartItem ChartData where
    set cData = updateChart $ \chart -> chart { chartData = cData }

    encode datas = asList ("chd", encodeData datas)
                        where encodeData (Simple d)   = encodeSimple d
                              encodeData (Text d)     = encodeText d
                              encodeData (Extended d) = encodeExtended d


class Num a => ChartDataEncodable a where
    addEncodedChartData :: [a] -> ChartData -> ChartData

instance ChartDataEncodable Int where
    addEncodedChartData d cd@(Simple old) = Simple $ old ++ [d]
    addEncodedChartData d cd@(Extended old) = Extended $ old ++ [d]
    addEncodedChartData d _ = error "Invalid type for specified encoding. Use float data"

instance ChartDataEncodable Float where
    addEncodedChartData d cd@(Text old) = Text $ old ++ [d]
    addEncodedChartData d _             = error "Invalid type for specified encoding. Use int data"

addDataToChart d = do c <- get
                      let old = chartData c
                      set $ addEncodedChartData d old

-- color

instance ChartItem ChartColors where
    set colors = updateChart $ \chart -> chart { chartColors = Just colors }

    encode (ChartColors colors) = asList ("chco", intercalate "," colors)



addColorToChart color = do chart <- get
                           let (ChartColors old) = fromMaybe (ChartColors []) $ chartColors chart
                               new = ChartColors $ old ++ [color]
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
                                                                    LegendBottom  -> "b"
                                                                    LegendTop     -> "t"
                                                                    LegendVBottom -> "bv"
                                                                    LegendVTop    -> "tv"
                                                                    LegendRight   -> "r"
                                                                    LegendLeft    -> "l"
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

-- LABELS (Pie Chart, Google-O-Meter
instance ChartItem ChartLabels where
    set labels = updateChart $ \chart -> chart { chartLabels = Just labels }

    encode (ChartLabels labels) = asList ("chl", intercalate "|" labels)


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
                                                 encodeMaybe $ chartGrid   chart,
                                                 encodeMaybe $ chartLabels chart]

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


encodeText datas = "t:" ++ intercalate "|" (map encData datas) where
    encData = intercalate "," . map encDatum
    encDatum i | i >= 0 && i <= 100 = showDecimal i
               | otherwise          = "-1"
    showDecimal :: Float -> String
    showDecimal i | ((makeFloat (truncate i)) - i) == 0  = show $ truncate i
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


-- smart constructors

solid = Fill . Solid

legend labels = Legend labels Nothing

legendWithPosition labels position = Legend labels (Just position)

makeAxis = defaultAxis

makeGrid = defaultGrid

simple = Simple []

text = Text []

extended = Extended []

-- helper functions

setChartSize w h = set (Size w h)

setChartType = set

setChartTitle = set

setDataEncoding = set

addChartData = addDataToChart

addColors = set . ChartColors

addColor  = addColorToChart

addFill = addFillToChart

addLegend = set

addAxis = addAxisToChart

addGrid = set

addLabels = set . ChartLabels

-- API Functions

getChartData m = execState m defaultChart

getUrl =  convertToUrl . getChartData

debugPieChart = getUrl $ do setChartSize 640 400
                            setChartType Pie
                            setChartTitle "Test"
                            addChartData  ([1,2,3,4,5]::[Int])
                            addColor "FF0000"
                            addLegend $ legend ["t1","t2", "t3","t4","t5"]
                            addLabels $ ["Test 1", "Test 2", "Test 3", "Test 4", "Test 5"]

debugBarChart = getUrl $ do setChartSize 640 400
                            setChartType BarVerticalGrouped
                            setDataEncoding text
                            addChartData  ([100,200,300,400,500]::[Float])
                            addChartData  ([3,4,5,6,7]::[Float])
                            addChartData  ([4.0,5.0,6.0,7.0,8]::[Float])
                            addAxis $ makeAxis { axisType = AxisLeft,axisLabels = Just ["0","100"] }
                            addColors ["FF0000","00FF00","0000FF"]
                            addLegend $ legend ["Set 1", "Set 2", "Set 3"]

