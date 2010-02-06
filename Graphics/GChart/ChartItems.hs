{-# LANGUAGE TypeSynonymInstances, NoMonomorphismRestriction #-}
module Graphics.GChart.ChartItems (
  ChartM,
  ChartItem(set),
  ChartDataEncodable,
  getChartDataFromChartM,
  addDataToChart,
  addColorToChart,
  addFillToChart,
  addAxisToChart,
  getParams
) where

import Graphics.GChart.Types
import Graphics.GChart.DataEncoding

import Control.Monad.State
import Data.List
import Data.Maybe

-- Monad
type ChartM a = State Chart a

-- | Typeclass abstracting all the fields in a chart
class ChartItem c where
  -- set the field
  set :: c -> ChartM ()
  -- encode the field into string params
  encode :: c -> [(String,String)]


-- Setting/Encoding Chart Data

updateChart u = do chart <- get
                   put $ u chart

asList a = [a]

getChartDataFromChartM m = execState m defaultChart

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


-- CHART MARGINS
instance ChartItem ChartMargins where
    set margins = updateChart $ \chart -> chart { chartMargins = Just margins }

    encode (ChartMargins a b c d e) = asList ("chma",intercalate "|" $ cm:[lm])
        where cm  = intercalate ","  [show a, show b, show c, show d]
              lm = case e of
                     Just (x,y) -> show x ++ "," ++ show y
                     _          -> ""

-- URL Conversion
-- FIXME : too much boilerplate. Can it be reduced?
encodeMaybe Nothing = [("","")]
encodeMaybe (Just x)  = encode x

getParams chart =  filter (/= ("","")) $ concat [encode $ chartType chart,
                                                 encode $ chartSize chart,
                                                 encode $ chartData chart,
                                                 encodeMaybe $ chartTitle   chart,
                                                 encodeMaybe $ chartColors  chart,
                                                 encodeMaybe $ chartFills   chart,
                                                 encodeMaybe $ chartLegend  chart,
                                                 encodeMaybe $ chartAxes    chart,
                                                 encodeMaybe $ chartGrid    chart,
                                                 encodeMaybe $ chartLabels  chart,
                                                 encodeMaybe $ chartMargins chart]

