{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.GChart.ChartItems.Labels where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util

import Data.List(intercalate)
import Control.Monad(liftM)
import Data.Maybe(catMaybes, fromJust)

-- Chart Title
instance ChartItem ChartTitle where
    set title = updateChart $ \chart -> chart { chartTitle = Just title }

    encode title =  ("chtt", titleStr title) : [("chts", chts) | chts /= ""]
                    where chts = encodeColorAndFontSize title

encodeColorAndFontSize title =
    intercalate "," $ catMaybes [titleColor title,
                                 liftM show . titleFontSize  $ title]


-- Chart Legend
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

-- Chart Labels (Pie Chart, Google-O-Meter)
instance ChartItem ChartLabels where
    set labels = updateChart $ \chart -> chart { chartLabels = Just labels }

    encode (ChartLabels labels) = asList ("chl", intercalate "|" labels)

-- Axis Styles and Labels
instance ChartItem ChartAxes where
    set axes = updateChart $ \chart -> chart { chartAxes = Just axes }

    encode axes = filter (/= ("","")) $ map (\f -> f axes) [encodeAxesTypes,
                                                            encodeAxesLabels,
                                                            encodeAxesPositions,
                                                            encodeAxesRanges,
                                                            encodeAxesStyles]

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


-- TODO: Data Point Labels
