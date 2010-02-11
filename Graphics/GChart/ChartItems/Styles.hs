{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.GChart.ChartItems.Styles where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util

import Data.List(intercalate)
import Control.Monad(liftM)
import Data.Maybe(catMaybes)

-- Bar Width and Spacing
instance ChartItem BarChartWidthSpacing where
    set widthspacing = updateChart $ \chart -> chart { barChartWidthSpacing = Just widthspacing }

    encode (Nothing           , Nothing)               = error "Invalid Values"
    encode (Just Automatic    , Just (Relative _))     = error "Invalid Values"
    encode (Just (BarWidth _) , Just (Relative _))     = error "Invalid Values"
    encode (Just Automatic    , Nothing)               = asList ("chbh", "a")
    encode (Just Automatic    , Just (Fixed (b,g)))    = asList ("chbh", intercalate "," ["a",show b, show g])
    encode (Just (BarWidth bw), Just (Fixed (b,g)))    = asList ("chbh", intercalate "," [show bw, show b ,show g])
    encode (Just (BarWidth bw), Nothing)               = asList ("chbh", show bw)
    encode (Nothing           , Just (Relative (b,g))) = asList ("chbh", intercalate "," ["r", show b, show g])
    encode (_,_)                                       = error "Invalid Values"

-- TODO: Bar Chart Zero Line

-- Chart Margins
instance ChartItem ChartMargins where
    set margins = updateChart $ \chart -> chart { chartMargins = Just margins }

    encode (ChartMargins a b c d e) = asList ("chma",intercalate "|" $ cm:[lm])
        where cm  = intercalate ","  [show a, show b, show c, show d]
              lm = case e of
                     Just (x,y) -> show x ++ "," ++ show y
                     _          -> ""

-- Line Styles
instance ChartItem ChartLineStyles where
    set styles = updateChart $ \chart -> chart { chartLineStyles = Just styles }

    encode styles = asList ("chls", intercalate "|" $ map encodeStyle styles) where
                     encodeStyle (LS a b c) = intercalate "," $ map showFloat [a, b, c]

-- Grid Lines
instance ChartItem ChartGrid where
    set grid = updateChart $ \chart -> chart { chartGrid = Just grid }

    encode grid = asList ("chg", encodeGrid grid) where
                  encodeGrid (ChartGrid a b c d e f)= intercalate "," $ catMaybes [Just (show a),
                                                                                   Just (show b),
                                                                                   liftM show c,
                                                                                   liftM show d,
                                                                                   liftM show e,
                                                                                   liftM show f]
-- ChartItem instance for ChartMarkers
instance ChartItem ChartMarkers where
    set markers    = updateChart $ \chart -> chart { chartMarkers = Just markers }
    encode markers = asList ("chm",intercalate "|" $ map encodeChartMarker markers)

-- Shape Markers
instance ChartMarker ShapeMarker where
    encodeChartMarker marker = optionalat ++  (intercalate "," $  [marker_type, color, idx, datapoint, size] ++ [show priority | priority /= 0]) where
                                 marker_type = case shapeType marker of
                                                 ShapeArrow       -> "a"
                                                 ShapeCross       -> "c"
                                                 ShapeDiamond     -> "d"
                                                 ShapeCircle      -> "o"
                                                 ShapeSquare      -> "s"
                                                 VerticalLine     -> "v"
                                                 VerticalLineFull -> "V"
                                                 HorizontalLine   -> "h"
                                                 ShapeX           -> "x"

                                 color = shapeColor marker

                                 datapoint = case shapeDataPoint marker of
                                               DataPoint x                  ->  show x
                                               DataPointEvery               ->  "-1"
                                               DataPointEveryN x            ->  "-" ++ show x
                                               DataPointEveryNRange (x,y) n ->  intercalate ":"$  map show [x,y,n]
                                               DataPointXY (x,y)            ->  show x ++ ":" ++ show y

                                 idx  = show $ shapeDataSetIdx marker
                                 size = show $ shapeSize marker
                                 priority = shapePriority marker

                                 optionalat = [ '@' | isDataPointXY $ shapeDataPoint marker ]
                                 isDataPointXY (DataPointXY _) = True
                                 isDataPointXY _               = False
-- Range Markers
instance ChartMarker RangeMarker where
    encodeChartMarker marker = intercalate "," [rangetype, color,"0",x,y] where
                                 rangetype = case rangeMarkerType marker of
                                               RangeMarkerHorizontal -> "r"
                                               RangeMarkerVertical   -> "R"

                                 color = rangeMarkerColor marker
                                 x = show.fst $ rangeMarkerRange marker
                                 y = show.snd $ rangeMarkerRange marker

-- Financial Markers
instance ChartMarker FinancialMarker where
    encodeChartMarker marker = (intercalate "," $  ["F", color, idx, datapoint, size] ++ [show priority | priority /= 0]) where
                                 color = financeColor marker

                                 datapoint = case financeDataPoint marker of
                                               DataPoint x                  ->  show x
                                               DataPointEvery               ->  "-1"
                                               DataPointEveryN x            ->  "-" ++ show x
                                               DataPointEveryNRange (x,y) n ->  error "Invalid value for finanical marker"
                                               DataPointXY (x,y)            ->   show x ++ ":" ++ show y

                                 idx  = show $ financeDataSetIdx marker
                                 size = show $ financeSize marker
                                 priority = financePriority marker

-- Pie Chart Orientation
instance ChartItem PieChartOrientation where
    set orientation = updateChart $ \chart -> chart { pieChartOrientation = Just orientation }

    encode (PCO orientation) = asList ("chp", show orientation)
