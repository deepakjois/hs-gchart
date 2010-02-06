module Graphics.GChart.ChartItems.Styles where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util

import Data.List(intercalate)
import Control.Monad(liftM)
import Data.Maybe(catMaybes)

-- TODO: Bar Width and Spacing

-- TODO: Bar Chart Zero Line

-- Chart Margins
instance ChartItem ChartMargins where
    set margins = updateChart $ \chart -> chart { chartMargins = Just margins }

    encode (ChartMargins a b c d e) = asList ("chma",intercalate "|" $ cm:[lm])
        where cm  = intercalate ","  [show a, show b, show c, show d]
              lm = case e of
                     Just (x,y) -> show x ++ "," ++ show y
                     _          -> ""

-- TODO: Line Styles

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
-- TODO: Shape, Range, Financial Markers