module Graphics.GChart.ChartItems.Basics where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util


-- Chart Size
instance ChartItem ChartSize where
    set size = updateChart $ \chart -> chart { chartSize = Just size }

    encode size =  asList ("chs", show width ++ "x" ++ show height) where
                   Size width height = size


-- Chart Type
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
                                    Formula              -> "tx"
