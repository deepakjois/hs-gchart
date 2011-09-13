{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.GChart.ChartItems.Colors where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util

import Data.List(intercalate)
-- Chart Colors
instance ChartItem ChartColors where
    set colors = updateChart $ \chart -> chart { chartColors = Just colors }

    encode (ChartColors colors) = asList ("chco", intercalate "," colors)

-- TODO: Fill Area

-- Chart Fills
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


