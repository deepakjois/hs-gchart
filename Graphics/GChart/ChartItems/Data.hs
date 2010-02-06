module Graphics.GChart.ChartItems.Data where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util

import Graphics.GChart.DataEncoding

-- Chart Data
instance ChartItem ChartData where
    set cData = updateChart $ \chart -> chart { chartData = cData }

    encode datas = asList ("chd", encodeData datas)
                        where encodeData (Simple d)   = encodeSimple d
                              encodeData (Text d)     = encodeText d
                              encodeData (Extended d) = encodeExtended d

instance ChartDataEncodable Int where
    addEncodedChartData d cd@(Simple old) = Simple $ old ++ [d]
    addEncodedChartData d cd@(Extended old) = Extended $ old ++ [d]
    addEncodedChartData d _ = error "Invalid type for specified encoding. Use float data"

instance ChartDataEncodable Float where
    addEncodedChartData d cd@(Text old) = Text $ old ++ [d]
    addEncodedChartData d _             = error "Invalid type for specified encoding. Use int data"
