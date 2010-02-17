module Graphics.GChart.ChartItems.Data where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems.Util

import Graphics.GChart.DataEncoding

import Data.List(intercalate)

-- Chart Data
instance ChartItem ChartData where
    set cData = updateChart $ \chart -> chart { chartData = Just cData }

    encode datas = asList ("chd", encodeData datas)
                        where encodeData (Simple d)   = encodeSimple d
                              encodeData (Text d)     = encodeText d
                              encodeData (Extended d) = encodeExtended d

instance ChartItem ChartDataScales where
    set scales = updateChart $ \chart -> chart { chartDataScales = Just scales }

    encode (CDS scales) = asList ("chds", intercalate "," $ map (\(min,max) -> showFloat min ++ "," ++ showFloat max) scales)

instance ChartDataEncodable Int where
    addEncodedChartData d cd@(Simple old) = Simple $ old ++ [d]
    addEncodedChartData d cd@(Extended old) = Extended $ old ++ [d]
    addEncodedChartData d _ = error "Invalid type for specified encoding. Use float data"

instance ChartDataEncodable Float where
    addEncodedChartData d cd@(Text old) = Text $ old ++ [d]
    addEncodedChartData d _             = error "Invalid type for specified encoding. Use int data"

instance ChartItem QREncoding where
    set qrEnc = updateChart $ \chart -> chart { qrEncoding = Just qrEnc }

    encode qrEnc = asList ("choe", encStr)
                   where encStr = case qrEnc of
                                    UTF8     -> "UTF-8"
                                    Shift_JIS -> "Shift_JIS"
                                    ISO8859_1  -> "ISO-8859-1"

instance ChartItem ChartLabelData where
    set chld = updateChart $ \chart -> chart { chartLabelData = Just chld }

    encode (QRLabelData ec m) = asList ("chld", concat [show ec, "|", show m])
