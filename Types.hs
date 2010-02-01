import Control.Monad.State
import Data.List
import Data.Char (chr, ord)
import Numeric (showHex)

data ChartSize = Size Int Int deriving Show

data ChartType = Line |
                 Sparklines |
                 LineXY deriving Show

data ChartData = D [Int] |
                 XY [(Int,Int)] deriving Show

data Chart = Chart { chartSize  :: ChartSize,
                     chartType  :: ChartType,
                     chartData  :: [ChartData],
                     chartTitle :: Maybe String } deriving Show

type ChartM a = State Chart a


class ChartItem c where
  set :: c -> ChartM ()
  encode :: c -> [(String,String)]


defaultChart = Chart { chartSize  = Size 320 200,
                       chartType  = Line,
                       chartData  = [],
                       chartTitle = Nothing }

-- Setting Chart Data

updateChart u = do cd <- get
                   put $ u cd
                   return ()

asList a = [a]

instance ChartItem ChartSize where
    set size = updateChart $ \cd -> cd { chartSize = size }

    encode size =  asList ("chs", show width ++ "x" ++ show height) where
                   Size width height = size


setChartSize w h = updateChart $ \cd -> cd {  chartSize = Size w h }

setChartType t = updateChart $ \cd -> cd { chartType = t }

setChartTitle t = updateChart $ \cd -> cd { chartTitle = Just t } 

addChartData = updateChart . flip addDataToChart

addDataToChart cd d = cd { chartData = old ++ [d] }
                      where old = chartData cd

-- Encoding Chart Data



-- type
encodeChartType t = asList ("cht",cType)
                    where cType = case t of
                                    Line       -> "lc"
                                    LineXY     -> "lxy"
                                    Sparklines -> "ls"

-- data
encodeChartData datas = asList ("chd", encodeSimple $ normalise datas)

normalise datas = concatMap n datas
                  where n (D d) = [d]
                        n (XY d) = map fst d :  [map snd d]

-- size
encodeChartSize size = asList ("chs", show width ++ "x" ++ show height) where
                       Size width height = size


-- title
encodeChartTitle (Just title) = asList ("chl",title)
encodeChartTitle _ = []


-- URL Conversion
getParams chart = encodeChartType  (chartType  chart) ++ 
                  encodeChartData  (chartData  chart) ++
                  encodeChartTitle (chartTitle chart) ++
                  encodeChartSize  (chartSize  chart)

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


-- API Functions

getChartData m = execState m defaultChart

getUrl =  convertToUrl . getChartData

debugChart = getUrl $ do setChartSize 320 200
                         setChartType Line
                         addChartData $ D [1,2,3,4,5]
                         addChartData $ D [3,4,5,6,7]
