{-# LANGUAGE TypeSynonymInstances #-}
import Control.Monad.State
import Data.List
import Data.Char (chr, ord)
import Numeric (showHex)

data ChartSize = Size Int Int deriving Show

data ChartType = Line |
                 Sparklines |
                 LineXY deriving Show

type ChartTitle = Maybe String


data ChartData = D [Int] |
                 XY [(Int,Int)] deriving Show

data Chart = Chart { chartSize  :: ChartSize,
                     chartType  :: ChartType,
                     chartTitle :: ChartTitle,
                     chartData  :: [ChartData] } deriving Show

newtype ChartM a = ChartM {unChartM :: Chart -> a }

instance Monad ChartM where
  return = ChartM . const

  m >>= k  = ChartM $ \chart -> let v = unChartM m chart
                               in unChartM (k v) chart

class ChartItem c where
  set :: c -> ChartM Chart
  encode :: c -> [(String,String)]


defaultChart = Chart { chartSize  = Size 320 200,
                       chartType  = Line,
                       chartData  = [],
                       chartTitle = Nothing }

-- Setting/Encoding Chart Data

getChart = ChartM id

updateChart u = do chart <- getChart
                   return $ u chart

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
                                    Line       -> "lc"
                                    LineXY     -> "lxy"
                                    Sparklines -> "ls"

-- title
instance ChartItem ChartTitle where
    set title = updateChart $ \chart -> chart { chartTitle = title }

    encode title = case title of
                     Just t -> asList ("chl", t)
                     _      -> []

-- helper functions for syntactic sugar

setChartSize :: Int -> Int -> ChartM Chart
setChartSize w h = set (Size w h)

setChartType :: ChartType -> ChartM Chart
setChartType = set

setChartTitle :: String -> ChartM Chart
setChartTitle = set . Just

{-


addChartData = updateChart . flip addDataToChart

addDataToChart cd d = cd { chartData = old ++ [d] }
                      where old = chartData cd


-- data
encodeChartData datas = asList ("chd", encodeSimple $ normalise datas)

normalise datas = concatMap n datas
                  where n (D d) = [d]
                        n (XY d) = map fst d :  [map snd d]

-}

-- URL Conversion
getParams chart =  concat [encode $ chartType chart,
                           encode $ chartTitle chart,
                           encode $ chartSize chart]

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

getChartData m = unChartM m defaultChart

getUrl =  convertToUrl . getChartData

debugChart = getUrl $ do setChartSize 320 200
                         setChartType Line
                         setChartTitle "Test"
                         -- addChartData $ D [1,2,3,4,5]
                         -- addChartData $ D [3,4,5,6,7]
