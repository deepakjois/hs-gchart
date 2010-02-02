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

data ChartData = D [[Int]] deriving Show

type ChartColors = Maybe [String] 

data Chart = Chart { chartSize   :: ChartSize,
                     chartType   :: ChartType,
                     chartTitle  :: ChartTitle,
                     chartData   :: ChartData,
                     chartColors :: ChartColors } deriving Show

type ChartM a = State Chart a


class ChartItem c where
  set :: c -> ChartM ()
  encode :: c -> [(String,String)]


defaultChart = Chart { chartSize  = Size 320 200,
                       chartType  = Line,
                       chartData  = D [],
                       chartTitle = Nothing,
                       chartColors = Nothing}

-- Setting/Encoding Chart Data

updateChart u = do chart <- get
                   put $ u chart

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

-- data
-- FIXME just a placeholder for now
instance ChartItem ChartData where
    set cData = updateChart $ \chart -> chart { chartData = cData }

    encode (D cData) = asList ("chd", encodeSimple cData)


addDataToChart :: [Int] -> ChartM ()
addDataToChart d = do c <- get
                      let D old = chartData c
                          new = D (old ++ [d])
                      set new


-- color

instance ChartItem ChartColors where
    set colors = updateChart $ \chart -> chart { chartColors = colors }

    encode colors = case colors of 
                      Just c -> asList ("chco", intercalate "," c)
                      _      -> []

-- URL Conversion
getParams chart =  concat [encode $ chartType chart,
                           encode $ chartTitle chart,
                           encode $ chartSize chart,
                           encode $ chartData chart,
                           encode $ chartColors chart]

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


-- helper functions for syntactic sugar

setChartSize :: Int -> Int -> ChartM ()
setChartSize w h = set (Size w h)

setChartType :: ChartType -> ChartM ()
setChartType = set

setChartTitle :: String -> ChartM ()
setChartTitle = set . Just

addChartData = addDataToChart

addColors = set . Just

-- API Functions

getChartData m = execState m defaultChart

getUrl =  convertToUrl . getChartData

debugChart = getUrl $ do setChartSize 640 400
                         setChartType Line
                         setChartTitle "Test"
                         addChartData  [1,2,3,4,5]
                         addChartData  [3,4,5,6,7]
                         addColors ["FF0000","00FF00"]
