{-# LANGUAGE TypeSynonymInstances, NoMonomorphismRestriction #-}
{-|

Import this module to generate charts using the Google Chart API.

For more examples, refer to @Examples.hs@ in the source tarball, or download it
directly from Github : <http://github.com/hs-gchart/>.

For documentation regarding the full data model, refer to 
"Graphics.GChart.Types".

For more information about the Google Chart API, refer to
<http://code.google.com/apis/chart/>

-}
module Graphics.GChart ( 
  module Graphics.GChart.Types,

  -- * Setting Chart Parameters
  {-| 

Use these functions to set the parameters of the chart.

These functions must be called inside a @do@ block, which can then be passed
onto 'getUrl' or 'getChartData'.  For e.g, here is a simple pie chart function

@
generatePieChart = getChartUrl $ do setChartSize 640 400
                                 setChartType Pie
                                 setChartTitle \"Test\"
                                 addChartData  ([1,2,3,4,5]::[Int])
                                 addColor \"FF0000\"
                                 setLegend $ legend [\"t1\",\"t2\", \"t3\",\"t4\",\"t5\"]
                                 setLabels $ [\"Test 1\", \"Test 2\", \"Test 3\", \"Test 4\", \"Test 5\"]
@

-} 
  setChartSize, setChartType, setDataEncoding, setChartTitle,addChartData, setColors, addColor, addFill,
  setLegend, addAxis, setGrid, setLabels,
  -- * Retrieving Chart data
  getChartData, getChartUrl, convertToUrl,

  -- * Smart Constructors 
  -- | These functions can be used to construct chart
  -- parameters more conveniently
  solid, legend, legendWithPosition, makeAxis, makeGrid,
  simple,text,extended
) where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems
import Graphics.GChart.DataEncoding

import Data.List

{- Smart Constructors -}

-- | generates a 'Solid' fill from a hex color value
solid = Fill . Solid

-- | generates a 'ChartLegend' from a list of labels
legend :: [String] -> ChartLegend
legend labels = Legend labels Nothing

-- | generats a 'ChartLegend' from a list of lables and a 'LegendPosition'
legendWithPosition :: [String] -> LegendPosition -> ChartLegend
legendWithPosition labels position = Legend labels (Just position)

{-| returns a default axis. Use this to override the fields with your own
 values. For e.g :

@
makeAxis { 'axisType' = 'AxisTop',
           'axisLabels' = [\"0\",\"50\",\"100\"] }
@

-}
makeAxis :: Axis
makeAxis = defaultAxis


{-| returns a default axis. Use this to override the fields with your own
 values. For e.g :

@
makeGrid { 'xAxisStep' = 10,
           'yAxisStep' = 10,
            xOffset = Just 5 }
@

-}
makeGrid :: ChartGrid
makeGrid = defaultGrid

-- | Use this to specify the 'Simple' encoding for the 'setDataEncoding'
-- function.
simple :: ChartData
simple = Simple []

-- | Use this to specify the 'Text' encoding for the 'setDataEncoding'
-- function.
text :: ChartData
text = Text []

-- | Use this to specify the 'Extended' encoding for the 'setDataEncoding'
--  function.
extended :: ChartData
extended = Extended []

{- Setting Chart Parameters-}

-- | Set the chart size by passing the width and the height in pixels
-- For e.g : @setChartSize 320 200@
setChartSize :: Int -> Int -> ChartM ()
setChartSize w h = set (Size w h)

-- | Set the chart type by passing a 'ChartType'
setChartType :: ChartType -> ChartM ()
setChartType = set

-- | Set the chart title by passing a 'ChartTitle'
setChartTitle :: String -> ChartM ()
setChartTitle = set

{-| Use it with 'simple', 'text' or 'extended' to specify the encoding. For e.g

@
setDataEncoding simple
@

Make sure you pass in values of the right type, Int for simple and extended
encoding, and Float for text encoding.
-}
setDataEncoding :: ChartData -> ChartM ()
setDataEncoding = set

{-| Add data to chart. Make sure you have set the data encoding using 
 'setDataEncoding' before calling this function, otherwise it may generate
 gibberish, or throw an error
-}
addChartData :: ChartDataEncodable a => [a] -> ChartM ()
addChartData = addDataToChart

-- | Pass a list of colors corresponding to the datasets in the chart
setColors :: [Color] -> ChartM ()
setColors = set . ChartColors

{-| Add a color to the chart. This color will be added to the list 'ChartColors'.

Make sure you do not include a call to 'setColors' at any time after a call to
'addColor', since this will lead to all previous values being erased.

-}
addColor :: Color -> ChartM()
addColor  = addColorToChart

-- | Add a 'Fill' to the chart
addFill :: Fill -> ChartM ()
addFill = addFillToChart

-- | Set a Legend for the chart
setLegend :: ChartLegend -> ChartM ()
setLegend = set

-- | Add an 'Axis' to the chart
addAxis :: Axis -> ChartM ()
addAxis = addAxisToChart

-- | Set a 'ChartGrid' for the chart
setGrid :: ChartGrid -> ChartM ()
setGrid = set

-- | Set labels for the chart
setLabels :: [String] -> ChartM ()
setLabels = set . ChartLabels

{- Retrieving Chart Data -}

-- | Extracts the data out of the monad and returns a value of type 'Chart'
getChartData :: ChartM () -> Chart
getChartData = getChartDataFromChartM

-- | Extracts the data out of the monad and returns a URL string for the chart 
getChartUrl :: ChartM () -> String
getChartUrl =  convertToUrl . getChartData

-- | Converts a value of type 'Chart' to a URL
convertToUrl :: Chart -> String
convertToUrl chart = baseURL ++ intercalate "&" urlparams where
    baseURL = "http://chart.apis.google.com/chart?"
    urlparams = [urlEnc a ++ "=" ++ urlEnc b | (a,b) <- getParams chart]

debugPieChart = getChartUrl $ do setChartSize 640 400
                                 setChartType Pie
                                 setChartTitle "Test"
                                 addChartData  ([1,2,3,4,5]::[Int])
                                 addColor "FF0000"
                                 setLegend $ legend ["t1","t2", "t3","t4","t5"]
                                 setLabels $ ["Test 1", "Test 2", "Test 3", "Test 4", "Test 5"]

debugBarChart = getChartUrl $ do setChartSize 640 400
                                 setChartType BarVerticalGrouped
                                 setDataEncoding text
                                 addChartData  ([100,200,300,400,500]::[Float])
                                 addChartData  ([3,4,5,6,7]::[Float])
                                 addChartData  ([4.0,5.0,6.0,7.0,8]::[Float])
                                 addAxis $ makeAxis { axisType = AxisLeft,axisLabels = Just ["0","100"] }
                                 setColors ["FF0000","00FF00","0000FF"]
                                 setLegend $ legend ["Set 1", "Set 2", "Set 3"]
