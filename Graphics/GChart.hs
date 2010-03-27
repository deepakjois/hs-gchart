{-# LANGUAGE TypeSynonymInstances, NoMonomorphismRestriction #-}
{-|

Import this module to generate charts using the Google Chart API.

For more information about the Google Chart API, refer to

- Chart API Intro <http://code.google.com/apis/chart/image_charts.html>

- Getting Started <http://code.google.com/apis/chart/docs/making_charts.html>

For documentation full Haskell data model, refer to "Graphics.GChart.Types".

Here is an example to use the functions in the module to generate a chart URL :

@
generatePieChart = getChartUrl $ do setChartSize 640 400
                                 setChartType Pie
                                 setChartTitle \"Test\"
                                 addChartData  ([1,2,3,4,5]::[Int])
                                 addColor \"FF0000\"
                                 setLegend $ legend [\"t1\",\"t2\", \"t3\",\"t4\",\"t5\"]
                                 setLabels $ [\"Test 1\", \"Test 2\", \"Test 3\", \"Test 4\", \"Test 5\"]
@

For examples, refer to @Examples.hs@ in the source tarball, or download it
directly from Github :
<http://github.com/deepakjois/hs-gchart/blob/master/examples/Examples.hs>.


The module constists of:

- Smart Constructors - to make it convenient to construct data types

- Functions to set chart data

- Functions to retrieve chart data in form of URL or Haskell data type

-}
module Graphics.GChart (
  module Graphics.GChart.Types,

  -- * Smart Constructors
   solid                ,
   legend               ,
   legendWithPosition   ,
   makeAxis             ,
   makeGrid             ,
   simple               ,
   text                 ,
   extended             ,
   automatic            ,
   automaticWithSpacing ,
   barwidth             ,
   barwidthspacing      ,
   relative             ,
   makeShapeMarker      ,
   makeRangeMarker      ,
   makeFinancialMarker  ,
   makeLineMarker       ,
   makeLineStyle        ,

  -- * Setting Chart Parameters
   setChartSize                     ,
   setChartHeight                   ,
   setChartType                     ,
   setChartTitle                    ,
   setChartTitleWithColor           ,
   setChartTitleWithColorAndFontSize,
   setDataEncoding                  ,
   addChartData                     ,
   addDataScale                     ,
   addChartDataXY                   ,
   setColors                        ,
   addColor                         ,
   addFill                          ,
   setLegend                        ,
   addAxis                          ,
   setGrid                          ,
   addShapeMarker                   ,
   addRangeMarker                   ,
   addFinancialMarker               ,
   addLineMarker                    ,
   addLineFill                      ,
   setLabels                        ,
   setLabel                         ,
   setBarWidthSpacing               ,
   setPieChartOrientation           ,
   addLineStyle                     ,
   setFormula                       ,
   setQREncoding                    ,
   setQRWidth                       ,
   setQRErrorCorrection             ,
  -- * Retrieving Chart Data

  getChartData,
  getChartUrl,
  convertToUrl
)  where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems
import Graphics.GChart.DataEncoding

import Data.List



-- | generates a 'Solid' fill from a hex color value
solid :: Color -> FillType -> Fill
solid color filltype = Fill (Solid color) filltype

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

-- | Set automatic bar width for bar chart
automatic :: BarChartWidthSpacing
automatic = (Just Automatic,Nothing)

-- | Set automatic bar width for bar chart, with spacing values
automaticWithSpacing :: Int -> Int -> BarChartWidthSpacing
automaticWithSpacing b g= (Just Automatic, Just (Fixed (b,g)))

-- | Set bar width for chart
barwidth :: Int -> BarChartWidthSpacing
barwidth n = (Just (BarWidth n), Nothing)

-- | Set bar width and spacing for chart
barwidthspacing :: Int -> Int -> Int -> BarChartWidthSpacing
barwidthspacing bw b g = (Just (BarWidth bw), Just (Fixed (b,g)))

-- | Set relative spacing
relative :: Float -> Float -> BarChartWidthSpacing
relative b g = (Nothing, Just (Relative (b,g)))

-- | Shape Marker
makeShapeMarker :: ShapeMarker
makeShapeMarker = defaultShapeMarker

-- | Range Marker
makeRangeMarker :: RangeMarker
makeRangeMarker = defaultRangeMarker

-- | Line Marker
makeLineMarker :: LineMarker
makeLineMarker = defaultLineMarker

-- | Financial Marker
makeFinancialMarker :: FinancialMarker
makeFinancialMarker = defaultFinancialMarker

-- | Line Style
makeLineStyle :: LineStyle
makeLineStyle = defaultLineStyle

-- | Set the chart size by passing the width and the height in pixels
-- For e.g : @setChartSize 320 200@
setChartSize :: Int -> Int -> ChartM ()
setChartSize w h = set (Size w h)

-- | Set chart height only. Applicable to 'Formula' charts
-- This will set the width to 0 which will automatically
-- be excluded when the data is being encoded
setChartHeight :: Int -> ChartM ()
setChartHeight h = set (Size 0 h)

-- | Set the chart type by passing a 'ChartType'
setChartType :: ChartType -> ChartM ()
setChartType = set

-- | Set the chart title by passing a 'ChartTitle'
setChartTitle :: String -> ChartM ()
setChartTitle title = set $ ChartTitle title Nothing Nothing

-- | Set the chart title with a color
setChartTitleWithColor :: String -> Color -> ChartM()
setChartTitleWithColor title color = set $ ChartTitle title (Just color) Nothing

-- | Set the chart title with color and font size
setChartTitleWithColorAndFontSize :: String -> Color -> FontSize -> ChartM ()
setChartTitleWithColorAndFontSize title color fontsize =
    set $ ChartTitle title (Just color) (Just fontsize)

{-| Use this with 'simple', 'text' or 'extended' to specify the encoding. For e.g

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


-- | Add a scale to chart.If more than one scale is added, it applies
-- the scale in order to each data series
addDataScale :: DataScale -> ChartM ()
addDataScale = addScaleToChart

-- | Works like 'addChartData', but for XY datasets for line XY chart etc
addChartDataXY :: ChartDataEncodable a => [(a,a)] -> ChartM ()
addChartDataXY series = do addDataToChart xseries
                           addDataToChart yseries
                        where xseries = map fst series
                              yseries = map snd series

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

-- | Adds a shape marker. Use `makeShapeMarker` smart constructor when calling
-- this function If value of data set index is not specified when using
-- 'makeShapeMarker', it automatically adds a data index to refer to the latest
-- data set
addShapeMarker :: ShapeMarker -> ChartM ()
addShapeMarker marker | shapeDataSetIdx marker > (- 1) = addMarker marker
                      | otherwise = do idx <- getDataSetIdx
                                       let newmarker = marker { shapeDataSetIdx = idx }
                                       addMarker marker

-- | Adds a range marker. You can use 'makeRangeMarker' smart constructor when
-- calling this function
addRangeMarker :: RangeMarker -> ChartM ()
addRangeMarker = addMarker

-- | Adds a financial marker. Use 'makeFinancialMarker' smart constructor when
-- calling this function. If value of data set index is not specified when using
-- 'makeFinancialMarker', it automatically adds a data index to refer to the latest
-- data set
addFinancialMarker :: FinancialMarker -> ChartM ()
addFinancialMarker marker | financeDataSetIdx marker > (- 1) = addMarker marker
                          | otherwise = do idx <- getDataSetIdx
                                           let newmarker = marker { financeDataSetIdx = idx }
                                           addMarker newmarker
-- | Adds a line marker. Use 'makeLineMarker' smart constructor when calling
-- this function. If value of data set index is not specified when using
-- 'makeLineMarker', it automatically adds a data index to refer to the
-- latest data set
addLineMarker :: LineMarker -> ChartM ()
addLineMarker marker | lineDataSetIdx marker > (- 1) = addMarker marker
                     | otherwise = do idx <- getDataSetIdx
                                      let newmarker = marker { lineDataSetIdx = idx }
                                      addMarker newmarker

-- | Adds a line fill to the chart
addLineFill :: LineFillType -> Color -> ChartM ()
addLineFill fillType color = addMarker (LineFillMarker fillType color)

-- | Set labels for the chart
setLabels :: [String] -> ChartM ()
setLabels = set . ChartLabels

-- | Set label for a chart
setLabel :: String -> ChartM ()
setLabel label = set $ ChartLabels [label]

-- | Set bar and width spacing
setBarWidthSpacing :: BarChartWidthSpacing -> ChartM ()
setBarWidthSpacing = set

-- | Set pie chart orientation in radians
setPieChartOrientation :: Float -> ChartM ()
setPieChartOrientation = set . PCO

-- | Add line style
addLineStyle :: LineStyle -> ChartM()
addLineStyle = addLineStyleToChart

-- | Set formula. Applies only to 'Formula' charts
setFormula :: String -> ChartM ()
setFormula formula = setLabels [formula]

-- | Set QR code output encoding. Valid for 'QRCode' only
setQREncoding :: QREncoding -> ChartM ()
setQREncoding = set

-- | Sets the error correction level for 'QRCode'
setQRErrorCorrection :: ErrorCorrectionLevel -> ChartM ()
setQRErrorCorrection ec = set qrLabelData where
                           qrLabelData = QRLabelData ec defMargin
                           QRLabelData _ defMargin = defaultQREncodingLabelData

-- | Sets the width (in rows) of the white border around the data portion of the 'QRCode'
setQRWidth :: Int -> ChartM ()
setQRWidth m = set qrLabelData where
                qrLabelData = QRLabelData defEC m
                QRLabelData defEC _ = defaultQREncodingLabelData

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

