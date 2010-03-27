import Graphics.GChart
import Data.Char(ord)
{-

Examples to demonstrate usage of GChart.

Some examples are taken from this article : http://24ways.org/2007/tracking-christmas-cheer-with-google-charts

-}

christmasPie = getChartUrl $ do setChartSize 600 300
                                setDataEncoding simple
                                setChartType Pie
                                addChartData dataSeries1
                                setChartTitle "Food and Drink Consumed Christmas 2007"
                                setLabels labelSeries1
                                setColors ["00AF33",
                                           "4BB74C",
                                           "EE2C2C",
                                           "CC3232",
                                           "33FF33",
                                           "66FF66",
                                           "9AFF9A",
                                           "C1FFC1",
                                           "CCFFCC" ]


barGraph = getChartUrl $ do setChartSize 600 300
                            setDataEncoding simple
                            setChartType BarHorizontalGrouped
                            addChartData dataSeries1
                            setChartTitle "Food and Drink Consumed Christmas 2007"
                            addAxis $ makeAxis {  axisType = AxisBottom }
                            addAxis $ makeAxis {  axisType = AxisLeft,
                                                  axisLabels = Just labelSeries1 }
                            addColor "00AF33"

linexyGraph1 =
    getChartUrl $ do setChartSize 800 300
                     setChartType LineXY
                     setDataEncoding text
                     setChartTitle "Projected Christmas Cheer for 2007"
                     setGrid $ makeGrid { xAxisStep = 3.333,
                                          yAxisStep = 10,
                                          lineSegmentLength = Just 1,
                                          blankSegmentLength = Just 3 }
                     addAxis $ makeAxis { axisType = AxisLeft,
                                          axisRange = Just $ Range (0,100) (Just 50) }
                     addAxis $ makeAxis { axisType = AxisBottom,
                                          axisLabels = Just $ ["Dec 1st"] ++ blanks 4 ++ ["6th"] ++ blanks 18 ++ ["25th","26th"] ++ blanks 4 ++ ["Dec 31st"] }
                     addChartDataXY dataSeries2

linexyGraph2 =
    getChartUrl $ do setChartSize 800 300
                     setChartType LineXY
                     setDataEncoding text
                     setChartTitle "Projected Christmas Cheer for 2007"

                     setGrid $ makeGrid { xAxisStep = 3.333,
                                          yAxisStep = 10,
                                          lineSegmentLength = Just 1,
                                          blankSegmentLength = Just 3 }

                     addAxis $ makeAxis { axisType = AxisLeft,
                                          axisRange = Just $ Range (0,100) (Just 50) }
                     addAxis $ makeAxis { axisType = AxisBottom,
                                          axisLabels = Just $ ["Dec 1st"] ++ blanks 4 ++ ["6th"] ++ blanks 18 ++ ["25th","26th"] ++ blanks 4 ++ ["Dec 31st"] }

                     addChartDataXY dataSeries3
                     addColor "458B00"

                     addChartDataXY dataSeries4
                     addColor "CD2626"

                     setLegend $ legendWithPosition ["2006","2007"] LegendRight

bargraph2 = getChartUrl $ do setChartSize 600 300
                             setChartType BarHorizontalGrouped
                             setDataEncoding simple
                             addChartData dataSeries1
                             setChartTitleWithColor "Food and Drink Consumed Christmas 2007" "00AF33"
                             addAxis $ makeAxis {  axisType = AxisBottom }
                             addAxis $ makeAxis {  axisType = AxisLeft,
                                                   axisLabels = Just labelSeries1 }
                             addColor "00AF33"


bargraphAutoSpacing = getChartUrl $ do setChartSize 190 125
                                       setChartType BarVerticalGrouped
                                       setDataEncoding simple
                                       addChartData ([10,15,20,25,30]::[Int])
                                       addChartData ([13,5,6,34,12]::[Int])
                                       setColors ["4d89f9","000000"]
                                       setBarWidthSpacing automatic


bargraphRelativeSpacing = getChartUrl $ do setChartSize 190 125
                                           setChartType BarVerticalGrouped
                                           setDataEncoding simple
                                           addChartData ([10,15,20,25,30]::[Int])
                                           addChartData ([13,5,6,34,12]::[Int])
                                           setColors ["4d89f9","000000"]
                                           setBarWidthSpacing $ relative 0.5 1.5


scatterPlotWithMarkers = getChartUrl $ do setChartSize 200 125
                                          setChartType ScatterPlot
                                          setDataEncoding simple
                                          addChartDataXY dataSeries5
                                          addAxis $ makeAxis { axisType = AxisBottom,
                                                               axisLabels = Just $ blanks 1 ++ ["1","2","3","4","5"] }
                                          addAxis $ makeAxis { axisType = AxisLeft,
                                                               axisLabels = Just $ blanks 1 ++ ["50","100"] }
                                          setGrid $ makeGrid { xAxisStep = 20, yAxisStep = 25 }

                                          addShapeMarker $ makeShapeMarker { shapeType  = ShapeSquare
                                                                           , shapeColor = "ff0000"
                                                                           , shapeSize  = 10 }

lineChartWithLineStyles = getChartUrl $ do setChartSize 200 125
                                           setChartType Line
                                           setDataEncoding simple

                                           addChartData $ map encSimpleReverse "93zyvneTTOMJMLIJFHEAECFJGHDBFCFIERcgnpy45879"
                                           addLineStyle $ makeLineStyle { lineStyleThickness = 3, lineStyleLineSegment = 6, lineStyleBlankSegment = 3 }

                                           addChartData $ map encSimpleReverse "IJKNUWUWYdnswz047977315533zy1246872tnkgcaZQONHCECAAAAEII"
                                           addLineStyle $ makeLineStyle { lineStyleThickness = 1, lineStyleLineSegment = 1, lineStyleBlankSegment = 0 }

formulaChart = getChartUrl $ do setChartType Formula
                                setChartHeight 200
                                setFormula "\\Large\\mathbb{Q}+\\lim_{x\\to3}\\frac{1}{x}"
                                addColor "FF0000"
                                addFill $ Fill (LinearGradient 20 [("76A4FB",1),("FFFFFF",0)]) Background

qrCodeChart = getChartUrl $ do setChartType QRCode
                               setChartSize 150 150
                               setLabel "Hello World"
                               setQREncoding UTF8
                               setQRErrorCorrection L'

barGraphWithShiftedZeroLine = getChartUrl $ do setChartType BarVerticalGrouped
                                               setChartSize 320 200
                                               setDataEncoding text
                                               addChartData ([30,-60,50,140,80,-90]::[Float])
                                               addDataScale (-80,140)
                                               addAxis $ makeAxis { axisType = AxisLeft,
                                                                    axisRange =  Just $ Range (-80,140) Nothing}

lineMarkerSample = getChartUrl $ do setChartType BarVerticalGrouped
                                    setChartSize 200 150
                                    setDataEncoding simple
                                    setBarWidthSpacing $ barwidth 20
                                    addChartData $ map encSimpleReverse "1XQbnf4"
                                    addColor "76A4FB"
                                    addLineMarker $ makeLineMarker { lineColor = "0033FF",
                                                                     lineWhichPoints = PointsAll,
                                                                     lineZorder = 1 }

lineFillSample = getChartUrl $ do setChartType Line
                                  setChartSize 200 125
                                  setDataEncoding simple
                                  addChartData $ map encSimpleReverse "cefhjkqwrlgYcfgc"
                                  addLineFill (LineFillBetween 0 1) "224499"
                                  addChartData $ map encSimpleReverse "QSSVXXdkfZUMRTUQ"
                                  addLineFill (LineFillBetween 1 2) "FF0000"
                                  addChartData $ map encSimpleReverse "HJJMOOUbVPKDHKLH"
                                  addLineFill (LineFillBetween 2 3) "80C65A"
                                  addAxis $  makeAxis { axisType = AxisBottom,
                                                       axisLabels = Just  ["Sep","Oct","Nov","Dec"] }
                                  addAxis $ makeAxis { axisType = AxisLeft,
                                                       axisLabels = Just  ["","50","100"] }

blanks x = replicate x ""

dataSeries1 :: [Int]
dataSeries1 = [10,20,8,25,5,3,15,9,5]

dataSeries2 :: [(Float,Float)]
dataSeries2 = [(0,0),(100,100)]

dataSeries3 :: [(Float,Float)]
dataSeries3 = zip [0,16.7,23.3,33.3,60,76.7,83.3,86.7,93.3,96.7,100] [30,45,20,50,15,80,60,70,40,55,80]

dataSeries4 :: [(Float,Float)]
dataSeries4 = zip [0,10,16.7,26.7,33.3] [50,10,30,55,60]

dataSeries5 :: [(Int,Int)]
dataSeries5 = zip xseries yseries where
             xseries = map encSimpleReverse "984sttvuvkQIBLKNCAIipr3z9"
             yseries = map encSimpleReverse "DEJPgq0uov17_zwopQOD"

labelSeries1 = ["Egg nog",
                "Christmas Ham",
                "Milk (not including egg nog)",
                "Cookies",
                "Roasted Chestnuts",
                "Chocolate",
                "Various Other Beverages",
                "Various Other Foods",
                "Snacks"]

encSimpleReverse :: Char -> Int
encSimpleReverse c | ord c >= ord 'A' && ord c <= ord 'Z' = ord c - ord 'A'
                   | ord c >= ord 'a' && ord c <= ord 'z' = 26 + (ord c - ord 'a')
                   | ord c >= ord '0' && ord c <= ord '9' = 52 + (ord c - ord '0')
                   | otherwise = -1

main = do putStrLn christmasPie
          putStrLn barGraph
          putStrLn linexyGraph1
          putStrLn linexyGraph2
          putStrLn bargraph2
          putStrLn bargraphAutoSpacing
          putStrLn bargraphRelativeSpacing
          putStrLn scatterPlotWithMarkers
          putStrLn lineChartWithLineStyles
          putStrLn formulaChart
          putStrLn qrCodeChart

