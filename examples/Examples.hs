import Graphics.GChart

{-

Some examples to demonstrate usage of GChart.

All examples are taken from this article : http://24ways.org/2007/tracking-christmas-cheer-with-google-charts

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
                                       setBarWidthSpacing $ automatic


bargraphRelativeSpacing = getChartUrl $ do setChartSize 190 125
                                           setChartType BarVerticalGrouped
                                           setDataEncoding simple
                                           addChartData ([10,15,20,25,30]::[Int])
                                           addChartData ([13,5,6,34,12]::[Int])
                                           setColors ["4d89f9","000000"]
                                           setBarWidthSpacing $ relative 0.5 1.5

blanks x = take x $ repeat ""

dataSeries1 :: [Int]
dataSeries1 = [10,20,8,25,5,3,15,9,5]

dataSeries2 :: [(Float,Float)]
dataSeries2 = [(0,0),(100,100)]

dataSeries3 :: [(Float,Float)]
dataSeries3 = zip [0,16.7,23.3,33.3,60,76.7,83.3,86.7,93.3,96.7,100] [30,45,20,50,15,80,60,70,40,55,80]

dataSeries4 :: [(Float,Float)]
dataSeries4 = zip [0,10,16.7,26.7,33.3] [50,10,30,55,60]

labelSeries1 = ["Egg nog",
                "Christmas Ham",
                "Milk (not including egg nog)",
                "Cookies",
                "Roasted Chestnuts",
                "Chocolate",
                "Various Other Beverages",
                "Various Other Foods",
                "Snacks"]

main = do putStrLn christmasPie
          putStrLn barGraph
          putStrLn linexyGraph1
          putStrLn linexyGraph2
          putStrLn bargraph2
          putStrLn bargraphAutoSpacing
          putStrLn bargraphRelativeSpacing
