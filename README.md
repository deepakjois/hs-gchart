## Introduction

 **GChart** is a Haskell wrapper around [Google Chart API].

[Google Chart API]: http://code.google.com/apis/chart/image_charts.html

There is a library on Hackage called [GoogleChart] which provides another
wrapper, however it has not been updated in a long while. GChart improves upon
that effort by trying to design a more elegant API, and supporting more chart
types and features.

[GoogleChart]: http://hackage.haskell.org/packages/archive/GoogleChart/0.2/doc/html/Graphics-Google-Chart.html

## Installation

    cabal install hs-gchart

## Documentation

* [API functions reference](http://hackage.haskell.org/package/hs-gchart)

* [Haskell data types reference](http://hackage.haskell.org/packages/archive/hs-gchart/0.4/doc/html/Graphics-GChart-Types.html)

* [Google Chart API]

## Getting Started

These examples below are available in `examples/Examples.hs` in the source
tarball. Look at the source file for more Some examples are taken from [this
article](http://24ways.org/2007/tracking-christmas-cheer-with-google-charts)

For examples 1 and 2, the following code is common

    dataSeries1 :: [Int]
    dataSeries1 = [10,20,8,25,5,3,15,9,5]

    labelSeries1 = ["Egg nog",
                    "Christmas Ham",
                    "Milk (not including egg nog)",
                    "Cookies",
                    "Roasted Chestnuts",
                    "Chocolate",
                    "Various Other Beverages",
                    "Various Other Foods",
                    "Snacks"]

### Example 1 : Pie Chart

The code below

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

Generates the following chart.

![Generated Pie Chart](http://chart.apis.google.com/chart?cht=p&chs=600x300&chd=s:KUIZFDPJF&chtt=Food+and+Drink+Consumed+Christmas+2007&chco=00AF33,4BB74C,EE2C2C,CC3232,33FF33,66FF66,9AFF9A,C1FFC1,CCFFCC&chl=Egg+nog|Christmas+Ham|Milk+%28not+including+egg+nog%29|Cookies|Roasted+Chestnuts|Chocolate|Various+Other+Beverages|Various+Other+Foods|Snacks)

### Example 2 : Bar Chart

The code below

    barGraph = getChartUrl $ do setChartSize 600 300
                                setDataEncoding simple
                                setChartType BarHorizontalGrouped
                                addChartData dataSeries1
                                setChartTitle "Food and Drink Consumed Christmas 2007"
                                addAxis $ makeAxis {  axisType = AxisBottom }
                                addAxis $ makeAxis {  axisType = AxisLeft,
                                                      axisLabels = Just labelSeries1 }
                                addColor "00AF33"

Generates the following chart

![Generated Bar Chart](http://chart.apis.google.com/chart?cht=bhg&chs=600x300&chd=s:KUIZFDPJF&chtt=Food+and+Drink+Consumed+Christmas+2007&chco=00AF33&chxt=x,y&chxl=1:|Egg+nog|Christmas+Ham|Milk+%28not+including+egg+nog%29|Cookies|Roasted+Chestnuts|Chocolate|Various+Other+Beverages|Various+Other+Foods|Snacks)


### Example 3 : Line XY Chart 1

The code below

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
    
    dataSeries2 :: [(Float,Float)]
    dataSeries2 = [(0,0),(100,100)]
    
    blanks x = take x $ repeat ""

Generates the following chart

![Line Graph](http://chart.apis.google.com/chart?cht=lxy&chs=800x300&chd=t:0,100|0,100&chtt=Projected+Christmas+Cheer+for+2007&chxt=y,x&chxl=1:|Dec+1st|||||6th|||||||||||||||||||25th|26th|||||Dec+31st&chxr=0,0.0,100.0,50.0&chg=3.333,10.0,1.0,3.0)

### Example 4 : Line XY Chart 2

The code below

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
     
    dataSeries3 :: [(Float,Float)]
    dataSeries3 = zip [0,16.7,23.3,33.3,60,76.7,83.3,86.7,93.3,96.7,100] [30,45,20,50,15,80,60,70,40,55,80]
     
    dataSeries4 :: [(Float,Float)]
    dataSeries4 = zip [0,10,16.7,26.7,33.3] [50,10,30,55,60]

    blanks x = take x $ repeat ""

Generates the following chart

![Line Graph](http://chart.apis.google.com/chart?cht=lxy&chs=800x300&chd=t:0,16.7,23.3,33.3,60,76.7,83.3,86.7,93.3,96.7,100|30,45,20,50,15,80,60,70,40,55,80|0,10,16.7,26.7,33.3|50,10,30,55,60&chtt=Projected+Christmas+Cheer+for+2007&chco=458B00,CD2626&chdl=2006|2007&chdlp=r&chxt=y,x&chxl=1:|Dec+1st|||||6th|||||||||||||||||||25th|26th|||||Dec+31st&chxr=0,0.0,100.0,50.0&chg=3.333,10.0,1.0,3.0)

### Example 5 : Scatter Plot with Shape Markers

The code below 

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
     
    -- Reverse engineering sample data from webpage
    dataSeries5 :: [(Int,Int)]
    dataSeries5 = zip xseries yseries where
                 xseries = map encSimpleReverse "984sttvuvkQIBLKNCAIipr3z9"
                 yseries = map encSimpleReverse "DEJPgq0uov17_zwopQOD"
     
    encSimpleReverse :: Char -> Int
    encSimpleReverse c | ord c >= ord 'A' && ord c <= ord 'Z' = (ord c - ord 'A')
                       | ord c >= ord 'a' && ord c <= ord 'z' = 26 + (ord c - ord 'a')
                       | ord c >= ord '0' && ord c <= ord '9' = 52 + (ord c - ord '0')
                       | otherwise = -1

Generates the following chart

![Scatter Plot with Shape Markers](http://chart.apis.google.com/chart?cht=s&chs=200x125&chd=s:984sttvuvkQIBLKNCAIi,DEJPgq0uov17_zwopQOD&chxt=x,y&chxl=0:||1|2|3|4|5|1:||50|100&chg=20.0,25.0&chm=s,ff0000,-1,-1,10)
