## Introduction

 **GChart** is a Haskell wrapper around [Google Chart API].

[Google Chart API]: http://code.google.com/apis/chart/

There is a library on Hackage called [GoogleChart] which provides another
wrapper, however it has not been updated in a long while. GChart improves upon
that effort by trying to design a more elegant API, and supporting more chart
types and features.

[GoogleChart]: http://hackage.haskell.org/packages/archive/GoogleChart/0.2/doc/html/Graphics-Google-Chart.html

## Installation

Coming Soon

## Getting Started

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

Coming Soon!

### Example 4 : Line XY Chart 2

Coming Soon!

## Documentation

* API functions reference

* Haskell data types reference, along with pending features and chart types

* [Google Chart API]