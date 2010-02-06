import Graphics.GChart

christmasPie = do setChartSize 600 300
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


barGraph = do setChartSize 600 300
              setChartType BarHorizontalGrouped
              addChartData dataSeries1
              setChartTitle "Food and Drink Consumed Christmas 2007"
              addAxis $ makeAxis {  axisType = AxisBottom }
              addAxis $ makeAxis {  axisType = AxisLeft,
                                    axisLabels = Just labelSeries1 }
              addColor "00AF33"

linexyGraph = setChartSize

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
                  