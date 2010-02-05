{-# LANGUAGE TypeSynonymInstances, NoMonomorphismRestriction #-}
module Graphics.GChart ( 
  module Graphics.GChart.Types,
  solid,
  legend,
  legendWithPosition,
  makeAxis,
  makeGrid,
  simple,
  text,
  extended,
  setChartSize,
  setDataEncoding,
  addChartData,
  addColors,
  addColor,
  addFill,
  addLegend,
  addAxis,
  addGrid,
  addLabels,
  getChartData,
  getUrl,
  convertToUrl,
) where

import Graphics.GChart.Types
import Graphics.GChart.ChartItems
import Graphics.GChart.DataEncoding

import Data.List

-- smart constructors

solid = Fill . Solid

legend labels = Legend labels Nothing

legendWithPosition labels position = Legend labels (Just position)

makeAxis = defaultAxis

makeGrid = defaultGrid

simple = Simple []

text = Text []

extended = Extended []

-- helper functions

setChartSize w h = set (Size w h)

setChartType = set

setChartTitle = set

setDataEncoding = set

addChartData = addDataToChart

addColors = set . ChartColors

addColor  = addColorToChart

addFill = addFillToChart

addLegend = set

addAxis = addAxisToChart

addGrid = set

addLabels = set . ChartLabels

-- API Functions

getChartData = getChartDataFromChartM

getUrl =  convertToUrl . getChartData

convertToUrl chart = baseURL ++ intercalate "&" urlparams where
    baseURL = "http://chart.apis.google.com/chart?"
    urlparams = [urlEnc a ++ "=" ++ urlEnc b | (a,b) <- getParams chart]

debugPieChart = getUrl $ do setChartSize 640 400
                            setChartType Pie
                            setChartTitle "Test"
                            addChartData  ([1,2,3,4,5]::[Int])
                            addColor "FF0000"
                            addLegend $ legend ["t1","t2", "t3","t4","t5"]
                            addLabels $ ["Test 1", "Test 2", "Test 3", "Test 4", "Test 5"]

debugBarChart = getUrl $ do setChartSize 640 400
                            setChartType BarVerticalGrouped
                            setDataEncoding text
                            addChartData  ([100,200,300,400,500]::[Float])
                            addChartData  ([3,4,5,6,7]::[Float])
                            addChartData  ([4.0,5.0,6.0,7.0,8]::[Float])
                            addAxis $ makeAxis { axisType = AxisLeft,axisLabels = Just ["0","100"] }
                            addColors ["FF0000","00FF00","0000FF"]
                            addLegend $ legend ["Set 1", "Set 2", "Set 3"]
