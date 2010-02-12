{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics.GChart.ChartItems (
  getChartDataFromChartM,
  addDataToChart,
  addColorToChart,
  addFillToChart,
  addAxisToChart,
  addMarker,
  getDataSetIdx,
  addLineStyleToChart,
  getParams
) where

import Graphics.GChart.Types
import Graphics.GChart.DataEncoding

import Graphics.GChart.ChartItems.Basics
import Graphics.GChart.ChartItems.Data
import Graphics.GChart.ChartItems.Colors
import Graphics.GChart.ChartItems.Styles
import Graphics.GChart.ChartItems.Labels

import Control.Monad.State
import Data.List
import Data.Maybe


getChartDataFromChartM m = execState m defaultChart

addDataToChart d = do c <- get
                      let old = chartData c
                      set $ addEncodedChartData d old

addColorToChart color = do chart <- get
                           let (ChartColors old) = fromMaybe (ChartColors []) $ chartColors chart
                               new = ChartColors $ old ++ [color]
                           set new

addFillToChart fill = do chart <- get
                         let fills = fromMaybe [] $ chartFills chart
                             newFills = fills ++ [fill]
                         set newFills




addAxisToChart axis = do chart <- get
                         let old = fromMaybe [] $ chartAxes chart
                             new = old ++ [axis]
                         set new

getDataSetIdx = do chart <- get
                   return $ dataSetLength chart

dataSetLength chart = case chartData chart of
                        Simple   d -> length d - 1
                        Text     d -> length d - 1
                        Extended d -> length d - 1

addMarker :: ChartMarker m => m -> ChartM ()
addMarker marker = do chart <- get
                      let old = fromMaybe [] $ chartMarkers chart
                          new = old ++ [AnyChartMarker marker]
                      set new

addLineStyleToChart :: LineStyle -> ChartM ()
addLineStyleToChart style = do chart <- get
                               let old = fromMaybe [] $ chartLineStyles chart
                                   new = old ++ [style]
                               set new

-- URL Conversion
encodeMaybe Nothing = [("","")]
encodeMaybe (Just x)  = encode x

-- FIXME : too much boilerplate. Can it be reduced?
getParams chart =  filter (/= ("","")) $ concat [encode $ chartType chart,
                                                 encodeMaybe $ chartSize chart,
                                                 encode $ chartData chart,
                                                 encodeMaybe $ chartTitle   chart,
                                                 encodeMaybe $ chartColors  chart,
                                                 encodeMaybe $ chartFills   chart,
                                                 encodeMaybe $ chartLegend  chart,
                                                 encodeMaybe $ chartAxes    chart,
                                                 encodeMaybe $ chartGrid    chart,
                                                 encodeMaybe $ chartMarkers chart,
                                                 encodeMaybe $ chartLabels  chart,
                                                 encodeMaybe $ chartMargins chart,
                                                 encodeMaybe $ barChartWidthSpacing chart,
                                                 encodeMaybe $ pieChartOrientation chart,
                                                 encodeMaybe $ chartLineStyles chart]

