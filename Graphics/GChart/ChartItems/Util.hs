module Graphics.GChart.ChartItems.Util (
  updateChart,asList
) where

import Control.Monad.State

updateChart u = do chart <- get
                   put $ u chart

asList a = [a]