{-# LANGUAGE  TypeSynonymInstances #-}
module Graphics.GChart.ChartItems.Util (
  updateChart, asList, showFloat
) where

import Control.Monad.State

updateChart u = do chart <- get
                   put $ u chart

asList a = [a]

showFloat :: Float -> String
showFloat i | makeFloat (truncate i) - i == 0  = show $ truncate i
            | otherwise                        = show (fromIntegral (round (i * 10.0)) / 10.0)
    where makeFloat i = fromIntegral i :: Float