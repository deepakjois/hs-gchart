module Types where

import Control.Monad.State

-- size
data ChartSize = Size Int Int deriving Show

-- type
data ChartType = Line |
                 Sparklines |
                 LineXY deriving Show

--title
type ChartTitle = String

-- data
data ChartData = D [[Int]] deriving Show

-- colors
type Color = String

type ChartColors = [Color]


-- fills
type Angle = Float
type Offset = Float
type Width = Float

data FillKind = Solid Color |
                LinearGradient Angle [(Color,Offset)]  |
                LinearStripes Angle [(Color,Width)]  deriving Show

data FillType = Background | Area | Transparent deriving Show

data Fill = Fill FillKind FillType deriving Show

type ChartFills = [Fill]


-- chart
data Chart = Chart { chartSize   :: ChartSize,
                     chartType   :: ChartType,
                     chartData   :: ChartData,
                     chartTitle  :: Maybe ChartTitle,
                     chartColors :: Maybe ChartColors,
                     chartFills  :: Maybe ChartFills }  deriving Show

-- Monad
type ChartM a = State Chart a

-- Typeclass abstracting all the fields in a chart
class ChartItem c where
  -- set the field
  set :: c -> ChartM ()
  -- encode the field into string params
  encode :: c -> [(String,String)]

-- Default value of a chart
defaultChart = Chart { chartSize  = Size 320 200,
                       chartType  = Line,
                       chartData  = D [],
                       chartTitle = Nothing,
                       chartColors = Nothing,
                       chartFills = Nothing }

