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

-- legend
-- FIXME change it to not clash with Prelude.Left and Prelude.Right
data LegendPosition = Bottom | Top | VBottom | VTop | Right | Left deriving Show
data ChartLegend = Legend [String] (Maybe LegendPosition) deriving Show


-- axis
data AxisType = AxisBottom | AxisTop | AxisLeft | AxisRight deriving Show
type AxisLabel = String
type AxisPosition = Float
data AxisRange = Range (Float,Float) (Maybe Float) deriving Show

type FontSize = Int
data AxisStyleAlignment = AxisStyleLeft | AxisStyleCenter | AxisStyleRight deriving Show
data DrawingControl = DrawLines | DrawTicks | DrawLinesTick deriving Show
data AxisStyle = Style { axisColor :: Color,
                         axisFontSize :: Maybe FontSize,
                         axisStyleAlign :: Maybe AxisStyleAlignment,
                         axisDrawingControl :: Maybe DrawingControl,
                         tickMarkColor      :: Maybe Color } deriving Show

data Axis = Axis { axisType :: AxisType,
                   axisLabels :: Maybe [AxisLabel],
                   axisPositions :: Maybe [AxisPosition],
                   axisRange :: Maybe AxisRange,
                   axisStyle :: Maybe AxisStyle } deriving Show

type ChartAxes = [Axis]


-- chart
data Chart = Chart { chartSize   :: ChartSize,
                     chartType   :: ChartType,
                     chartData   :: ChartData,
                     chartTitle  :: Maybe ChartTitle,
                     chartColors :: Maybe ChartColors,
                     chartFills  :: Maybe ChartFills,
                     chartLegend :: Maybe ChartLegend,
                     chartAxes   :: Maybe ChartAxes }  deriving Show


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
                       chartFills = Nothing,
                       chartLegend = Nothing,
                       chartAxes = Nothing }


defaultAxis = Axis { axisType = AxisBottom,
                     axisLabels = Nothing,
                     axisPositions = Nothing,
                     axisRange = Nothing,
                     axisStyle = Nothing }


defaultAxisStyle = Style { axisColor = "0000DD",
                           axisFontSize = Nothing,
                           axisStyleAlign = Nothing,
                           axisDrawingControl = Nothing,
                           tickMarkColor = Nothing }
