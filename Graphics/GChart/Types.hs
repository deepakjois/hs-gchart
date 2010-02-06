{-| This module contains the Haskell data model for the Google Chart API.

Details about the parameters can be found on the Google Chart API website :
<http://code.google.com/apis/chart/>

Some chart types are not supported yet :

- Maps <http://code.google.com/apis/chart/types.html#maps>

- QRCodes <http://code.google.com/apis/chart/types.html#qrcodes>

Some parameters are not supported yet :

- Data Scaling <http://code.google.com/apis/chart/formats.html#data_scaling>

- Shape markers <http://code.google.com/apis/chart/styles.html#shape_markers>

- Range markers <http://code.google.com/apis/chart/styles.html#range_markers>

- Financial markers <http://code.google.com/apis/chart/styles.html#financial_markers>

- Line Styles <http://code.google.com/apis/chart/styles.html#line_styles>

- Fill area <http://code.google.com/apis/chart/colors.html#fill_area_marker>

- Bar width and spacing <http://code.google.com/apis/chart/styles.html#bar_width>

- Bar chart zero line  <http://code.google.com/apis/chart/styles.html#zero_line>

- Data point labels <http://code.google.com/apis/chart/labels.html#data_point_labels>

- Chart title color and font customisation <http://code.google.com/apis/chart/labels.html#chart_title>

- Pie chart orientation <http://code.google.com/apis/chart/types.html#pie_charts>

-}

module Graphics.GChart.Types (

  -- * Typeclasses
  -- | Typeclasses for abstraction
  ChartM, ChartItem(set,encode), ChartDataEncodable(addEncodedChartData),

  -- * Chart data
  -- | This type represents the Chart
  Chart(..),

  -- * Required Parameters
  -- | These parameters are required for all charts

  -- ** Chart Size
  ChartSize(..),
  -- ** Chart Data
  ChartData(..),
  -- ** Chart Type
  ChartType(..),

  -- * Optional Parameters
  -- | All other parameters are optional

  -- ** Chart Colors
  ChartColors(..), Color,

  -- ** Solid Fill, Linear Gradient, Linear Stripes
  ChartFills, Fill(..), FillKind(..), FillType(..),
  Offset, Width, Angle,
  -- ** Chart Title
  ChartTitle,

  -- ** Chart Legend
  ChartLegend(..), LegendPosition(..),

  -- ** Axis styles and labels
  ChartAxes, Axis(..), AxisType(..), AxisLabel, AxisPosition, FontSize,
  AxisRange(..), AxisStyle(..), DrawingControl(..), AxisStyleAlignment(..),

  -- ** Grid Lines
  ChartGrid(..),

  -- ** Pie chart and Google-o-meter labels
  ChartLabels(..),

  -- ** Chart Margins
  ChartMargins(..),

  -- * Default Values
  {-| These functions return default values for complex parameters, which can be
       used as starting points to construct parameters when creating charts. -}
  defaultChart, defaultAxis, defaultGrid
) where

import Control.Monad.State

-- | Size of the chart. width and height specified in pixels
data ChartSize = Size Int Int deriving Show


-- | Chart type <http://code.google.com/apis/chart/types.html>
data ChartType
  = Line                  -- ^ Line Chart
  | Sparklines            -- ^ Sparklines
  | LineXY                -- ^ Line Chart w/ XY co-ordinates
  | BarHorizontalStacked  -- ^ Horizontal bar chart w/ stacked bars
  | BarVerticalStacked    -- ^ Vertical bar chart w/ stacked bars
  | BarHorizontalGrouped  -- ^ Horizontal bar chart w/ grouped bars
  | BarVerticalGrouped    -- ^ Vertical bar chart w/ grouped bars
  | Pie                   -- ^ Two dimensional pie chart
  | Pie3D                 -- ^ Three dimensional pie chart
  | PieConcentric         -- ^ Concentric pie chart
  | Venn                  -- ^ Venn Diagram
  | ScatterPlot           -- ^ Scatter Plot
  | Radar                 -- ^ Radar Chart
  | GoogleOMeter          -- ^ Google-o-meter
    deriving Show

-- | Title of the chart
-- | <http://code.google.com/apis/chart/labels.html#chart_title>
type ChartTitle = String


-- | Chart data along with encoding. XY data for is encoded a pair of
-- | consecutive data sets
-- | <http://code.google.com/apis/chart/formats.html>
data ChartData
  = Simple [[Int]]   -- ^ lets you specify integer values from 0-61, inclusive
  | Text [[Float]]   -- ^ supports floating point numbers from 0-100, inclusive
  | Extended [[Int]] -- ^ lets you specify integer values from 0-4095, inclusive
    deriving Show

-- | Color data specified as a hex string
type Color = String

-- | Chart colors specified as a list of 'Color' values for each data point.
-- <http://code.google.com/apis/chart/colors.html#chart_colors>
data ChartColors = ChartColors [Color] deriving Show


-- | Specifies the angle of the gradient between 0 (horizontal) and 90
-- (vertical). Applicable to 'LinearGradient' and 'LinearStripes'
type Angle = Float

-- | Specifies at what point the color is pure. In this parameter, 0 specifies
-- the right-most chart position and 1 specifies the left-most chart
-- position. Applicable to 'LinearGradient'
type Offset = Float

-- | Width of the stripe. must be between 0 and 1, where 1 is the full width of
-- the chart
type Width = Float

-- | Specifies the kind of fill
data FillKind
    = Solid Color -- ^ Solid Fill <http://code.google.com/apis/chart/colors.html#solid_fill>
    | LinearGradient Angle [(Color,Offset)] -- ^ Linear Gradient <http://code.google.com/apis/chart/colors.html#linear_gradient>
    | LinearStripes Angle [(Color,Width)]  -- ^ Linear Stripes <http://code.google.com/apis/chart/colors.html#linear_stripes>
      deriving Show

-- | Specifies the type of fill
data FillType
    = Background   -- ^ Background fill
    | Area         -- ^ Chart area fill
    | Transparent  -- ^ Apply transparency to whole chart (applicable to 'Solid' fill only)
      deriving Show

-- | Constructor for a chart fill
data Fill = Fill FillKind FillType deriving Show

-- | Chart fills, as a list of `Fill's
type ChartFills = [Fill]

-- | Position of legend on chart. Applies to 'ChartLegend'
data LegendPosition
    = LegendBottom   -- ^ Bottom of chart, horizontally
    | LegendTop      -- ^ Top of chart, horizontally
    | LegendVBottom  -- ^ Bottom of chart, vertically
    | LegendVTop     -- ^ Bottom of chart, vertically
    | LegendRight    -- ^ Left of chart
    | LegendLeft     -- ^ Right of chart
      deriving Show

-- | Specifies a chart legend
-- <http://code.google.com/apis/chart/labels.html#chart_legend>
data ChartLegend = Legend [String] (Maybe LegendPosition) deriving Show

-- | Type of 'Axis'
-- <http://code.google.com/apis/chart/labels.html#axis_type>
data AxisType
    = AxisBottom -- ^ Bottom x-axis
    | AxisTop    -- ^ Top x-axis
    | AxisLeft   -- ^ Left y-axis
    | AxisRight  -- ^ Right y-axis
      deriving Show

-- | 'Axis' Labels.
-- <http://code.google.com/apis/chart/labels.html#axis_labels>
type AxisLabel = String

{-| 'Axis' Label Positions. <http://code.google.com/apis/chart/labels.html#axis_label_positions>

Labels with a specified position of 0 are placed at the bottom of the y- or
r-axis, or at the left of the x- or t-axis.

Labels with a specified position of 100 are placed at the top of the y- or
r-axis, or at the right of the x- or t-axis.

-}
type AxisPosition = Float

{-| 'Axis' Range <http://code.google.com/apis/chart/labels.html#axis_range>

The range is specifies with a tuple containing the start and end values. An
optional interval value can be specified.

-}
data AxisRange = Range (Float,Float) (Maybe Float) deriving (Show,Eq)

-- | Font size in pixels. Applicable to 'AxisStyle'
type FontSize = Int

-- | Alignment of 'Axis' labels. Applies to 'AxisStyle'
data AxisStyleAlignment
    = AxisStyleLeft    -- ^ Left aligned labels
    | AxisStyleCenter  -- ^ Centered labels
    | AxisStyleRight   -- ^ Right aligned labels
      deriving (Show,Eq)

-- | Control drawing of 'Axis'. Applicable to 'AxisStyle'
data DrawingControl
    = DrawLines      -- ^ Draw axis lines only
    | DrawTicks      -- ^ Draw tick marks only
    | DrawLinesTicks -- ^ Draw axis lines and tick marks
      deriving (Show,Eq)

-- | Specify 'Axis' style
-- <http://code.google.com/apis/chart/labels.html#axis_styles>
data AxisStyle = Style { axisColor :: Color,
                         axisFontSize :: Maybe FontSize,
                         axisStyleAlign :: Maybe AxisStyleAlignment,
                         axisDrawingControl :: Maybe DrawingControl,
                         tickMarkColor      :: Maybe Color } deriving (Show,Eq)

-- | Specify an axis for chart.
-- <http://code.google.com/apis/chart/labels.html#axis_styles>
data Axis = Axis { axisType :: AxisType,
                   axisLabels :: Maybe [AxisLabel],
                   axisPositions :: Maybe [AxisPosition],
                   axisRange :: Maybe AxisRange,
                   axisStyle :: Maybe AxisStyle } deriving Show

-- | List of 'Axis' for chart
type ChartAxes = [Axis]

-- | Grid Lines for Chart
-- <http://code.google.com/apis/chart/styles.html#grid>
data ChartGrid =
    ChartGrid {
      xAxisStep :: Float -- ^ x-axis step size (0-100)
    , yAxisStep :: Float -- ^ y-axis step size (0-100)
    , lineSegmentLength :: Maybe Float  -- ^ length of line segment
    , blankSegmentLength :: Maybe Float -- ^ length of blank segment
    , xOffset :: Maybe Float -- ^ x axis offset
    , yOffset :: Maybe Float -- ^ y axis offset
    } deriving Show


{-
data ShapeType = ShapeArrow | ShapeCross | ShapeDiamond | ShapeCircle | ShapeSquare | VerticalLine | VerticalLineFull | HorizontalLine | ShapeX deriving Show
data ShapeDataPoint =  DataPoint Int | DataPointEvery | DataPointEveryN Int | DataPointEveryNRange Int Int Int | DataPointXY Float Float deriving Show
data RangeMarkerType = RangeMarkerHorizontal | RangeMarkerVertical deriving Show


data ChartMarker =  ShapeMarker { shapeType  :: ShapeType,
                                  shapeColor :: Color,
                                  shapeDataSetIdx :: Int,
                                  shapeDataPoint  :: ShapeDataPoint,
                                  shapeSize :: Int,
                                  shapePriority :: Int
                                 } |
                    RangeMarker { rangeType  :: RangeMarkerType,
                                  rangeColor :: Color,
                                  rangeSpan :: (Float,Float) } deriving Show

type ChartMarkers = [ChartMarker]
-}

-- | Labels for Pie Chart and Google-o-meter.
-- Specify a list with a single label for Google-o-meter
data ChartLabels = ChartLabels [String] deriving Show

-- | Chart Margins. All margin values specified are the minimum margins around
-- the plot area, in pixels.
-- <http://code.google.com/apis/chart/styles.html#chart_margins>
data ChartMargins =
    ChartMargins {
      leftMargin   :: Int                -- ^ Left margin around plot area
    , rightMargin  :: Int                -- ^ Right margin around plot area
    , topMargin    :: Int                -- ^ Top margin around plot area
    , bottomMargin :: Int                -- ^ Bottom margin around plot area
    , legendMargins  :: Maybe (Int,Int)  -- ^ Minimum width and height  of legend
    } deriving Show

-- | Data type for the chart
data Chart =
    Chart {
      chartSize    :: ChartSize
    , chartType    :: ChartType
    , chartData    :: ChartData
    , chartTitle   :: Maybe ChartTitle
    , chartColors  :: Maybe ChartColors
    , chartFills   :: Maybe ChartFills
    , chartLegend  :: Maybe ChartLegend
    , chartAxes    :: Maybe ChartAxes
    , chartGrid    :: Maybe ChartGrid
    , chartLabels  :: Maybe ChartLabels
    , chartMargins :: Maybe ChartMargins
    } deriving Show


-- | Chart monad which wraps a 'State' monad in turn
-- to keep track of the chart state and make it convenient
-- to update it
type ChartM a = State Chart a

-- | Typeclass abstracting all the fields in a chart
class ChartItem c where
  -- | sets the item in a chart
  set :: c -> ChartM ()

  -- | encode the field into a list string params that can
  -- then be converted into a query string URL
  encode :: c -> [(String,String)]


-- | Typeclass abstracting the numeric data that can be encoded.
-- This helps in passing Int and Float values as chart data, which
-- are then encoded correctly
class Num a => ChartDataEncodable a where
    -- | Adds the array of numeric data to the existing chart data.
    -- Throws a error if the data passed in doesnt match with the 
    -- current data encoding format.
    addEncodedChartData :: [a] -> ChartData -> ChartData


-- | Default value for a chart
defaultChart =
    Chart { chartSize  = Size 320 200,
            chartType  = Line,
            chartData  = Simple [],
            chartTitle = Nothing,
            chartColors = Nothing,
            chartFills = Nothing,
            chartLegend = Nothing,
            chartAxes = Nothing,
            chartGrid = Nothing,
            chartLabels = Nothing,
            chartMargins = Nothing
          }

-- | Default value for an axis
defaultAxis = Axis { axisType = AxisBottom,
                     axisLabels = Nothing,
                     axisPositions = Nothing,
                     axisRange = Nothing,
                     axisStyle = Nothing }

-- | Default value for an axis style
defaultAxisStyle = Style { axisColor = "0000DD",
                           axisFontSize = Nothing,
                           axisStyleAlign = Nothing,
                           axisDrawingControl = Nothing,
                           tickMarkColor = Nothing }

-- | Default value for a chart grid
defaultGrid = ChartGrid {  xAxisStep = 20,
                           yAxisStep = 20,
                           lineSegmentLength = Nothing,
                           blankSegmentLength = Nothing,
                           xOffset = Nothing,
                           yOffset = Nothing }


{-
defaultShapeMarker =  ShapeMarker { shapeType = ShapeCircle,
                                    shapeColor = "0000DD",
                                    shapeDataSetIdx = 0,
                                    shapeDataPoint = DataPointEvery,
                                    shapeSize = 5,
                                    shapePriority = 0 }
-}