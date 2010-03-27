{-# LANGUAGE ExistentialQuantification #-}
{-| This module contains the Haskell data model for the Google Chart API.

More details about the API and parameters can be found at :
<http://code.google.com/apis/chart/image_charts.html>

Some chart types are not supported yet:

- Box Charts <http://code.google.com/apis/chart/docs/gallery/compound_charts.html#box_charts>

- Candlestick Charts <http://code.google.com/apis/chart/docs/gallery/compound_charts.html#candlestick_charts>

- Compound Charts <http://code.google.com/apis/chart/docs/gallery/compound_charts.html>

- Dynamic Icons <http://code.google.com/apis/chart/docs/gallery/dynamic_icons.html>

- Map Charts <http://code.google.com/apis/chart/docs/gallery/map_charts.html>

Some parameters are not supported yet:

- Text and Data Value Markers <http://code.google.com/apis/chart/docs/chart_params.html#gcharts_data_point_labels>

- Shape offset feature for shape markers  <http://code.google.com/apis/chart/docs/chart_params.html#gcharts_shape_markers>

- Bug in 'BarChartWidthSpacing'. Not fully accurate

- Modfy FillType to conform to new API <http://code.google.com/apis/chart/docs/chart_params.html#gcharts_gradient_fills>

- Dynamic icon type <http://code.google.com/apis/chart/docs/gallery/dynamic_icons.html>

- Geographic area <http://code.google.com/apis/chart/docs/gallery/map_charts.html>

- Vertical slice filling <http://code.google.com/apis/chart/docs/chart_params.html#gcharts_line_fills>

- Bar chart zero line <http://code.google.com/apis/chart/docs/gallery/bar_charts.html#chp>

-}

module Graphics.GChart.Types (

  -- * Typeclasses
  -- | Typeclasses for abstraction
  ChartM, ChartItem(set,encode), ChartDataEncodable(addEncodedChartData),

  -- * Chart
  -- | Data type to represent the chart
  Chart(..),

  -- * Chart Parameters
  {-| Some of these parameters behave differently depending on the chart type;

      More details : <http://code.google.com/apis/chart/docs/chart_params.html>
  -}

  -- ** Bar Width and Spacing
  BarChartWidthSpacing(..), BarWidth(..), BarGroupSpacing(..),

  -- ** Series Colors
  ChartColors(..), Color,

  -- ** Chart Data
  ChartData(..), ChartDataScales(..), DataScale(..),

  -- ** Chart Legend Text and Style
  ChartLegend(..), LegendPosition(..),

  -- ** Solid, Gradient and Striped Fills
  ChartFills, Fill(..), FillKind(..), FillType(..),
  Offset, Width, Angle,

  -- ** Grid Lines
  ChartGrid(..),

  -- ** Pie chart labels, Google-o-meter label (TODO: QR code data, Formula TeX data)
  ChartLabels(..),

  -- ** Chart Label Data
  ChartLabelData(..), ErrorCorrectionLevel(..),

  -- ** Line Styles
  ChartLineStyles(..), LineStyle(..),

  -- ** Line, Shape, Range and Financial Markers
  AnyChartMarker(..), ChartMarker(..), ChartMarkers,
  LineWhichPoints(..),LineMarker(..),
  ShapeType(..), MarkerDataPoint(..), ShapeMarker(..),
  RangeMarkerType(..), RangeMarker(..),
  FinancialMarker(..),

  -- ** Line Fills
  LineFillType(..), LineFillMarker(..),

  -- ** Chart Margins
  ChartMargins(..),

  -- ** QR code output encoding
  QREncoding(..),

  -- ** Pie Chart Orientation
  PieChartOrientation(..),

  -- ** Chart Size
  ChartSize(..),

  -- ** Chart Type
  ChartType(..),

  -- ** Chart Title and Style
  ChartTitle(..),

  -- ** Visible Axis Axis styles and labels
  ChartAxes, Axis(..), AxisType(..), AxisLabel, AxisPosition, FontSize,
  AxisRange(..), AxisStyle(..), DrawingControl(..), AxisStyleAlignment(..),

  -- * Default Values
  {-| These functions return default values for complex parameters, which can be
       used as starting points to construct parameters when creating charts. -}

  defaultChart, defaultAxis, defaultGrid, defaultSpacing, defaultShapeMarker,
  defaultRangeMarker, defaultFinancialMarker, defaultLineStyle, defaultLineMarker,
  defaultQREncodingLabelData
) where

import Control.Monad.State

-- | Size of the chart. width and height specified in pixels
data ChartSize = Size Int Int deriving Show


-- | Chart type
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
  | RadarCurvedLines      -- ^ Radar Chart, connects points with curved lines
  | GoogleOMeter          -- ^ Google-o-meter
  | Formula               -- ^ Formula Chart
  | QRCode                -- ^ QR Codes
    deriving Show

-- | Title of the chart
data ChartTitle =
    ChartTitle {
      titleStr ::String               -- ^ Title
    , titleColor :: Maybe Color       -- ^ Title Color
    , titleFontSize :: Maybe FontSize -- ^ Title Font Size
    } deriving Show

-- | Data scaling expressed as (@series_min@,@series_max@). Applies to text encoding only
type DataScale = (Float,Float)

-- | List of Data scaling values
data ChartDataScales = CDS [DataScale] deriving Show

-- | Chart data along with encoding. XY data for is encoded a pair of
-- consecutive data sets
data ChartData
  = Simple [[Int]]   -- ^ lets you specify integer values from 0-61, inclusive
  | Text [[Float]]   -- ^ supports floating point numbers from 0-100, inclusive
  | Extended [[Int]] -- ^ lets you specify integer values from 0-4095, inclusive
    deriving Show

-- | Color data specified as a hex string
type Color = String

-- | Chart colors specified as a list of 'Color' values for each data point.
data ChartColors = ChartColors [Color] deriving Show


-- | Angle of the gradient between 0 (horizontal) and 90
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
    = Solid Color -- ^ Solid Fill
    | LinearGradient Angle [(Color,Offset)] -- ^ Linear Gradient
    | LinearStripes Angle [(Color,Width)]  -- ^ Linear Stripes
      deriving Show

-- | Specifies the type of fill
data FillType
    = Background   -- ^ Background fill
    | Area         -- ^ Chart area fill
    | Transparent  -- ^ Apply transparency to whole chart (applicable to 'Solid' fill only)
      deriving Show

-- | Constructor for a chart fill
data Fill = Fill FillKind FillType deriving Show

-- | Chart fills, as a list of 'Fill's
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

-- | Chart legend
data ChartLegend = Legend [String] (Maybe LegendPosition) deriving Show

-- | Type of 'Axis'
data AxisType
    = AxisBottom -- ^ Bottom x-axis
    | AxisTop    -- ^ Top x-axis
    | AxisLeft   -- ^ Left y-axis
    | AxisRight  -- ^ Right y-axis
      deriving Show

-- | 'Axis' Labels.
type AxisLabel = String

{-| 'Axis' Label Positions.

Labels with a specified position of 0 are placed at the bottom of the y- or
r-axis, or at the left of the x- or t-axis.

Labels with a specified position of 100 are placed at the top of the y- or
r-axis, or at the right of the x- or t-axis.

-}
type AxisPosition = Float

{-| 'Axis' Range

The range is specifies with a tuple containing the start and end values. An
optional interval value can be specified.

-}
data AxisRange = Range (Float,Float) (Maybe Float) deriving (Show,Eq)

-- | Font size in pixels. Applicable to 'AxisStyle' and 'ChartTitle'
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

-- | 'Axis' style
data AxisStyle = Style { axisColor :: Color,
                         axisFontSize :: Maybe FontSize,
                         axisStyleAlign :: Maybe AxisStyleAlignment,
                         axisDrawingControl :: Maybe DrawingControl,
                         tickMarkColor      :: Maybe Color } deriving (Show,Eq)

-- | Visible axis
data Axis = Axis { axisType :: AxisType,
                   axisLabels :: Maybe [AxisLabel],
                   axisPositions :: Maybe [AxisPosition],
                   axisRange :: Maybe AxisRange,
                   axisStyle :: Maybe AxisStyle } deriving Show

-- | List of 'Axis' for chart
type ChartAxes = [Axis]

-- | Grid Lines for Chart
data ChartGrid =
    ChartGrid {
      xAxisStep :: Float -- ^ x-axis step size (0-100)
    , yAxisStep :: Float -- ^ y-axis step size (0-100)
    , lineSegmentLength :: Maybe Float  -- ^ length of line segment
    , blankSegmentLength :: Maybe Float -- ^ length of blank segment
    , xOffset :: Maybe Float -- ^ x axis offset
    , yOffset :: Maybe Float -- ^ y axis offset
    } deriving Show

-- | Shape type of 'ShapeMarker'
data ShapeType = ShapeArrow            -- ^ Arrow
               | ShapeCross            -- ^ Cross
               | ShapeRectangle        -- ^ Rectangle
               | ShapeDiamond          -- ^ Diamond
               | ShapeErrorBarMarker   -- ^ Error Bar Marker
               | HorizontalLine        -- ^ Horizontal line across the chart at specified height
               | HorizontalLineFull    -- ^ Horizontal line through the specified data marker
               | ShapeCircle           -- ^ Circle
               | ShapeSquare           -- ^ Square
               | VerticalLine          -- ^ Vertical line from x-axis to data point
               | VerticalLineFull      -- ^ Vertical line across the chart
               | ShapeX                -- ^ X shape
                 deriving Show


-- | Data point value of `ShapeMarker`
data MarkerDataPoint =
    DataPoint Float       -- ^ A specific data point in the dataset. Use a
                          -- decimal value to interpolate between two points

  | DataPointEvery        -- ^ Draw a marker on each data point

  | DataPointEveryN Int   -- ^ Draw a marker on every n-th data point

  | DataPointEveryNRange (Int,Int) Int -- ^ @(x,y), n@ draw a marker on every n-th
                                       -- data point in a range, where x is the
                                       -- first data point in the range, and y is
                                       -- the last data point in the range

  | DataPointXY (Float,Float) -- ^ draw a marker at a specific point
                              -- (x,y). Specify the coordinates as floating
                              -- point values, where 0:0 is the bottom left
                              -- corner of the chart, 0.5:0.5 is the center of
                              -- the chart, and 1:1 is the top right corner of
                              -- the chart
    deriving Show

-- | Shape Marker
data ShapeMarker =
    SM { shapeType  :: ShapeType             -- ^ Shape type
       , shapeColor :: Color                 -- ^ Shape Marker color
       , shapeDataSetIdx :: Int              -- ^ Data Set Index
       , shapeDataPoints  :: MarkerDataPoint -- ^ Data point value
       , shapeSize :: Int                    -- ^ Size in pixels
       , shapeWidth :: Maybe Int             -- ^ Optional width used for certain shapes
       , shapeZorder :: Float                -- ^ The layer on which to draw the
                                             -- marker. This is a floating point
                                             -- number from -1.0 to 1.0,
                                             -- inclusive, where -1.0 is the
                                             -- bottom and 1.0 is the top

       -- TODO Add shape offset feature
       } deriving Show

-- | 'RangeMarker' type
data RangeMarkerType = RangeMarkerHorizontal -- ^ horizontal range
                     | RangeMarkerVertical   -- ^ vertical range
                       deriving Show

-- | Range Marker
data RangeMarker =
  RM { rangeMarkerType  :: RangeMarkerType -- ^ Range marker type
     , rangeMarkerColor :: Color           -- ^ Range marker color
     , rangeMarkerRange :: (Float, Float) -- ^ @(start,end) range. @For
                                          -- horizontal range markers, the
                                          -- (start,end) value is a position on
                                          -- the y-axis, where 0.00 is the
                                          -- bottom of the chart, and 1.00 is
                                          -- the top of the chart. For vertical
                                          -- range markers, the (start,end)
                                          -- value is a position on the x-axis,
                                          -- where 0.00 is the left of the
                                          -- chart, and 1.00 is the right of the
                                          -- chart.
    } deriving Show


-- | Financial Marker, for line charts and vertical bar charts
data FinancialMarker =
    FM { financeColor :: Color                -- ^ Finance Marker color
       , financeDataSetIdx :: Int             -- ^ Data Set Index
       , financeDataPoint  :: MarkerDataPoint -- ^ Data point value
       , financeSize :: Int                   -- ^ Size in pixels
       , financePriority :: Int               -- ^ Priority of drawing. Can be one of -1,0,1
       } deriving Show

-- | Which points in a series to use to draw the line.
data LineWhichPoints = PointsAll            -- ^ Use all the points in the series.
                     | Points (Maybe Float, Maybe Float) -- ^ (start,end) indicating a specific range of points
                       deriving Show

-- | Line Marker
data LineMarker =
    LM { lineColor :: Color                    -- ^ Line Marker Color
       , lineDataSetIdx :: Int                 -- ^ Data set index
       , lineWhichPoints :: LineWhichPoints    -- ^ Which points to draw the line markers on
       , lineSize :: Int                       -- ^ Width of line in pixels
       , lineZorder :: Float                   -- ^ Floating point between -1 and 1 indicating
                                               -- the layer on which to draw.
       } deriving Show

-- | Line fill type for 'LineFill'
data LineFillType = LineFillFrom Int   -- ^ Line fill starting from a start index
                  | LineFillBetween Int Int -- ^ Line fill between a start index and end index
                    deriving Show

-- | Line Fill Marker
data LineFillMarker = LineFillMarker LineFillType Color deriving Show


-- | Typeclass to abstract over different chart markers
class Show a => ChartMarker a where
    encodeChartMarker :: a -> String
    encodeChartMarker a = ""

-- | Data type to abstract over all kinds of ChartMarker
data AnyChartMarker = forall w. ChartMarker w => AnyChartMarker w

instance ChartMarker AnyChartMarker where
    encodeChartMarker (AnyChartMarker m) = encodeChartMarker m

instance Show AnyChartMarker where
    show (AnyChartMarker m) = show m

type ChartMarkers = [AnyChartMarker]

-- | Labels for Pie Chart and Google-o-meter.
-- Specify a list with a single label for Google-o-meter
data ChartLabels = ChartLabels [String] deriving Show

-- | Pie Chart Orientation. Applicable only to Pie Charts,
data PieChartOrientation = PCO Float deriving Show

-- | Chart Margins. All margin values specified are the minimum margins around
-- the plot area, in pixels.
data ChartMargins =
    ChartMargins {
      leftMargin   :: Int                -- ^ Left margin around plot area
    , rightMargin  :: Int                -- ^ Right margin around plot area
    , topMargin    :: Int                -- ^ Top margin around plot area
    , bottomMargin :: Int                -- ^ Bottom margin around plot area
    , legendMargins  :: Maybe (Int,Int)  -- ^ Minimum width and height  of legend
    } deriving Show


-- | Bar Width
data BarWidth = Automatic    -- ^ Automatic resizing
              | BarWidth Int -- ^ Bar width in pixels
              deriving Show

-- | Bar and Group Spacing
data BarGroupSpacing = Fixed (Int, Int)          -- ^ Fixed spacing values in pixels
                     | Relative (Float,Float)    -- ^ Relative values as percentages
                     deriving Show


-- | Bar Width and Spacing.
type BarChartWidthSpacing =  (Maybe BarWidth, Maybe BarGroupSpacing)

-- | Line Style. Applicable for line charts
data LineStyle =  LS { lineStyleThickness :: Float    -- ^ Thickness
                     , lineStyleLineSegment :: Float  -- ^ Length of Line Segment
                     , lineStyleBlankSegment :: Float -- ^ Length of Blank Segment
                      } deriving Show

type ChartLineStyles = [LineStyle]

-- | QR Code Output Encoding
data QREncoding = UTF8 | Shift_JIS | ISO8859_1 deriving Show

-- | Error Correction Level for QR Code
data ErrorCorrectionLevel = L' -- ^ recovery of up to 7% data loss
                          | M' -- ^ recovery of up to 15% data loss
                          | Q' -- ^ recovery of up to 25% data loss
                          | H' -- ^ recovery of up to 30% data loss

instance Show ErrorCorrectionLevel where
    show L' = "L"
    show M' = "M"
    show Q' = "Q"
    show H' = "H"

-- | Chart Label Data. Applies to 'QRCode'
data ChartLabelData = QRLabelData ErrorCorrectionLevel Int -- ^ Error Correction Level and Margin (as no. of rows)
                      deriving Show

-- | Data type for the chart
data Chart =
    Chart {
      chartSize    :: Maybe ChartSize
    , chartType    :: ChartType
    , chartData    :: Maybe ChartData
    , chartDataScales :: Maybe ChartDataScales
    , chartTitle   :: Maybe ChartTitle
    , chartColors  :: Maybe ChartColors
    , chartFills   :: Maybe ChartFills
    , chartLegend  :: Maybe ChartLegend
    , chartAxes    :: Maybe ChartAxes
    , chartMarkers :: Maybe ChartMarkers
    , chartGrid    :: Maybe ChartGrid
    , chartLabels  :: Maybe ChartLabels
    , chartMargins :: Maybe ChartMargins
    , barChartWidthSpacing :: Maybe BarChartWidthSpacing
    , pieChartOrientation  :: Maybe PieChartOrientation
    , chartLineStyles      :: Maybe ChartLineStyles
    , qrEncoding           :: Maybe QREncoding
    , chartLabelData       :: Maybe ChartLabelData
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
    Chart { chartSize  = Nothing,
            chartType  = Line,
            chartData  = Nothing,
            chartDataScales = Nothing,
            chartTitle = Nothing,
            chartColors = Nothing,
            chartFills = Nothing,
            chartLegend = Nothing,
            chartAxes = Nothing,
            chartGrid = Nothing,
            chartLabels = Nothing,
            chartMargins = Nothing,
            chartMarkers = Nothing,
            barChartWidthSpacing = Nothing,
            pieChartOrientation = Nothing,
            chartLineStyles = Nothing,
            qrEncoding = Nothing,
            chartLabelData = Nothing
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

-- | Default value for bar and group spacing in bar chart
defaultSpacing = Fixed (4,8)

-- | Default value of a shape marker. Make sure you change the value of @shapeDataSetIdx@
defaultShapeMarker =  SM { shapeType = ShapeCircle,
                           shapeColor = "0000DD",
                           shapeDataSetIdx = -1,
                           shapeDataPoints = DataPointEvery,
                           shapeSize = 5,
                           shapeWidth = Nothing,
                           shapeZorder = 0 }

-- | Default value of range marker
defaultRangeMarker = RM { rangeMarkerType  = RangeMarkerHorizontal,
                          rangeMarkerColor = "0000DD",
                          rangeMarkerRange = (0.0,1.0) }

-- | Default value of a financial marker. Make sure you change the value of @financeDataSetIdx@
defaultFinancialMarker = FM { financeColor = "0000DD",
                              financeDataSetIdx = -1,
                              financeDataPoint = DataPointEvery,
                              financeSize = 5,
                              financePriority = 0 }

-- | Default value of a line marker. Make sure you change the value of @lineDataSetIdx@
defaultLineMarker = LM { lineColor = "0000DD"
                       , lineDataSetIdx = -1
                       , lineWhichPoints = PointsAll
                       , lineSize = 5
                       , lineZorder = 0.0 }

-- | Default value of a line style
defaultLineStyle = LS { lineStyleThickness = 1,
                        lineStyleLineSegment = 1,
                        lineStyleBlankSegment = 0 }

-- | Default chart label data for QR Encoding
defaultQREncodingLabelData = QRLabelData L' 4

