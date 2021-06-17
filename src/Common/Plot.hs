{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Plot where

import Graphics.Rendering.Chart.Backend.Cairo hiding (FileFormat(..))
import Graphics.Rendering.Chart.Easy hiding (points)
import Polysemy
import Polysemy.State
import System.IO (hClose)
import System.IO.Temp
import Data.Aeson hiding ((.=))
import GHC.Generics

data Color = R | B | Y | G | K | M deriving (Generic)

instance Show Color where
  show R = "Red"
  show B = "Blue"
  show Y = "Yellow"
  show G = "Green"
  show K = "Black"
  show M = "Magenta"
  
instance ToJSON Color where toEncoding = genericToEncoding defaultOptions

colorToColour :: (Ord a, Floating a) => Color -> AlphaColour a
colorToColour K = opaque black
colorToColour G = opaque green
colorToColour Y = opaque yellow
colorToColour B = opaque blue
colorToColour R = opaque red
colorToColour M = opaque magenta

data PlotDouble = PlotDouble
  { points :: [(Double, Double)],
    plotName :: String,
    color :: Color
  } deriving (Generic, Show)

instance ToJSON PlotDouble where toEncoding = genericToEncoding defaultOptions

data AxesType = Logarithmic | Linear deriving (Generic, Show)

instance ToJSON AxesType where toEncoding = genericToEncoding defaultOptions

data ChartParams = ChartParamsDouble
  { plotsDouble :: [PlotDouble],
    legend :: String,
    xAxis :: (AxesType, String),
    yAxis :: (AxesType, String)
  } deriving (Generic, Show)

instance ToJSON ChartParams where toEncoding = genericToEncoding defaultOptions

axisTypeToScaling :: (RealFloat a, Show a) => AxesType -> AxisFn a
axisTypeToScaling Logarithmic = autoScaledLogAxis def
axisTypeToScaling Linear = autoScaledAxis def

data FileFormat = PNG | PDF

data ChartLang m a where
  MakeChart :: ChartParams -> FileFormat -> ChartLang m FilePath
  ChartOutAsJSON :: ChartParams -> FilePath -> ChartLang m ()

showFileFormat :: FileFormat -> String
showFileFormat PNG = ".png"
showFileFormat PDF = ".pdf"

makeSem ''ChartLang

defaultChartOptions :: PlotDouble -> ChartParams
defaultChartOptions p = ChartParamsDouble [p] (plotName p) (Linear, "") (Linear, "")

errorPlot :: [Double] -> Color -> PlotDouble
errorPlot d c = PlotDouble 
  (map (\d -> (d, d)) d)
  ("График ошибок в канале")
  c
  

interpretChartLangCairo :: Members '[Embed IO] r => FilePath -> InterpreterFor ChartLang r
interpretChartLangCairo dir = interpret \case
  MakeChart chartParams@ChartParamsDouble {} fileFormat -> do
    (chartPath, h) <- embed $ openTempFile dir (showFileFormat fileFormat)
    embed $ hClose h
    embed $
      toFile (def {_fo_size = (1920, 1080)}) chartPath $ do
        layout_all_font_styles . font_size .= 30
        layout_title .= legend chartParams
        layout_x_axis . laxis_title .= (snd . xAxis $ chartParams)
        layout_x_axis . laxis_generate .= (axisTypeToScaling . fst . xAxis $ chartParams)
        layout_y_axis . laxis_title .= (snd . yAxis $ chartParams)
        layout_y_axis . laxis_generate .= (axisTypeToScaling . fst . yAxis $ chartParams)
        setColors $ map (colorToColour . color) (plotsDouble chartParams)
        mapM_ (\plotParams -> plot (line (plotName plotParams) [points plotParams])) (plotsDouble chartParams)
    pure chartPath
    
  ChartOutAsJSON cp fp -> embed $ encodeFile (dir ++ fp) cp 
