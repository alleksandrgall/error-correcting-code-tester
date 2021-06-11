{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Common.Plot where

import Graphics.Rendering.Chart.Backend.Cairo hiding (FileFormat(..))
import Graphics.Rendering.Chart.Easy hiding (points)
import Polysemy
import Polysemy.State
import System.IO (hClose)
import System.IO.Temp

data Color = R | B | Y | G | K

instance Show Color where
  show R = "Red"
  show B = "Blue"
  show Y = "Yellow"
  show G = "Green"
  show K = "Black"

colorToColour :: (Ord a, Floating a) => Color -> AlphaColour a
colorToColour K = opaque black
colorToColour G = opaque green
colorToColour Y = opaque yellow
colorToColour B = opaque blue
colorToColour R = opaque red

data PlotDouble = PlotDouble
  { points :: [(Double, Double)],
    plotName :: String,
    color :: Color
  }

data AxesType = Logarithmic | Linear

data ChartParams = ChartParamsDouble
  { plotsDouble :: [PlotDouble],
    legend :: String,
    xAxis :: (AxesType, String),
    yAxis :: (AxesType, String)
  } 

axisTypeToScaling :: (RealFloat a, Show a) => AxesType -> AxisFn a
axisTypeToScaling Logarithmic = autoScaledLogAxis def
axisTypeToScaling Linear = autoScaledAxis def

data FileFormat = PNG | PDF

data ChartLang m a where
  MakeChart :: ChartParams -> FileFormat -> ChartLang m FilePath

showFileFormat :: FileFormat -> String
showFileFormat PNG = ".png"
showFileFormat PDF = ".pdf"

makeSem ''ChartLang

interpretChartLangCairo :: Members '[Embed IO] r => FilePath -> InterpreterFor ChartLang r
interpretChartLangCairo dir = interpret \case
  MakeChart chartParams@ChartParamsDouble {} fileFormat -> do
    (chartPath, h) <- embed $ openTempFile dir (showFileFormat fileFormat)
    embed $ hClose h
    embed $
      toFile def chartPath $ do
        layout_title .= legend chartParams
        layout_x_axis . laxis_title .= (snd . xAxis $ chartParams)
        layout_x_axis . laxis_generate .= (axisTypeToScaling . fst . xAxis $ chartParams)
        layout_y_axis . laxis_title .= (snd . yAxis $ chartParams)
        layout_y_axis . laxis_generate .= (axisTypeToScaling . fst . yAxis $ chartParams)
        setColors $ map (colorToColour . color) (plotsDouble chartParams)
        mapM_ (\plotParams -> plot (line (plotName plotParams) [points plotParams])) (plotsDouble chartParams)
    pure chartPath
