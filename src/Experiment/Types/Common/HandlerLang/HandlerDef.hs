{-# LANGUAGE DeriveFunctor #-}

module Experiment.Types.Common.HandlerLang.HandlerDef
    (formatStringVars,
     getFormatString,
     handlerInfo,
     CascadeHandlers,
     HandlerInfo (..),
     HandlerType(..)    
     )
    where 

import Data.Text (Text)
import Experiment.Types.Common.FormatString
import Control.Monad.Free 

class HandWithFormatString a where 
    formatStringVars :: a -> [Text]

data HandlerType = NK | Poly | Matr | Interleaver | Noise

data HandlerInfo a =  HandlerInfo HandlerType FormatString a
    deriving (Functor)

getFormatString :: HandlerInfo a -> FormatString
getFormatString (HandlerInfo _ fs _) = fs

instance HandWithFormatString (HandlerInfo a) where 
    formatStringVars (HandlerInfo NK _ _) =             [_codeK defFormatNames, 
                                                        _codeN defFormatNames, 
                                                        _outputPath defFormatNames,
                                                        _inputPath defFormatNames]

    formatStringVars (HandlerInfo Poly _ _) =           [_polyInCMD defFormatNames,
                                                         _outputPath defFormatNames,
                                                         _inputPath defFormatNames]

    formatStringVars (HandlerInfo Matr _ _) =           [_codeGen defFormatNames,
                                                         _codeCheck defFormatNames,
                                                         _outputPath defFormatNames,
                                                         _inputPath defFormatNames]

    formatStringVars (HandlerInfo Interleaver _ _) =    [_outputPath defFormatNames, --add interleaver par
                                                         _inputPath defFormatNames]

    formatStringVars (HandlerInfo Noise _ _) =          [_outputPath defFormatNames,
                                                         _inputPath defFormatNames] --add noise par


handlerInfo :: HandlerType -> FormatString -> CascadeHandlers ()
handlerInfo ht fs = Free (HandlerInfo ht fs (Pure ()))


type CascadeHandlers a = Free HandlerInfo a
