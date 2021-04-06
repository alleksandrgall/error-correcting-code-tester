{-#LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Experiment.Types.Common.ParamsLang.Params where

import Control.Monad.Free
import Data.HashMap.Lazy
import Experiment.Types.Common.FormatString
import Data.String (fromString)
import Data.Text hiding (empty)

--Данные для запуска исполняемого файла
data Param a =  CodeN Int a
            | CodeK Int a
            | PolyInCMD [Double] a
            | PolyFile FilePath a
            | InputPath FilePath a
            | OutputPath FilePath a
            | CodeGen FilePath a
            | CodeCheck FilePath a
    deriving (Functor)
type ExecParams a = Free Param a
type CascadeParams a = Free (Free Param) a

codeN :: Int -> ExecParams ()
codeN n = Free (CodeN n (Pure ()))

codeK :: Int -> ExecParams ()
codeK k = Free (CodeK k (Pure ()))

codeGen :: FilePath -> ExecParams ()
codeGen genMatr = Free (CodeGen genMatr (Pure ()))

codeCheck :: FilePath -> ExecParams ()
codeCheck checkMatr = Free (CodeCheck checkMatr (Pure ()))

polyInCMD :: [Double] -> ExecParams ()
polyInCMD pl = Free (PolyInCMD pl (Pure ()))

polyFile :: FilePath -> ExecParams ()
polyFile pl = Free (PolyFile pl (Pure ()))

inputPath :: FilePath -> ExecParams ()
inputPath fPath = Free (InputPath fPath (Pure ()))

outputPath :: FilePath -> ExecParams ()
outputPath fPath = Free (OutputPath fPath (Pure ()))

execParams :: ExecParams () -> CascadeParams ()
execParams exP = Free (exP >> Pure (Pure ()))

paramsToMaps :: CascadeParams () -> [HashMap Text Text]
paramsToMaps cascadePrms = Prelude.reverse (interpretCascadeParams [] cascadePrms)

interpretCascadeParams :: [HashMap Text Text] -> CascadeParams () -> [HashMap Text Text]
interpretCascadeParams acc (Pure _) = acc
interpretCascadeParams acc (Free params) = interpretExecParams acc empty params


interpretExecParams :: [HashMap Text Text] -> HashMap Text Text -> ExecParams (CascadeParams ()) -> [HashMap Text Text]
interpretExecParams acc hm (Pure next) = interpretCascadeParams (hm:acc) next
interpretExecParams acc hm (Free par) = interpretParam acc hm par

interpretParam :: [HashMap Text Text] -> HashMap Text Text -> Param (ExecParams (CascadeParams ())) -> [HashMap Text Text]
interpretParam acc hm (CodeN n next) = let hm' = insert (_codeN defFormatNames) (pack. show $ n) hm
                                        in interpretExecParams acc hm' next
interpretParam acc hm (CodeK k next) = let hm' = insert (_codeK defFormatNames) (pack. show $ k) hm
                                        in interpretExecParams acc hm' next
interpretParam acc hm (InputPath fin next) = let hm' = insert (_inputPath defFormatNames) (fromString fin) hm
                                            in interpretExecParams acc hm' next
interpretParam acc hm (OutputPath fout next) = let hm' = insert (_outputPath defFormatNames) (fromString fout) hm
                                               in interpretExecParams acc hm' next
interpretParam acc hm (PolyInCMD ply next) = let hm' = insert (_polyInCMD defFormatNames) (showPoly ply) hm
                                        in interpretExecParams acc hm' next    
            where  showPoly (p:ps) = (pack. show $ p) `append` showPoly ps
                   showPoly [] = "" 
interpretParam acc hm (PolyFile ply next) = let hm' = insert (_polyFile defFormatNames) (fromString ply) hm
                                            in interpretExecParams acc hm' next
interpretParam acc hm (CodeGen genMatr next) = let hm' = insert (_codeGen defFormatNames) (fromString genMatr) hm
                                            in interpretExecParams acc hm' next
interpretParam acc hm (CodeCheck checkMatr next) = let hm' = insert (_codeCheck defFormatNames) (fromString checkMatr) hm
                                                in interpretExecParams acc hm' next