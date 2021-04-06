{-# LANGUAGE OverloadedStrings #-}
module Experiment.Types.Common.FormatString.FormatString where

import Data.HashMap.Strict as H
import Control.Monad.Except
import Prelude hiding (concat, head, tail)
import Data.Text  (append, unpack, Text, intercalate, head, tail, foldl)
import FaultHandling (HandlerException (..))
import Data.String (fromString, IsString)
import Data.Text.Format.Simple (format', format)


--Когда пользователь задает форматную строку он указывает как вызывать его приложение 
--Например, для вызова исполняемого файла на Linux форматная может иметь вид
--"${filePath} " ++ Str ++ " ${n} " ++ Str ++ " ${k} " Str ++ " ${runMode} " ++ Str ++ " ${input} " ++ Str ++ " ${output}"
--Str - любая
--filePath - путь к исполняемому файлу
--

type OutPutWaiter = FilePath -> String --Частично примененная функция создания строки для исполнения, в аргументе ожидается путь вывода
type FormatString = Text

data FormatNames = FormatNames { --eDsl вместо всего этого???
    _codeN :: Text
   ,_codeK :: Text
   ,_inputPath :: Text
   ,_outputPath :: Text 
   ,_runMode :: Text
   ,_execPath :: Text
   ,_polyInCMD :: Text
   ,_polyFile :: Text
   ,_codeGen :: Text
   ,_codeCheck :: Text
} 

defFormatNames :: FormatNames
defFormatNames = FormatNames {
    _codeN = "n"
   ,_codeK = "k"
   ,_inputPath = "fin"
   ,_outputPath = "fout"
   ,_execPath = "execPath"
   ,_runMode = "runMode"
   ,_polyInCMD = "polyInCMD"
   ,_polyFile = "polyFile"
   ,_codeGen = "codeGen"
   ,_codeCheck = "codeCheck"
}

mapOfParamsToStringWithTemplate :: HashMap Text Text -> FormatString -> String
mapOfParamsToStringWithTemplate paramsMap formatOrder = unpack $ format' formatOrder ((flip H.lookup) paramsMap)
  