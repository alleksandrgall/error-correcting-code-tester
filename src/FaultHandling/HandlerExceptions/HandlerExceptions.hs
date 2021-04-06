module FaultHandling.HandlerExceptions.HandlerExceptions where
--Проверка наличия всех необходимых параметров запуска в ExecExt (makeExecHandler), при запуске полученного ExecHandler

import Data.Text
import Control.Monad.Except
import Control.Monad.Identity

type HandlerException a = ExceptT HandlerExceptionConstr Identity a

data HandlerExceptionConstr = IncorrectFormatString !Text --Проверка корректности форматстринг в реалтайме в гуи
                      | BadParams !Text
                      | ProgramIsNotExecutable !Text
                      | UnexpectedException !Text