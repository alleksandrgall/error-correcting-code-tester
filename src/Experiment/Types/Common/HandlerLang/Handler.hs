{-# LANGUAGE OverloadedStrings #-}

module Experiment.Types.Common.HandlerLang.Handler
    (CascadeHandlers,
    compileExecStrings,
    handlerInfo)
where

import Experiment.Types.Common.HandlerLang.HandlerDef

import FaultHandling

import Control.Monad.Free
import Control.Monad.Except
import Experiment.Types.Common.FormatString
import Data.HashMap.Lazy as H (HashMap, insert, keys)
import Data.List ((\\))
import Control.Monad.ST
import Data.STRef
import qualified Data.Text as T

validator :: Bool -> HandlerInfo a -> HashMap T.Text T.Text -> HandlerException ()
validator isFirst h hm = do
    validateParamMap isFirst h hm
    validateFormatString h

validateParamMap :: Bool -> HandlerInfo a -> HashMap T.Text T.Text -> HandlerException ()
validateParamMap isFirst h hm = do
    let missingParams = if isFirst then (formatStringVars h \\ keys hm) \\ [_outputPath defFormatNames] else ((formatStringVars h \\ keys hm)
                    \\ [_outputPath defFormatNames]) \\ [_inputPath defFormatNames]
    when (missingParams /= []) $
      throwError (BadParams $ "You are missing following params: " `T.append` T.intercalate ", " missingParams)

validateFormatString :: HandlerInfo a -> HandlerException ()
validateFormatString h = processRes (foreignUnused (getFormatString h) (formatStringVars h))                   

processRes :: ([T.Text], [T.Text]) -> HandlerException ()
processRes (foreignVars, unusedVars)    
        | null unusedVars && null foreignVars = return ()
        | null unusedVars && (T.last (last foreignVars) /= '}') = throwError (IncorrectFormatString $ "Parse error on" `T.append` last foreignVars)
        | null unusedVars = throwError (IncorrectFormatString $ "Found unknown vars: " `T.append` T.intercalate ", " foreignVars)
        | unusedVars /= [] && foreignVars /= [] = throwError (IncorrectFormatString $ "Found unknown vars: " `T.append` T.intercalate ", " foreignVars
                                                `T.append` "\n" `T.append` "Neccesery vars are missing: " `T.append` T.intercalate ", " unusedVars)
        | otherwise = throwError (IncorrectFormatString $ "Neccesery vars are missing: " `T.append` T.intercalate ", " unusedVars)

foreignUnused :: T.Text -> [T.Text] -> ([T.Text], [T.Text])
foreignUnused formStr vars = runST $ do 
    varsRef <- newSTRef vars 
    foreignRef <- newSTRef [T.empty]
    foldM_ (helper varsRef foreignRef) T.empty (T.unpack formStr)
    unusedVars <- readSTRef varsRef
    foreignVars <- readSTRef foreignRef
    return (foreignVars, unusedVars)

helper:: STRef s [T.Text] -> STRef s [T.Text] -> T.Text -> Char -> ST s T.Text
helper unusedRef foreignRef acc c = do 
    let headIsDol x = T.head x == '$'
        secIsOpen x = (T.head . T.tail $ x) == '{'
        inVar x = headIsDol x && secIsOpen x
        curacc | inVar acc = acc `T.snoc` c
               | c == '$' = acc `T.snoc` c
               | c == '{' && headIsDol acc = acc `T.snoc` c
               | otherwise = acc
        isVar x = inVar x && (T.last acc == '}')
    if not (inVar curacc) then return T.empty 
    else case isVar curacc of
        True -> checkAccValid unusedRef foreignRef curacc
        False -> return curacc

checkAccValid :: STRef s [T.Text] -> STRef s [T.Text] -> T.Text -> ST s T.Text 
checkAccValid unusedRef foreignRef someVar = do
    unusedVars <- readSTRef unusedRef
    if someVar `elem` unusedVars then
        writeSTRef unusedRef (unusedVars \\ [someVar])
    else 
        modifySTRef' foreignRef (\foreignVars -> someVar:foreignVars)
    return T.empty

type InputOutputWaiter = FilePath -> FilePath -> String

compileExecStrings :: CascadeHandlers () -> [HashMap T.Text T.Text] -> HandlerException [InputOutputWaiter]
compileExecStrings = interpretCascadeHandler True []

--FilePath содержит информация о том, куда записан результат работы предыдущего файла при каскадировании
interpretCascadeHandler ::
       Bool
    -> [InputOutputWaiter]                      
    -> CascadeHandlers () 
    -> [HashMap T.Text T.Text] 
    -> HandlerException [InputOutputWaiter]
interpretCascadeHandler isFirst res (Pure _) _ | not isFirst = return (reverse res)
                                               | otherwise = throwError (BadParams "Empty handler")
interpretCascadeHandler isFirst res (Free _) [] | not isFirst = return (reverse res)
                                        | otherwise = throwError (BadParams "Empty params")
interpretCascadeHandler isFirst res (Free hInfo) paramsMaps = do
    validator isFirst hInfo (head paramsMaps)
    interpretHandInfo isFirst res hInfo paramsMaps

interpretHandInfo :: 
       Bool 
    -> [InputOutputWaiter]   
    -> HandlerInfo (CascadeHandlers ()) 
    -> [HashMap T.Text T.Text] 
    -> HandlerException [InputOutputWaiter]
interpretHandInfo isFirst res (HandlerInfo _ fs next) (p:params) 
    = let newF = if not isFirst then \inp out -> makeExecString fs (changeInput inp . changeOutput out $ p) 
                                else \_ out -> makeExecString fs (changeOutput out p)
      in interpretCascadeHandler False (newF:res) next params

changeInput :: FilePath -> HashMap T.Text T.Text -> HashMap T.Text T.Text
changeInput newInput = insert (_inputPath defFormatNames) (T.pack newInput)

changeOutput :: FilePath -> HashMap T.Text T.Text -> HashMap T.Text T.Text
changeOutput newOutput = insert (_outputPath defFormatNames) (T.pack newOutput)

makeExecString :: FormatString -> HashMap T.Text T.Text -> String
makeExecString formatStr paramsHashMap = mapOfParamsToStringWithTemplate paramsHashMap formatStr 