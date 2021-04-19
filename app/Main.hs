{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Sandbox
import Data.ByteString.Char8 as B

main :: IO ()
main = do
  dog <- B.readFile "temp/img.png" 
  B.writeFile "temp/img1.png" dog