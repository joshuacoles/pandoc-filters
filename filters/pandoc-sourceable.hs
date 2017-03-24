module Main where

import Text.Pandoc.JSON

import Selection
import Lib

source :: Block -> IO Block
source cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "src" namevals of 
    Just src -> return . (CodeBlock (id, classes, (removeKeys ["src", "lines"] namevals))) . select (lookup "lines" namevals) =<< readFile (toAbs $ src)
    Nothing -> return cb
  
  
source x = return x

main :: IO ()
main = toJSONFilter source
