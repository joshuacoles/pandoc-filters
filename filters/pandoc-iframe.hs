module Main where

import Text.Pandoc.JSON

import Lib (base64)

iframe :: Block -> Block
iframe cb@(CodeBlock (id, classes, namevals) contents) =
  if "iframe" `elem` classes 
  then RawBlock (Format "html") (createHTML contents)
  else cb

iframe x = x

createHTML :: String -> String
createHTML text = "<iframe></iframe><script>document.currentScript.previousElementSibling.contentDocument.write(atob(\"" ++ (base64 text) ++"\"))</script>"

main :: IO ()
main = toJSONFilter iframe
