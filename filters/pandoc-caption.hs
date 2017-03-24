module Main where

import Text.Pandoc.JSON

caption :: Block -> Block
caption p@(Para (dot:rest)) = 
    if shouldTransform dot 
    then Para [(makeSpan (dot:rest))]
    else p

caption x = x

shouldTransform :: Inline -> Bool
shouldTransform (Str ('.':_)) = True
shouldTransform _ = False

trimDot :: Inline -> Inline
trimDot (Str ('.':rest)) = Str rest
trimDot x = x

makeSpan :: [Inline] -> Inline
makeSpan (dot:rest) = Span ("", ["caption"], []) ((trimDot dot):rest)

main :: IO ()
main = toJSONFilter caption


