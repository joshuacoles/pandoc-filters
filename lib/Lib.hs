module Lib ((|>), (|:>), base64, tr, traceShow, toAbs, removeKey, removeKeys) where

import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base64 (encode)

import Debug.Trace
import System.Directory
import System.IO.Unsafe

infixl 5 |>
(|>) a f = f a

infixl 5 |:>
(|:>) f g = g . f

base64 = unpack . encode . pack

tr a = traceShow a a

toAbs = unsafePerformIO . makeAbsolute

removeKey :: Eq a => a -> [(a, b)] -> [(a, b)]
removeKey a l = filter (\(x, y) -> x /= a) l

removeKeys :: Eq a => [a] -> [(a, b)] -> [(a, b)]
removeKeys a l = filter (\(x, y) -> not (x `elem` a)) l

