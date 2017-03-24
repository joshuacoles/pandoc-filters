module Selection (select) where

import Data.List.Split
import Lib

-- selectedLines :: String -> [Int]
-- selectedLines = splitOn " " |:> map (read :: String -> Int) |:> map (\x -> x - 1)

-- selectFn :: String -> ((Int, String) -> (Int, String))
-- selectFn selection = (\lines -> ) 
--     where lns = selectedLines selection

select :: Maybe String -> String -> String
select selection' text = case selection' of
    Just selection -> select' selection text
    Nothing        -> text

select' :: String -> String -> String
select' selection text = selection |> splitOn " " |> map (read :: String -> Int) |> map (\x -> x - 1) |> map (lns !!) |> unlines
    where lns = lines text
