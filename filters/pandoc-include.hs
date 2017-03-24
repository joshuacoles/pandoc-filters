module Main where 

import Text.Pandoc.JSON
import Control.Monad
import System.Directory
import System.FilePath
import Lib

import System.Process
import System.Posix.Env (getEnvDefault)
import System.IO.Unsafe

pandocCommand :: IO String
pandocCommand = getEnvDefault "MARKDOWN_WRAPPER" "pandoc"

execCommand :: String -> String -> IO String
execCommand pandoc markdown = readCreateProcess cp ""
    where cp = (proc pandoc [markdown]) { cwd = (Just (takeDirectory markdown)) }

parseFile :: FilePath -> IO [Block]
parseFile file = pandocCommand >>= (\cmd -> execCommand cmd file) >>= (return . (read :: String -> [Block]))

isComment ('#':_) = True
isComment _       = False


computeList :: String -> IO [String]
computeList = lines |:> filter (not . isComment) |:> filterM doesFileExist

include :: Block -> IO [Block]

include (CodeBlock (_, ["include"], _) list) = 
    (computeList list) >>= (return . fmap (parseFile . unsafePerformIO . makeAbsolute)) >>= mconcat

include x = return [x]

main :: IO ()
main = toJSONFilter include
