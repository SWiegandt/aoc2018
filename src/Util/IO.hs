module Util.IO where

import           System.IO

prompt :: String -> IO String
prompt query = do
    putStr query
    hFlush stdout
    getLine
