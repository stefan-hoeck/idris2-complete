||| Experimenting with shell auto-completion for Idris2
module Main

import Data.Maybe
import Data.String
import System

main : IO ()
main = do idrisWords <- getEnv "IDRIS_WORDS"
          putStrLn $ fromMaybe "" idrisWords
