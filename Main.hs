module Main where

import System
import System.Cmd


import Language.MUDA.AST
import Language.MUDA.Parser
import Language.MUDA.PPrint

debugPrinter ast =  do putStrLn $ "// [DBG]"
                       putStrLn $ show ast  

main = do args <- getArgs
          if length args > 0

            then

              do { runLex program (args !! 0) (args !! 0) debugPrinter }

            else

              error "muda"
