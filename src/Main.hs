module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.Options
import Idris.ElabDecls
import Idris.REPL
import Idris.Main
import Idris.ModeCommon

import IRTS.Compiler
import IRTS.CodegenPerl6

import System.Environment
import System.Exit

import Paths_idris_perl6

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath }

showUsage = do putStrLn "Usage: idris-perl <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.perl") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--yes-really":xs) = process opts xs -- GRRR
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

c_main :: Opts -> Idris ()
c_main opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- elabMain
                 ir <- compile (Via IBCFormat "perl6") (output opts) (Just mainProg)
                 runIO $ codegenPerl6 ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else runMain (c_main opts)


