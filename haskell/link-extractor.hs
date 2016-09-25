#!/usr/bin/env runhaskell
-- dependencies: libghc-pandoc-dev

-- usage: 'link-extract.hs [file]'; prints out a newline-delimited list of hyperlinks found in targeted Pandoc Markdown files when parsed. To print out the filename for each hyperlink as well, add the option "--print-filenames", which can be helpful in searching many files.
-- Hyperlinks are not necessarily to the WWW but can be internal or interwiki hyperlinks (eg '/local/file.pdf' or '!Wikipedia').

import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, def, readMarkdown, Inline(Link), Pandoc)
import Control.Monad

main :: IO ()
main = do args <- getArgs
          let verbose = "--print-filenames" == args!!0
          let args' = if verbose then tail args else args
          files <- mapM readFile args'
          zipWithM_ (analyzePage verbose) args' files
          return ()

analyzePage :: Bool -> String -> String -> IO Pandoc
analyzePage prt filename contents = do let parsed = readMarkdown def (unlines . drop 1 . lines $ contents)
                                       bottomUpM (printLinks filename prt) parsed

printLinks :: String -> Bool -> Inline -> IO Inline
printLinks f p y@(Link _ (x, _)) = if p then putStrLn (f++": "++x) >> return y else putStrLn x >> return y
printLinks _ _ y                   = return y

{- draft attempt at separation of concerns:

import Control.Monad (when)
import System.Environment (getArgs)
import Text.Pandoc -- (bottomUpM, def, readMarkdown, Inline(Link), Pandoc)

main :: IO ()
main = do args <- getArgs
          let printFiles = "--print" == (args !! 0)
          fileContents <- if printFiles then mapM readFile (drop 1 args) else mapM readFile args
          let files = if printFiles then zip (drop 1 args) fileContents  else zip args fileContents
          let results = zip files (map analyzePage fileContents)
          print results

analyzePage :: String -> Block
analyzePage x = let (Pandoc _ block) = bottomUp parseLink (readMarkdown def (unlines . drop 1 . lines $ x)) in concat block

parseLink :: Inline -> Inline
parseLink (Link _ (x, _)) = Str x
parseLink _               = Str ""
-}
