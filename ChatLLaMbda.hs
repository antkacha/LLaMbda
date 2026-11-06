import Data.Char
import System.IO
import Control.Exception

import LTypes
import qualified Lambek

import Parser
import PySupport

import LExp (prettyLExp)

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

-- read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do
  -- print the prompt
  putStr "> "
  -- get a line of input
  line <- getLine
  -- split it up into "standardized" words
  let ws = map stdize $ words line
  -- test if the string of words is a grammatical sentence according to the lexicon
  let readings = Lambek.derivations elx (stp elx) ws
  if null readings
    then putStrLn "That is not a well-formed sentence"
    else do
      putStrLn "I can derive your sentence as follows:"
      mapM_ (\(e, _) -> putStrLn ("  " ++ prettyLExp e)) readings  -- repeat
  repl elx phandle
      
main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython db
  -- start the read-eval-print loop
  putStrLn "Starting chat..."
  repl elx phandle
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)
      
    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex
