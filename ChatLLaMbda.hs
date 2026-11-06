import Data.Char
import System.IO
import Control.Exception

import LTypes
import qualified Lambek

import Parser
import PySupport

import LExp (prettyLExp)
import Semantics
import Data.List (nub, intercalate)

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

-- Проверяем, используется ли атомарный тип в словаре.
usesAtom :: Atm -> Lexicon sem -> Bool
usesAtom p lex = any (\(_, t, _) -> occurs t) (items lex)
  where
    occurs (Atm q) = p == q
    occurs (DivL a b) = occurs a || occurs b
    occurs (DivR b a) = occurs a || occurs b

-- Принимаем предложения и, если словарь поддерживает их, вопросы.
goalTypes :: Lexicon sem -> [LTp]
goalTypes lex =
  nub (stp lex : [Atm "q" | usesAtom "q" lex])

-- read-eval-print-loop
-- showDerivs определяет, показывать ли выражения вместе с ответом.
repl :: Bool -> Lexicon PythonCode -> PythonHandle -> IO ()
repl showDerivs elx phandle = do
  putStr "> "
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      line <- getLine
      case words line of
        [] -> repl showDerivs elx phandle

        [":quit"] -> putStrLn "Bye!"

        [":help"] -> do
          putStrLn ":derivs  show/hide lambda expressions"
          putStrLn ":quit    exit"
          putStrLn ":help    show commands"
          repl showDerivs elx phandle

        [":derivs"] -> do
          let enabled = not showDerivs
          putStrLn ("Showing derivations: "
                    ++ if enabled then "on" else "off")
          repl enabled elx phandle

        input -> do
          let ws = map stdize input
          answer showDerivs elx phandle ws
          repl showDerivs elx phandle

-- Обработка обычного предложения.
answer :: Bool -> Lexicon PythonCode -> PythonHandle -> [String] -> IO ()
answer showDerivs elx phandle ws = do
  let unknown = nub [w | w <- ws, null (Lambek.typesOf elx w)]
      readings = concat
        [Lambek.derivations elx goal ws | goal <- goalTypes elx]
        
  if not (null unknown)
    then putStrLn ("Unknown words: " ++ intercalate ", " unknown)
    else if null readings
      then putStrLn "That is not a well-formed sentence"
      else do
        values <- mapM (evalReading phandle) readings
        let senses = zip readings values

        if showDerivs
          then do
            putStrLn ("Derivations: " ++ show (length readings))
            mapM_ (\((e, _), value) ->
              putStrLn ("  " ++ prettyLExp e ++ "  ==>  " ++ value))
              senses
          else return ()

        case senses of
          [(_, value)] -> putStrLn value
          _ -> do
            putStrLn "That sentence is ambiguous"
            mapM_ (\((e, _), value) ->
              putStrLn ("Sense [" ++ prettyLExp e ++ "]: " ++ value))
              senses
      
main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython (PCode (text db ++ "\n" ++ pyPrelude))
    -- start the read-eval-print loop
  putStrLn "Starting chat..."
  repl False elx phandle
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
