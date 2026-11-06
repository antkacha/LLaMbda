module Semantics where

import LExp
import PySupport
import Lambek (Reading)
import Data.Char (ord)
import Numeric (showHex)

-- Префикс отличает связанные переменные от имён в базе фактов.
pyVar :: Var -> String
pyVar x = "_v_" ++ x

-- Слова заменяем их значениями из словаря.
toPython :: [(Var, PythonCode)] -> LExp -> String
toPython env (V x) =
  case lookup x env of
    Just code -> "(" ++ text code ++ ")"
    Nothing   -> pyVar x

-- Применение превращается в вызов функции.
toPython env (A f x) =
  "(" ++ toPython env f ++ ")(" ++ toPython env x ++ ")"

-- Лямбда-абстракция превращается в Python lambda.
toPython env (L x e) =
  "(lambda " ++ pyVar x ++ " : "
  ++ toPython (filter (\(name, _) -> name /= x) env) e ++ ")"

readingToPython :: Reading PythonCode -> String
readingToPython (e, env) = toPython env e

-- Передаём выражение как строку: экранируем кавычки и спецсимволы.
pyStringLit :: String -> String
pyStringLit s = "\"" ++ concatMap escape s ++ "\""
  where
    escape c
      | c >= ' ' && c <= '~' && c /= '"' && c /= '\\' = [c]
      | otherwise =
          let h = showHex (ord c) ""
          in "\\U" ++ replicate (8 - length h) '0' ++ h

-- Python возвращает одну строку: результат или описание ошибки.
pyPrelude :: String
pyPrelude = unlines
  [ ""
  , "sys.ps2 = ''"
  , "def __llrun(src):"
  , "    try:"
  , "        value = eval(src, globals())"
  , "        if isinstance(value, (list, set, tuple, frozenset)):"
  , "            names = sorted(set(str(x) for x in value))"
  , "            result = ', '.join(names) if names else 'nobody'"
  , "        else:"
  , "            result = str(value)"
  , "    except Exception as err:"
  , "        result = 'ERROR: ' + type(err).__name__ + ': ' + str(err)"
  , "    print(result.replace('\\n', ' '), flush=True)"
  , ""
  , ""
  ]

evalReading :: PythonHandle -> Reading PythonCode -> IO String
evalReading handle reading =
  runPythonCode handle
    (PCode ("__llrun(" ++ pyStringLit (readingToPython reading) ++ ")"))