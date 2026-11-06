module Semantics where

import LExp
import PySupport
import Lambek (Reading)
import Data.Char (ord)
import Numeric (showHex)
import Data.List (elemIndex)

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

    -- Форма выражения для сравнения прочтений.
-- У связанных переменных сравниваем положение, а не имя.
data Meaning
  = Bound Int
  | WordMeaning Var String
  | Apply Meaning Meaning
  | Abstract Meaning
  | Conjunction [Meaning]
  deriving (Eq, Show)

readingMeaning :: Reading PythonCode -> Meaning
readingMeaning (expr, env) = go [] expr
  where
    go bound (V x) =
      case elemIndex x bound of
        Just i  -> Bound i
        Nothing -> WordMeaning x (maybe "" text (lookup x env))

    -- Для стандартного and убираем различия в расстановке скобок.
    go bound (A (A (V f) a) b)
      | f `notElem` bound && isAnd f =
          Conjunction (parts (go bound a) ++ parts (go bound b))

    go bound (A f x) = Apply (go bound f) (go bound x)
    go bound (L x e) = Abstract (go (x : bound) e)

    isAnd f =
      case lookup f env of
        Just code ->
          words (text code) == words "lambda x : lambda y : x and y"
        Nothing -> False

    parts (Conjunction xs) = xs
    parts x = [x]

sameMeaning :: Reading PythonCode -> Reading PythonCode -> Bool
sameMeaning a b = readingMeaning a == readingMeaning b