{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes
import LExp

-- Все способы разделить список на две части.
splits :: [a] -> [([a], [a])]
splits xs = [splitAt i xs | i <- [0 .. length xs]]

-- Выбираем элемент и сохраняем части слева и справа от него.
picks :: [a] -> [([a], a, [a])]
picks [] = []
picks (x:xs) =
  ([], x, xs) : [(x:left, y, right) | (left, y, right) <- picks xs]

-- Правые правила: переносим аргумент в контекст.
dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma (DivL a b) = dec_rinv (a : gamma) b
dec_rinv gamma (DivR b a) = dec_rinv (gamma ++ [a]) b
dec_rinv gamma (Atm p) =
  or [dec_lfoc left a right p | (left, a, right) <- picks gamma]

-- Левые правила: ищем аргумент слева или справа от типа в фокусе.
dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc left (DivL a b) right p =
  or [dec_rinv near a && dec_lfoc far b right p
     | (far, near) <- splits left]

dec_lfoc left (DivR b a) right p =
  or [dec_rinv near a && dec_lfoc left b far p
     | (near, far) <- splits right]

-- Аксиома: один атом, совпадающий с целью.
dec_lfoc left (Atm q) right p =
  null left && null right && q == p

-- Все возможные типы слова из словаря.
typesOf :: Lexicon sem -> String -> [LTp]
typesOf lex w =
  [t | (word, t, _) <- items lex, word == w]

-- Перебираем варианты типов слов и проверяем тип предложения.
isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws =
  not (null ws) &&
  or [dec_rinv gamma (stp lex)
     | gamma <- mapM (typesOf lex) ws]

-- Выбираем имя, которого ещё нет в контексте.
freshFor :: [(Var, LTp)] -> Var
freshFor gamma =
  case [[c] | c <- ['a'..'z'], [c] `notElem` used] of
    x : _ -> x
    []    -> numbered 0
  where
    used = map fst gamma

    numbered :: Integer -> Var
    numbered n
      | name `elem` used = numbered (n + 1)
      | otherwise = name
      where name = "v" ++ show n

-- Правые правила теперь строят выражения вместо ответа True/False.
der_rinv :: [(Var, LTp)] -> LTp -> [LExp]
der_rinv gamma (DivL a b) =
  [L x e | e <- der_rinv ((x, a) : gamma) b]
  where x = freshFor gamma

der_rinv gamma (DivR b a) =
  [L x e | e <- der_rinv (gamma ++ [(x, a)]) b]
  where x = freshFor gamma

der_rinv gamma (Atm p) =
  [foldl A (V x) args
  | (left, (x, a), right) <- picks gamma
  , args <- der_lfoc left a right p]

-- Для типа в фокусе собираем список его аргументов.
der_lfoc :: [(Var, LTp)] -> LTp -> [(Var, LTp)] -> Atm -> [[LExp]]
der_lfoc left (DivL a b) right p =
  [e : args
  | (far, near) <- splits left
  , e <- der_rinv near a
  , args <- der_lfoc far b right p]

der_lfoc left (DivR b a) right p =
  [e : args
  | (near, far) <- splits right
  , e <- der_rinv near a
  , args <- der_lfoc left b far p]

der_lfoc left (Atm q) right p
  | null left && null right && q == p = [[]]
  | otherwise = []

  -- Прочтение: выражение и значения выбранных записей словаря.
type Reading sem = (LExp, [(Var, sem)])

-- Номер различает повторяющиеся слова в предложении.
wordVar :: Int -> String -> Var
wordVar i w = w ++ "_" ++ show i

derivations :: Lexicon sem -> LTp -> [String] -> [Reading sem]
derivations lex goal ws =
  [(e, env)
  | choice <- mapM entriesOf (zip [0..] ws)
  , let gamma = [(x, t) | (x, t, _) <- choice]
        env = [(x, value) | (x, _, value) <- choice]
  , e <- der_rinv gamma goal]
  where
    entriesOf (i, w) =
      [(wordVar i w, t, value)
      | (word, t, value) <- items lex
      , word == w]