{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

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
