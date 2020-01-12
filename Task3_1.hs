module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
instance Show WeirdPeanoNumber where
  show Zero       = "Zero"
  show (Succ wpn) = "Succ " ++ show wpn
  show (Pred wpn) = "Pred " ++ show wpn

instance Eq WeirdPeanoNumber where
  (==) wpn1 wpn2 = toInt wpn1 == toInt wpn2

instance Ord WeirdPeanoNumber where
  compare wpn1 wpn2 = compare' (normalize wpn1) (normalize wpn2)

instance Enum WeirdPeanoNumber where
  toEnum i = fromInt (fromIntegral i)
  fromEnum wpn = fromIntegral (toInt wpn)

instance Real WeirdPeanoNumber where
  toRational wpn = toRational (toInt wpn)

instance Integral WeirdPeanoNumber where
  toInteger = toInt
  quotRem = quotRem'

instance Num WeirdPeanoNumber where
  (+) = add
  (*) = multiply
  fromInteger = fromInt
  negate = negate'
  abs = abs'
  signum = signum'

-- Получить Int из числа Пеано
toInt :: WeirdPeanoNumber -> Integer
toInt Zero       = 0
toInt (Succ wpn) = 1 + toInt wpn
toInt (Pred wpn) = toInt wpn - 1

-- Получить число Пеано из Int
fromInt :: Integer -> WeirdPeanoNumber
fromInt i
  | i > 0 = Succ (fromInt (i - 1))
  | i < 0 = Pred (fromInt (i + 1))
  | i == 0 = Zero

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize wpn = fromInt (toInt wpn)

-- Сравнивает два числа
compare' :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
compare' wpn1 wpn2
  | toInt wpn1 > toInt wpn2 = GT
  | toInt wpn1 < toInt wpn2 = LT
  | toInt wpn1 == toInt wpn2 = EQ

-- Возвращает целую часть и остаток от деления
quotRem' :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
quotRem' wpn1 wpn2 =
  case (wpn1, wpn2) of
    (Zero, _) -> (Zero, Zero)
    (_, Zero) -> error "Division by zero!"
    (_, _) -> (res, wpn1 - wpn2 * res)
      where s
              | wpn2 > 0 = wpn1
              | wpn2 < 0 = negate wpn1
            res = wpnQuot (abs wpn1) (abs wpn2) s

wpnQuot :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
wpnQuot wpn1 wpn2 s
  | wpn1 >= wpn2 =
    if s < 0
      then Pred (wpnQuot (wpn1 - wpn2) wpn2 s)
      else Succ (wpnQuot (wpn1 - wpn2) wpn2 s)
  | otherwise = Zero

-- Складывает два числа
add :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
add wpn1 Zero        = wpn1
add wpn1 (Succ wpn2) = Succ (wpn1 + wpn2)
add wpn1 (Pred wpn2) = Pred (wpn1 + wpn2)

-- Меняет знак числа
negate' :: WeirdPeanoNumber -> WeirdPeanoNumber
negate' Zero       = Zero
negate' (Succ wpn) = Pred (negate' wpn)
negate' (Pred wpn) = Succ (negate' wpn)

-- Возвращает модуль числа
abs' :: WeirdPeanoNumber -> WeirdPeanoNumber
abs' wpn
  | wpn > 0 = wpn
  | wpn < 0 = negate' wpn
  | wpn == 0 = Zero

-- Возвращает -1 для отрицательных чисел, 0 для ноля, и 1 для положительных чисел
signum' :: WeirdPeanoNumber -> WeirdPeanoNumber
signum' Zero     = Zero
signum' (Succ _) = Succ Zero
signum' (Pred _) = Pred Zero

-- Умножает два числа
multiply :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply wpn1 wpn2
  | signum' wpn1 == signum wpn2 = mult (abs' wpn1) (toInt (abs' wpn2))
  | otherwise = negate' (mult (abs' wpn1) (toInt (abs' wpn2)))
  where
    mult x 0 = Zero
    mult x n = x + mult x (n - 1)
