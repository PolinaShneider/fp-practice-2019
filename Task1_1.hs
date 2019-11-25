module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data BinaryOperation = Plus | Minus | Mult deriving (Show, Eq)

data Term = IntConstant { intValue :: Int } -- числовая константа
  | Variable { varName :: String } -- переменная
  | BinaryTerm { lhv :: Term, binOp :: BinaryOperation, rhv :: Term } -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l = BinaryTerm l Plus

(|-|) :: Term -> Term -> Term
(|-|) l = BinaryTerm l Minus

(|*|) :: Term -> Term -> Term
(|*|) l = BinaryTerm l Mult

-- Установим приоритет операций
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
  case expression of
    (Variable var) ->
      if var == varName
        then replacement
        else expression
    (BinaryTerm l op r) -> BinaryTerm (replaceVar varName replacement l) op (replaceVar varName replacement r)
    _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression =
  case expression of
    BinaryTerm left op right ->
      let l = evaluate left
          r = evaluate right
       in case (l, op, r) of
            (IntConstant a, Plus, IntConstant b)  -> IntConstant $ a + b
            (IntConstant a, Minus, IntConstant b) -> IntConstant $ a - b
            (IntConstant a, Mult, IntConstant b)  -> IntConstant $ a * b
            _                                     -> BinaryTerm l op r
    _ -> expression
