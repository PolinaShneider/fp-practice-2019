module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x lst =
  case lst of
      [] -> x
      (head:tail) -> foldl f (f x head) tail

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x lst =
    case lst of
        [] -> x
        (head:tail) -> f head (foldr f x tail)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f z =
  case f z of
    Nothing -> []
    Just (a, b) -> a : unfoldr f b

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum = foldl (+) 0

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse = foldl f [] where f t h = h : t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldl (\acc x -> acc ++ [f x]) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldl (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldl f []
  where
    f acc (Just elem) = elem : acc
    f acc Nothing = acc

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal matrix = fst $ foldl func ([], 0) matrix
  where
    func (result, n) lst = (result ++ [lst !! n], n + 1)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr foo []
  where
    foo x acc =
      if f x
        then x : acc
        else acc

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl func False
  where
    func acc element = (element == x) || acc

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr func from
  where func x = if (x < to && step > 0) || (x > to && step < 0) then Just (x, x + step) else Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append first second = foldr (:) second first

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\x -> if null x then Nothing else Just (splitAt (fromIntegral n) x)) lst
