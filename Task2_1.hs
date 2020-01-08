module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Prelude hiding(lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty | Leaf (Integer, v) | Node (Integer, v) (TreeMap v) (TreeMap v) deriving (Show, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains tree k =
    case tree of
        Empty -> False
        Node (key, _) left right
            | key == k -> True
            | key < k -> contains left k
            | key > k -> contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k tree =
    case tree of
        Empty -> error "Key isn't found in the Tree"
        Node (key, value) left right
            | key == k -> value
            | key > k -> lookup k right
            | key < k -> lookup k left

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) tree =
    case tree of
        Empty -> Node (k, v) Empty Empty
        Node (key, value) left right
            | key == k -> Node (k, v) left right
            | key > k -> Node (k, v) (insert (k, v) left) right
            | key < k -> Node (k, v) left (insert (k, v) right)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i Empty = Empty
remove i (Leaf (key, value))
  | i == key = Empty
  | otherwise = Leaf (key, value)
remove i (Node (key, value) Empty (Leaf (key', value')))
  | i == key = Leaf (key', value')
  | i == key' = Leaf (key, value)
  | otherwise = Node (key, value) Empty (Leaf (key', value'))

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE node result =
  case result of
    Empty -> error "Not found"
    Node (key, value) left right
      | key == node -> (key, value)
      | key > node -> nearestLE node left
      | key < node ->
        case right of
          Node (key, value) _ _
            | node == key -> (key, value)
            | node /= key -> nearestLE node right
          Empty -> (key, value)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList = foldr insert Empty

-- Построение списка пар из дерева
listFromTree :: TreeMap v      -> [(Integer, v)]
listFromTree Empty             = []
listFromTree (Leaf (k, v))     = [(k, v)]
listFromTree (Node (k, v) l r) = listFromTree l ++ [(k, v)] ++ listFromTree r

-- Вычисление размера дерева
size :: TreeMap v -> Integer
size Empty = 0
size (Leaf (key, value)) = 1
size (Node (key, value) left right) = size left + 1 + size right

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Empty = error "The Tree is empty"
kMean k (Leaf (key, value))             | k == 0 = (key, value)
                                        | otherwise = error "Cannot calculate k-mean, we have just a single Leaf"
kMean k (Node (key, value) left right)  | k < size left = kMean k left
                                        | k > size left = kMean (k - size left - 1) right
                                        | otherwise = (key, value)
