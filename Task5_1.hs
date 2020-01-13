module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость
data DList a = DNil | DCons { left :: DList a, current :: a, right :: DList a }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist = list2dlist' DNil

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h:t) =
  let rec = DCons left h (list2dlist' rec t)
   in rec

-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil _ = error "Index out of bounds"
index (DCons _ x r) i
  | i == 0 = x
  | otherwise = index r (i - 1)

insertAt :: DList a -> Int -> a -> DList a
-- Список пустой: вставляем в нулевой элемент
insertAt DNil _ value = DCons DNil value DNil
insertAt (DCons left val right) i value
-- Нашли нужный индекс. В следующий надо осуществить вставку.
-- 1) Создаем элемент
-- 2) Переносим текущее значение в правую часть
  | i == 1 =
    let rec = DCons left val (insertAt' rec value right)
     in rec
-- Вставка в 0 элемент
  | i == 0 =
    let rec = DCons DNil value (insertAt' rec val right)
     in rec
-- Если вставляем в индекс, который больше длины списка, то ошибка, иначе продолжаем
  | i /= 1 =
    case right of
      DNil -> error "List index is out of bounds"
      _    -> DCons left val (insertAt right (i - 1) value)

insertAt' :: DList a -> a -> DList a -> DList a
-- Обновляем последний элемент
insertAt' left val DNil = DCons left val DNil
-- Обновляем все правые элементы, прокидывая "себя" в правый элемент
insertAt' left val (DCons left' val' right) =
  let rec = DCons left val (insertAt' rec val' right)
   in rec

removeAt :: DList a -> Int -> DList a
-- Слева пусто
removeAt DNil _ = error "List is empty. Nothing to remove"
-- Справа пусто
removeAt (DCons _ _ DNil) i
  | i == 0 = DNil
  | otherwise = error "Incorrect index value"

-- Удаление нулевого индекса
removeAt (DCons left current (DCons _ val right)) 0 = DCons left val right
-- Общий случай
removeAt (DCons left current right) index = DCons left current (removeAt right (index - 1))
