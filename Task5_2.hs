module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

instance (Show a) => Show (Zipper a) where
    show (Zipper l r) = show (reverse l) ++ show r  

instance (Eq a) => Eq (Zipper a) where
    (==) z1 z2 = toList z1 == toList z2 

toList :: Zipper a -> [a]
toList (Zipper l r) = l ++ r

fromList :: [a] -> Zipper a
fromList = Zipper []

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concat' :: Zipper a -> Zipper a -> Zipper a
concat' left right = Zipper (toList left) (toList right)

insertManyAt' :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt' 0 (Zipper left right) (Zipper left_ right_) = Zipper (left_ ++ left) (right ++ right_)
insertManyAt' index target source = insertManyAt' (index - 1) target (goRight source)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index (Zipper [] []) into = into
insertManyAt index target (Zipper [] []) = target
insertManyAt index target (Zipper left right) = if index < 0 || index > length(left ++ right)
                                         then error "Incorrect Index"
                                         else insertManyAt' index target (Zipper [] (left ++ right))

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to (Zipper [] []) = error "Zipper is empty"
subZipper from to (Zipper left right)
                        | from < 0 || from > length(left ++ right) = error "From is greater than list length"
                        | to < 0 || to > length(left ++ right) = error "To is greater than list length"
                        | from > to = error "Error from > to"
                        | otherwise = Zipper [] $take (to - from + 1) $drop from $concat [left,right]