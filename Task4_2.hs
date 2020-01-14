module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

-- Functor, отображает элементы одного множества на элементы другого.
-- Применим функцию func к каждом элементу FourOf
instance Functor FourOf where
    fmap func (FourOf a b c d) = FourOf (func $ a) (func $ b) (func $ c) (func $ d)

instance Applicative FourOf where
-- Нам не важен контекст — всегда будем возвращать значение типа FourOf
    pure el = FourOf el el el el
-- Применим функции (func1 func2 func3 func4) в контексте к значениям в контексте (a1 a2 a3 a4)
-- То есть мы применяем каждый элемент второй четверки (a, b, c, d) к функциям из первой четверки
    (<*>) (FourOf func1 func2 func3 func4) (FourOf a b c d) = FourOf (func1 $ a) (func2 $ b) (func3 $ c) (func4 $ d)

instance Monad FourOf where
-- Обернем значение в монаду
    return x = FourOf x x x x
-- Для последовательного применения функции в определении `>>=` используется вспомогательный pattern matching.
-- С его помощью аргументы конструктора по порядку упаковываются в монаду. 
-- После поэлементной распаковки получаем конструктор `FourOf` со значениями, в которых учтен результат отображения func.     
    FourOf a b c d >>= func = FourOf a' b' c' d' where
        FourOf a' _ _ _ = func a 
        FourOf _ b' _ _ = func b
        FourOf _ _ c' _ = func c
        FourOf _ _ _ d' = func d
