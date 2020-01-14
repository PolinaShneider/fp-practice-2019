module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

-- Functor занимается преобразованием элементов одного множества в элементы другого множества
-- Здесь fmap применяет функцию func ко всем элементам с типом `el`, хранящимся в контейнере.
instance Functor FunMonad where
    fmap func (FunMonad x) = FunMonad (\el -> func (x el))

-- Applicative оборачивает в контекст и аргумент, и функцию, которая к нему применяется 
instance Applicative FunMonad where
-- pure помещает значения в контекст
-- То есть независимо от аргумента мы получим FunMonad
    pure x = FunMonad (\a -> x)
-- <*> применяет функцию, помещенную в контекст func1 к значению, помещенному в контекст func2
-- То есть мы сначала достаем оба параметра из контекста, а затем применяем полученную функцию к значению
    (FunMonad func1) <*> (FunMonad func2) = FunMonad (\a -> func1 a (func2 a))

instance Monad FunMonad where
-- Результат вызова `return` - монадический контейнер
    return x = FunMonad (\a -> x)
-- >>= применяет функцию func к FunMonad и возвращает результат, обернутый в FunMonad
    FunMonad x >>= func = FunMonad (\a -> fun (func (x a)) a)