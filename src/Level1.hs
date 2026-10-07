{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- Можно писать лямбды в своё удовольствие.
{- HLINT ignore "Redundant lambda" -}

-- | Домашка 4. Введение в Haskell: уровень 1, обязательные задачи.
--
-- Условие каждой задачи — комментарий перед её кодом; решение пишется на месте заглушки @todo@,
-- а в задаче 1.6 — на месте заглушки 'Todo' в типе.
module Level1 where


-- 1.1. Пары из λ-исчисления
--
-- pair = λa. λb. λf. f a b   — пара «запоминает» a и b и ждёт функцию-селектор f
-- fst  = λp. p (λa. λb. a)   — передаём селектор, который берёт первый элемент
-- snd  = λp. p (λa. λb. b)   — передаём селектор, который берёт второй элемент

pair = \a b -> \f -> f a b
fstChurch = \p -> p (\a _ -> a)
sndChurch = \p -> p (\_ b -> b)


-- 1.2. Взаимная рекурсия
--
-- Каждая функция делает шаг к нулю и передаёт работу другой.

isEven :: Integer -> Bool
isEven n
  | n == 0 = True
  | n < 0 = isOdd (n + 1)
  | otherwise = isOdd (n - 1)

isOdd :: Integer -> Bool
isOdd n
  | n == 0 = False
  | n < 0 = isEven (n + 1)
  | otherwise = isEven (n - 1)


-- 1.3. Найдите ошибку
--
-- facBuggy 0 = go 0 (-1) = 0   (сразу срабатывает n' <= 0 и возвращается acc = 0),
-- а должно быть 0! = 1.
-- Для остальных n всё верно, например:
-- facBuggy 3 = go 3 2 = go 6 1 = go 6 0 = 6.

facBuggy :: Integer -> Integer
facBuggy n = go n (n - 1)
  where
    go acc n'
      | n' <= 0 = acc
      | otherwise = go (acc * n') (n' - 1)

counterexample :: Integer
counterexample = 0


-- 1.4. Рекуррентная последовательность
--
-- Храним три последних элемента (a, b, c) = (b[k], b[k+1], b[k+2])
-- и на каждом шаге сдвигаем окно на один. Получается линейное время.

itemAt :: Integer -> Integer
itemAt n = go n 1 2 3
  where
    go k a b c
      | k <= 0 = a
      | otherwise = go (k - 1) b c (c - 2 * b + 3 * a)


-- 1.5. Цифры числа
--
-- cnt — сколько цифр уже насчитали, s — их сумма.
-- Отщепляем последнюю цифру (mod 10) и отбрасываем её (div 10).
-- Рекурсивный вызов go — последнее действие, значит, рекурсия хвостовая.

nSumDigits :: Integer -> (Integer, Integer)
nSumDigits n = go (abs n) 0 0
  where
    go m cnt s
      | m < 10 = (cnt + 1, s + m)
      | otherwise = go (m `div` 10) (cnt + 1) (s + m `mod` 10)


-- 1.6. Предскажите тип

-- uncurry const
-- uncurry :: (a -> b -> c) -> (a, b) -> c,  const :: a -> b -> a,  значит c = a
typeOfUncurryConst :: (a, b) -> a
typeOfUncurryConst = undefined

-- curry fst
-- curry :: ((a, b) -> c) -> a -> b -> c,  fst :: (a, b) -> a,  значит c = a
typeOfCurryFst :: a -> b -> a
typeOfCurryFst = undefined

-- flip (,)
-- (,) :: a -> b -> (a, b),  flip меняет порядок аргументов
typeOfFlipPair :: b -> a -> (a, b)
typeOfFlipPair = undefined