# Лабораторная Работа 1

- Выполнил: Кузьмин Артемий Андреевич
- Группа: P3314
- Вариант: 5, 26

## Содержание

- [Проблема Эйлера 5](#проблема-эйлера-5)
- [Проблема Эйлера 26](#проблема-эйлера-26)
- [Вывод](#вывод)

## Проблема Эйлера 5

### Условие

2520 наименьшее число, которое делится без остатка на все числа от 1 до 10. Необходимо найти наименьшее число, которое делится без остатка на все числа от 1 до 20.

### Решение

Решение задачи сводится к тому, чтобы найти НОК для всех чисел от 1 до 20

#### Решение с помощью хвостовой рекурсии

```haskell
euler5Tail :: Integer -> Integer
euler5Tail n
    | n < 1 = -1
    | otherwise = helper n 1
  where
    helper 1 acc = acc
    helper k acc = helper (k - 1) (lcm acc k)
```

С помощью вспомогательной функции накапливается результат НОК между всеми числами

#### Решение с помощью рекурсии

```haskell
euler5Recursion :: Integer -> Integer
euler5Recursion n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = lcm (euler5Recursion $ n - 1) n
```

Логика аналогична решению хвостовой рекурсией, отличие в том, что здесь не используется дополнительная функция для хранения результата

#### Решение с помощью свертки

```haskell
euler5Foldr :: Integer -> Integer
euler5Foldr n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = foldr lcm 1 [1, 2 .. n]
```

Находим НОК сначала между `1` и `n`, затем между результатом прошлой операции и `n - 1` и так далее

#### Решение с помощью отображения

```haskell
euler5Map :: Integer -> Integer
euler5Map n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = case helper n of
      (h:_) -> h
      [] -> -1
  where
    helper 1 = [1]
    helper k = map (lcm k) (helper $ k - 1)
```

Функция НОК с текущим значением `k` будет рекурсивно применяется к элементу массива. После `n` отображений будет массив с НОК, поэтому возврящается первый элемент списка

#### Решение с помощью бесконечного списка

```haskell
euler5InfiniteList :: Integer -> Integer
euler5InfiniteList n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = case helper n [1 ..] of
      (h:_) -> h
      [] -> -1
  where
    helper _ [] = []
    helper _ [_] = []
    helper 1 list = list
    helper k (x1 : x2 : xs) = helper (k - 1) (lcm x1 x2 : xs)
```

`n` раз находим НОК для первых двух элементов бесконечного списка и добавляем результат в начало.

#### Решение с помощью традиционного языка

```java
    public long euler5(long n) {
        if (n < 1) return -1;
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result = (result * i) / MathFunctions.gcd(result, i);
        }
        return result;
    }
```

По формуле Евклида считается НОК между числами от `1` до `n`

## Проблема Эйлера 26

### Условие

Дробь в числителе имеет 1. Десятичные представления дробей со знаменателями от 2 до 10 выглядят следующим образом:

```text
1/2  = 0.5
1/3  = 0.(3)
1/4  = 0.25
1/5  = 0.2
1/6  = 0.1(6)
1/7  = 0.(142857)
1/8  = 0.125
1/9  = 0.(1)
1/10 = 0.1
```

Числа в `()` обозначают повторяющийся цикл. Необходимо найти число `d` такое, что `d < 1000` и число `1/d` имело самый длинный повторяющийся цикл.

#### Решение с помощью хвостовой рекурсии

Для решения задачи я реализовал вспомогательную функцию `cycleLength n`, которая определяет длину повторяющегося цикла для числа `1/n`.

```haskell
cycleLength :: Integer -> Integer
cycleLength n = case helper 10 [] of
    Just result -> toInteger result
    Nothing -> -1
  where
    helper base rems
        | base `mod` n == 0 = Just 0
        | (base `mod` n) `elem` rems = elemIndex (base `mod` n) (0 : rems)
        | otherwise = helper (10 * (base `mod` n)) ((base `mod` n) : rems)

```

Основная идея заключается в том, чтобы запомнать остатки от деления на `n`. Если в какой-то момент остаток стал равен 0, то повторяющегося цикла нет. Когда какой-то остаток встречается второй раз, то значит произошло зацикливание и длина повторяющегося цикла будет равна разности длины массива остатков и позиции повторившегося остатка

Само решение:

```haskell
euler26Tail :: Integer -> Integer
euler26Tail = helper 0 0
  where
    helper result maxLength n
        | n == 0 = result
        | cycleLength n > maxLength = helper n (cycleLength n) (n - 1)
        | otherwise = helper result maxLength (n - 1)
```

Здесь перебираются длины циклов всех чисел от `1` до `n` и запоминается лучший результат

#### Решение с помощью рекурсии

```haskell
euler26Recursion :: Integer -> Integer
euler26Recursion = snd . helper
  where
    helper 1 = (cycleLength 1, 1)
    helper n = max (cycleLength n, n) (helper (n - 1))
```

Рекурсивно ищется максимальная пара вида `(длина цикла, число)`. Послее этого возвращается второй элемент пары.

#### Решение с помощью свертки

```haskell
euler26Foldr :: Integer -> Integer
euler26Foldr n = snd $ foldr max (-1, 0) [(cycleLength x, x) | x <- [1 .. n]]
```

Генерируется список пар `(длина цикла, число)` и затем делается свертка применяя последовательно `max`

#### Решение с помощью отображения

```haskell
euler26Map :: Integer -> Integer
euler26Map n = snd . maximum $ map (\x -> (cycleLength x, x)) [1 .. n]
```

К каждому элементу массива `[1 .. n]` применяется функция `\x -> (cycleLength x, x)`. Далее из этих пар находится максимальная.

#### Решение с помощью бесконечного списка

```haskell
euler26InfiniteList :: Integer -> Integer
euler26InfiniteList n = snd . maximum $ take (fromIntegral n) (zip ([cycleLength x | x <- [1 ..]]) [1 ..])
```

Два бесконечных массива складываются в один с парами вида `(длина цикла, число)`. Затем из первых `n` элементов находится максимальный.

#### Решение с помощью традиционного языка

```java
public int euler26(Integer n) {
        LinkedList<Integer> remainders = new LinkedList<>();
        int result = 1;
        int maxLen = 0;
        for (int i = 1; i <= n; i++) {
            remainders.clear();
            int base = 10;
            int mod;
            int len = 0;
            do {
                mod = base % i;
                base = mod * 10;
                if (remainders.contains(mod)) break;
                remainders.add(mod);
            } while (mod != 0);
            if (mod != 0) len = remainders.size() - remainders.indexOf(mod);
            if (len > maxLen) {
                result = i;
                maxLen = len;
            }
        }
        return result;
    }
```

Алгоритм нахождения решения идентичен всем выше описанным способам.

## Вывод

Я познакомился с базовым синтаксисом языка Haskell, некоторыми функциями его стандартной библиотеки. Попробовал писать код в идиоматическом для Haskell стиле используюя `.` и `$`. Научился писать код в функциональном стиле используя такие базовые концепции как рекурсия, функции высших порядков и списки
