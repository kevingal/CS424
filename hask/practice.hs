fib :: Int -> Integer
fib 1 = 1
fib n = fibAux n 2 1 1

fibAux n i m mPrevious
    | n == i    = m
    | otherwise = fibAux n (i + 1) (m + mPrevious) m

doubleAll = foldr((:) . (*2)) []