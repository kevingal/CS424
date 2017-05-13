-- 2015A, q2
-- (still have to reverse)
revCount :: [a] -> [Int] -> [a]
revCount [] _ = []
revCount (x:xs) (0:ns) = revCount xs ns
revCount xa@(x:xs) (n:ns) = x : revCount xa ((n-1) : ns)

-- 2015J, q2
afterFilter :: (a -> Bool) -> [a] -> [a]
afterFilter _ []  = []
afterFilter _ [x] = []
afterFilter p (x1:x2:xs)
    | p x1      = x2 : afterFilter p (x2:xs)
    | otherwise = afterFilter p (x2:xs)
    
-- 2014A, q2
mySort :: (a -> a -> Bool) -> [a] -> [a]
mySort p xs = mySortAux p xs []

mySortAux p [] sxs      = sxs
mySortAux p (x:xs) sxs  = mySortAux p xs (insert p x sxs)

insert :: (a -> a -> Bool) -> a -> [a] -> [a]
insert p y [] = [y]
insert p y (x:xs)
    | p y x     = (y:x:xs)
    | otherwise = x : insert p y xs

-- 2014J, q2
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f xs = mapEveryOtherAux f xs True

mapEveryOtherAux _ [] _         = []
mapEveryOtherAux f (x:xs) False = x : mapEveryOtherAux f xs True
mapEveryOtherAux f (x:xs) True  = (f x) : mapEveryOtherAux f xs False