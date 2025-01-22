qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

isort :: Ord a => [a] -> [a]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert x isort (xs)

insert :: (Ord a, Eq a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x:y:ys else y : insert x:ys

bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort [x] = [x]
bsort xs = if null (bubble xs) then xs else bsort (bubble xs) 

swap :: [a] -> [a] 
swap (x:y:zs) | x > y = y:x:zs | otherwise = x:y:zs
swap _ = error ""

bubble :: [a] -> [a] 
bubble [] = []
bubble [x] = [x]
bubble (x:y:zs) -> if x <= y then x : bubble (y:zs) else swap (x:y:zs) ++ bubble (tail (swap (x:y:zs))) 

shuffle :: (Ord a, Int a) => a -> [a] -> [a]
shuffle 0 xs = xs
shuffle n (x:xs) = shuffle (n-1) (riffle xs)

riffle :: Ord a => [a] -> [a]
riffle [] = []
riffle [x] = [x]
riffle xs = interleave splitAt (length xs `div` 2) xs

interleave :: [a] -> [b] -> [a, b]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) ys = x : interleave ys xs

