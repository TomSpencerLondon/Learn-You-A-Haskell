doubleMe x = x + x

doubleUs x y = doubleMe 2 + doubleMe 2

doubleSmallNumber x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, conan O'Brien!"

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

double (x) = x * 2

qsort_reverse :: Ord a => [a] -> [a]
qsort_reverse [] = []
qsort_reverse (x:xs) = qsort_reverse larger ++ [x] ++ qsort_reverse smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]



