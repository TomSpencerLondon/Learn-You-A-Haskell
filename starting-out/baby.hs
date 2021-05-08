doubleMe x = x + x

doubleUs x y = doubleMe 2 + doubleMe 2

doubleSmallNumber x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, conan O'Brien!"

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]



