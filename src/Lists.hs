module Lists where

import Data.List

-- https://en.wikipedia.org/wiki/Pythagorean_triple
-- List is ordered by x,y,z in ascending order, contains also primitive triples
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(x,y,z) | z <- [1..], y <- [1..z], x <- [1..y], z^2 == x^2 + y^2]

-- https://en.wikipedia.org/wiki/Identity_matrix
-- Note: sublists are rows
eyeMatrix :: Num a => Int -> [[a]]
eyeMatrix n
       | n == 0 = [[]]
       | otherwise = [[ if k == x then 1 else 0 | x <- [1.. toInteger n], let k=y ]| y <- [1.. toInteger n]]


-- https://en.wikipedia.org/wiki/Matrix_multiplication
-- Note: sublists are rows
-- if wrong sizes, raise error "Incorrect matrix sizes"
matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication x y
         | length (x !! 0) /= length y = error "Incorrect matrix sizes"
         | otherwise = [[ sum (zipWith (*) a b) | b <- (transpose y) ] | a <- x ]
