{-# OPTIONS_GHC -Wall #-}

-- http://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html
module Golf where

-- Exercise 1
-- ==========
-- Hopscotch

skips :: [a] -> [[a]]
skips xs = map (extractEvery xs) [1..(length xs)]
				where extractEvery x n = map snd . filter ((==n) . fst) $ zip (cycle [1..n]) x

-- Exercise 2
-- ==========
-- Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs) 
	| and [x1 < x2, x3 < x2] = x2 : (localMaxima (x3:xs))
	| otherwise = localMaxima (x2:x3:xs) -- further speed up ?
localMaxima _ = []

-- Exercise 3
-- ==========
-- Histogram
-- putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]

histogramList :: [Integer] -> [Integer]
histogramList xs = map (histogramN xs) [0..9]
	where histogramN l n = fromIntegral $ length $ filter (==n) l

histogram :: [Integer] -> String
histogram x = toStr xs (maximum xs) ++ "==========\n" ++ ['0'..'9'] ++ "\n"
				where 	
					xs = histogramList x				
					toStr _ 0 = []
					toStr s i = map (id1 i) s ++ ['\n'] ++ toStr s (i-1)
					id1 n a 
						| a >=n = '*'
						| otherwise = ' '
