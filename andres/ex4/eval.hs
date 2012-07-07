rev :: [a] -> [a]
rev  = foldl (flip (:)) []

rev' :: [a] -> [a]
rev' = foldr (\x r -> r ++ [x]) []

conc :: [a] -> [a] -> [a]
conc xs ys = foldr (:) ys xs

conc' :: [a] -> [a] -> [a]
conc'      = foldl (\k x -> k . (x : )) id

main = print $ rev' [1..1000000]
