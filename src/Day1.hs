adjust :: Int -> Int
adjust x = x `div` 3 - 2

part1 :: IO ()
part1 = do
    content <- readFile "resources/day1.txt"
    let numbers = fmap read (lines content)
    let total = sum $ fmap adjust numbers
    putStrLn $ "Solution: " ++ (show total)

adjustSeries :: Int -> [Int]
adjustSeries x = let newX = adjust x in
                 if newX <= 0 then [] else newX : adjustSeries newX

part2 :: IO()
part2 = do
    content <- readFile "resources/day1.txt"
    let numbers = fmap read (lines content)
    let total = sum $ fmap (sum . adjustSeries) numbers
    putStrLn $ "Solution: " ++ (show total)
