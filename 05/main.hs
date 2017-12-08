import Data.List.Split
import Data.Sequence
import Data.Foldable

parse x =  map read (splitOn "\n" x) ::[Int]

getNumber x y = fromIntegral(index x y + y)

runThrough1 step location offsets
    | location < 0 = step
    | location >= Data.Sequence.length offsets = step
    | otherwise = runThrough1 (step + 1) 
                             (getNumber offsets location)
                             (update location (index offsets location +1) offsets )
    

newValue offsets location 
    | value >= 3 = value - 1
    | otherwise = value + 1
    where value = index offsets location

runThrough2 step location offsets
    | location < 0 = step
    | location >= Data.Sequence.length offsets = step
    | otherwise = runThrough2 (step + 1) 
                             (getNumber offsets location)
                             (update location (newValue offsets location) offsets )


getResult1 x = runThrough1 0 0 (fromList(parse x))
getResult2 x = runThrough2 0 0 (fromList(parse x))

main = do
    y <- readFile "input.txt"
    result1 <- return $ getResult1 y
    result2 <- return $ getResult2 y
    print result1
    print result2
