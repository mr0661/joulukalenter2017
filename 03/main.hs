-- this is quite ugly, but meh
-- Navigate spiral in long steps
moveR move current target [x,y]
    | move + current >= target = (abs (x + target - current)) + (abs y)
    | otherwise = moveU move (current + move) target [(x + move), y]

moveU move current target [x,y]
    | move + current >= target = (abs x) + (abs (y + target - current))
    | otherwise = moveL (move + 1) (current + move) target [x, (y + move)]
    
moveL move current target [x,y]
    | move + current >= target = (abs (x - (target - current))) + (abs y)
    | otherwise = moveD move (current + move) target [(x - move), y]
    
moveD move current target [x,y]
    | move + current >= target = (abs x) + (abs (y - (target - current)))
    | otherwise = moveR (move + 1) (current + move) target [x, (y - move)]

getResult1 x = moveR 1 0 (x - 1) [0,0]


-- No beauty contest won by this either
contains point [] = False
contains point (x:xs) = point == (tail x) || contains point xs

leftAllowed [x,y] points = (not (contains [x-1,y] points)) && (contains [x,y-1] points)
downAllowed [x,y] points = (not (contains [x,y-1] points)) && (contains [x+1,y] points)
upAllowed   [x,y] points = (not (contains [x,y+1] points)) && (contains [x-1,y] points)

getValue [x,y] [] = 0
getValue point (x:xs)
    | point == (tail x) = head x
    | otherwise = getValue point xs

defineValue [x,y] points = 
    getValue [x-1,y+1] points + getValue [x,y+1] points + getValue [x+1,y+1] points +
    getValue [x-1,y] points   + getValue [x,y] points   + getValue [x+1,y] points +
    getValue [x-1,y-1] points + getValue [x,y-1] points + getValue [x+1,y-1] points
    
definePoint [x,y] points = [(defineValue [x,y] points),x,y]
addPoint [x,y] points = [(definePoint [x,y] points)] ++ points

move [x,y] previousPoints target
    | head (head previousPoints) > target = head (head previousPoints)
    | leftAllowed [x,y] previousPoints = move [x-1,y] ( addPoint [x,y] previousPoints ) target
    | downAllowed [x,y] previousPoints = move [x,y-1] ( addPoint [x,y] previousPoints ) target
    | upAllowed [x,y] previousPoints   = move [x,y+1] ( addPoint [x,y] previousPoints ) target
    --Move right--
    | otherwise = move [x+1,y] ( addPoint [x,y] previousPoints ) target

getResult2 x = move [1,0] [[1,0,0]] x

-- Get results --
main = do
    y <- readFile "input.txt"
    result1 <- return $ getResult1 (read y ::Integer)
    result2 <- return $ getResult2 (read y ::Integer)
    print result1
    print result2 