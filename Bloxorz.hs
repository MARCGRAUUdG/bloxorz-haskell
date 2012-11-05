{-# LANGUAGE MultiWayIf #-}

import qualified Data.Set as Set
import Control.Arrow (first)
import Data.List (findIndex, nubBy)
import Data.Function (on)

type Pos = (Int, Int)
type Terrain = Pos -> Bool
type Path = (Block, [Move])

data Block = Block Pos Pos deriving (Show, Read, Eq, Ord)

data Move = L | R | U | D deriving (Show, Read, Eq)

createBlock p1 p2
    | p1 <= p2 = Block p1 p2
    | otherwise = error $ "Invalid positions of block: pos1=" ++ show p1 ++ ", pos2=" ++ show p2

move :: Block -> Move -> Block
move b@(Block (x,y) (x',y')) m =
    case m of
        L -> if | isStanding b -> dy (-2) (-1)
                | x == x'      -> dy (-1) (-2)
                | otherwise    -> dy (-1) (-1)

        R -> if | isStanding b -> dy 1 2
                | x == x'      -> dy 2 1
                | otherwise    -> dy 1 1

        U -> if | isStanding b -> dx (-2) (-1)
                | x == x'      -> dx (-1) (-1)
                | otherwise    -> dx (-1) (-2)

        D -> if | isStanding b -> dx 1 2
                | x == x'      -> dx 1 1
                | otherwise    -> dx 2 1

    where dx mx mx' = createBlock (x+mx,y) (x'+mx',y')
          dy my my' = createBlock (x,y+my) (x',y'+my')

isStanding :: Block -> Bool
isStanding (Block p1 p2) = p1 == p2

neighbors :: Terrain -> Block -> [Move] -> [Path]
neighbors t b ms = filter legalNeigh $ map moves [L, R, U, D]
    where 
        legalNeigh (b, _) = isLegal t b
        moves m = (move b m, m:ms)

        isLegal :: Terrain -> Block -> Bool
        isLegal t (Block p1 p2) = t p1 && t p2

pathsFrom :: Terrain -> Block -> [Path]
pathsFrom t b = from [(b, [])] (Set.fromList [b])
    where
        from :: [Path] -> Set.Set Block -> [Path] -- require improving in neighborsOfPaths
        from []    _       = []
        from paths visited = paths ++ from neighborsOfPaths nowVisited
            where
                neighborsOfPaths = nubBy ((==) `on` fst) $
                                          [ neigOfB
                                          | (b, ms) <- paths
                                          , neigOfB <- neighbors t b ms
                                          , dontVisited neigOfB]

                dontVisited :: Path -> Bool
                dontVisited (b,_) = not $ b `Set.member` visited

                nowVisited = foldr Set.insert visited (map fst neighborsOfPaths)


terrainFromString :: String -> Terrain
terrainFromString str (x,y)
    | x < 0 || y < 0 || x >= maxX || y >= maxY = False
    | otherwise = (t !! x) !! y /= '-'
     where
        t = lines str
        maxX = length $ t
        maxY = length $ head t


solution :: Terrain -> Block -> Block -> [Move]
solution t start finish =
    case dropWhile (\(b,_)->finish/=b) (pathsFrom t start) of
        []         -> []
        ((b,ms):_) -> reverse ms

solutionFromString :: String -> [Move]
solutionFromString str = solution (terrainFromString str) (Block start start) (Block finish finish)
    where 
        ts = lines str
        start  = search 'S' 0 ts
        finish = search 'T' 0 ts

        search :: Char -> Int -> [String] -> (Int, Int)
        search c n (t:ts) = 
            case findIndex (==c) t of
                Nothing -> search c (n+1) ts
                Just m  -> (n,m)

strLevel0 =
        "------\n\
        \--ST--\n\
        \--oo--\n\
        \--oo--\n\
        \------"

level0 = terrainFromString strLevel0

strLevel1 = 
        "ooo-------\n\
        \oSoooo----\n\
        \ooooooooo-\n\
        \-ooooooooo\n\
        \-----ooToo\n\
        \------ooo-"

level1 = terrainFromString strLevel1

main = print $ solutionFromString strLevel1
