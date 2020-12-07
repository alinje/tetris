--import System.Random


type XC = Integer
type YC = Integer


type Square = (XC,YC)







type Space = Maybe Square

type Row = [Space]

type Board = [Row]

{-}
data Block = Straight [Square] | RightHook [Square] | LeftHook [Square] | RightSlide [Square] | LeftSlide [Square] | Pyramid [Square] | Box [Square] 
    deriving (Show, Eq)
    -}



data Block = Block [Square]
    deriving (Eq)

instance Show Block where
    show = showBlock

showBlock :: Block -> String
showBlock b | b == blockTypes !! 0 = "Straight"
            | b == blockTypes !! 1 = "RightHook"
            | b == blockTypes !! 2 = "LeftHook"
            | b == blockTypes !! 3 = "RightSlide"
            | b == blockTypes !! 4 = "LeftSlide"
            | b == blockTypes !! 5 = "Pyramid"
            | b == blockTypes !! 6 = "Cube"
            | otherwise            = showBlock (rotateRight (shuffleBlockMaxUp (shuffleBlockMaxLeft b)) (-1,-1))

blockTypes = [Block [(0,0), (0,1), (0,2), (0,3)], Block [(0,0), (0,1), (1,1), (2,1)], Block [(0,0), (1,0), (2,0), (0,1)],
              Block [(1,0), (2,0), (0,1), (1,1)], Block [(0,0), (0,1), (1,1), (2,1)], Block [(1,0), (0,1), (1,1), (2,1)],
              Block [(0,0), (1,0), (1,1), (2,1)], Block [(0,0), (0,1), (0,2), (0,3)]]

{-}
blockTypes = [Straight [(0,0), (0,1), (0,2), (0,3)], RightHook [(0,0), (0,1), (1,1), (2,1)], LeftHook [(0,0), (1,0), (2,0), (1,0)],
              RightSlide [(1,0), (2,0), (0,1), (1,1)], LeftSlide [(0,0), (0,1), (1,1), (2,1)], Pyramid [(1,0), (0,1), (1,1), (2,1)],
              Box [(0,0), (1,0), (1,1), (2,1)]]
              -}

exampleFilledRow = [Just (0,1), Just (1,1), Just (2,1), Just (3,1), Just (4,1), Just (5,1), Just (6,1), Just (7,1), Just (8,1), Just (9,1) ]

exampleJustAbove3 = [(3,6), (4,6), (5,6), (6,6)]

exampleFreeAbove3 = [(3,5), (4,5), (5,5), (6,5)]


exampleThreeRowBoard = replicate 7 makeEmptyRow ++ replicate 3 exampleFilledRow

makeEmptyRow :: Row
makeEmptyRow = replicate 10 Nothing

makeEmptyBoard :: Board
makeEmptyBoard = replicate 20 makeEmptyRow

{-}
makeBlock :: StdGen -> (Block, StdGen)
makeBlock g = (blockTypes !! fst rnd, snd rnd)
    where rnd = randomR (0,6) g
-}

checkAnyRow :: Board -> Bool
checkAnyRow b = not (all nCheckRow b)
    where nCheckRow a = not (checkRow a)

-- checks if row is filled
checkRow :: Row -> Bool
checkRow = all checkSpace

-- check if s is present
checkSpace :: Space -> Bool
checkSpace s = not (null s)

shuffleBoard :: Board -> Block -> Board
shuffleBoard b = undefined

-- if space below block is empty, gives block new coord one step down
shuffleBlock :: Board -> Block -> Block
shuffleBlock brd@(b:bs) blck@(Block tpls) | checkBelowBlock brd blck = Block (shuffleBlock' tpls)
                                          | otherwise                = blck

-- helper, shuffles the pieces if okayed by shuffleBlock
shuffleBlock' :: [(Integer, Integer)] -> [(Integer, Integer)]
shuffleBlock' ((x,y):ts) | ts == []  = [(x, y+1)]
                         | otherwise = (x, y+1) : (shuffleBlock' ts)

-- returns true if room for Block to fall another step
checkBelowBlock :: Board -> Block -> Bool
 -- if we are in the y row of the next square in the block and this is the last square, check if the space under the square is clear
checkBelowBlock brd@(ba:bb:bs) blck@(Block ((x,y):ts)) | y == 0 && null ts = not (checkXinRow bb x)            
 -- if we are in the y row of the next square in the block, check if the space under the square is clear and then recursively check the next square
                                               | y == 0            = not (checkXinRow bb x) && (checkBelowBlock brd (Block ts))
-- if not yet in the right row, y-1 and remove ba from board
                                               | otherwise         = checkBelowBlock (bb:bs) (shuffleBlockUp blck)
checkBelowBlock b (Block ((x,y) :ts)) = False
{-}
                                    | y == 0 && null ts = checkXinRow bs x
                                    | y == 0 = checkXinRow bs x && (checkBelowBlock [b,bs] ts)
                                    | otherwise = False   -}


shuffleBlockUp :: Block -> Block
shuffleBlockUp (Block ((x,y):ts)) | null ts   = Block [(x, y-1)]
                                  | otherwise = Block ((x, y-1) : (cB (shuffleBlockUp (Block ts))))


shuffleBlockMaxUp :: Block -> Block
shuffleBlockMaxUp = undefined

shuffleBlockMaxLeft :: Block -> Block
shuffleBlockMaxLeft = undefined

rotateRight :: Block -> (Integer, Integer) -> Block
rotateRight (Block ((x,y):ts)) (bX, bY) | bX == -1  = Block ((x,y):(cB (rotateRight (Block ts) (bX, bY))))
                                        | null ts   = Block [(bX-(y-bY),bY-(x-bX))]
                                        | otherwise = Block ((bX-(y-bY),bY-(x-bX)):cB (rotateRight (Block ts) (bX, bY)))

cB :: Block -> [Square]
cB (Block b) = b

-- checks if it's time to launch a new block
checkIfNewBlock :: Board -> Block -> Bool
checkIfNewBlock brd blck = not (checkBelowBlock brd blck)

gameOver :: Board -> Bool
gameOver = undefined

-- check if space x in row is a square
checkXinRow :: Row -> Integer -> Bool
checkXinRow (r:rs) i | i == 0 = checkSpace r
                     | otherwise = checkXinRow rs (i-1)

scoreCheckBoard :: Board -> Board
scoreCheckBoard (x:xs) | xs == [] && checkRow x = [makeEmptyRow]                          -- if last row is full, replace it for an empty 
                       | xs == []               = [x]                                     -- if last row is not full, return it as it is
                       | checkRow x             = makeEmptyRow : scoreCheckBoard xs     -- if row is full, replace it for an empty and continue iteration
                       | otherwise              = x : scoreCheckBoard xs                -- if row is not full, keep it as it is and continue iteration


oneRowScore = 100
assignRow :: Int -> Int
assignRow i | i == 0    = 100
            | otherwise = i * 2

checkScore :: Board -> Int
checkScore (x:xs) | xs == [] && checkRow x = oneRowScore                          
                  | xs == []               = 0                                    
                  | checkRow x             = assignRow (checkScore xs)
                  | otherwise              = 0 + (checkScore xs)


