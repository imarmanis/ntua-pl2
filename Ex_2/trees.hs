data Tree a = T a [Tree a]
  deriving (Show)

t1 :: Tree Integer
t1 = T 1 [ T 2 [ T 3 []
                     , T 4 []
                     ]
            , T 5 [ T 6 [] ]
            ]

t2 :: Tree Char
t2 = T 'a' [ T 'b' []
              , T 'c' [ T 'e' []
                         , T 'f' []
                         ]
              , T 'd' []
              ]
-- 1.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T a as) = f a $ map (foldTree f) as

-- 2.
sizeTree :: Num b => Tree a -> b
sizeTree = foldTree (\_ as -> sum $ 1:as)

heighTree :: (Ord b, Num b) => Tree a -> b
heighTree = foldTree (\_ as -> 1 + foldl max 0 as)

sumTree :: Num a => Tree a -> a
sumTree = foldTree (\a as -> sum $ a:as)

maxTree :: Ord a => Tree a -> a
maxTree = foldTree (\a as -> maximum $ a:as)

inTree :: Eq a => a -> Tree a -> Bool
inTree x = foldTree (\a as -> x == a || or as)

nodes :: Tree a -> [a]
nodes = foldTree (\a as -> a : concat as)

countTree :: (a -> Bool) -> Tree a -> Integer
countTree f = foldTree (\a as -> sum $ (if f a then 1 else 0):as )

leaves :: Tree a -> [a]
leaves = foldTree (\a as -> if null as then [a] else concat as)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\a as -> T (f a) as)

-- 3.
trimTree :: Int -> Tree a -> Tree a
trimTree 1 (T a _) = T a []
trimTree n (T a as) = T a $ map (trimTree $ n - 1) as

path :: [Int] -> Tree a -> a
path [] (T a _) = a
path (h:t) (T _ as) = path t $ as !! h
