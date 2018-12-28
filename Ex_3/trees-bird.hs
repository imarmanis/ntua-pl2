import Test.QuickCheck
import Data.Ratio

main :: IO ()
main = undefined

-- Ex 2

-- 1.
data Tree a = T a [Tree a]
  deriving (Show)

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T a as) = f a $ map (foldTree f) as

-- 2.
sizeTree :: Num b => Tree a -> b
sizeTree = foldTree (\_ as -> 1 + sum as) -- sum (1:as)

heighTree :: (Ord b, Num b) => Tree a -> b
heighTree = foldTree (\_ as -> 1 + maximum (0:as))

sumTree :: Num a => Tree a -> a
sumTree = foldTree (\a as -> a + sum as) -- sum (a:as)

maxTree :: Ord a => Tree a -> a
maxTree = foldTree (\a as -> maximum $ a:as)

inTree :: Eq a => a -> Tree a -> Bool
inTree x = foldTree (\a as -> x == a || or as) -- or ((x==a):as)

nodes :: Tree a -> [a]
nodes = foldTree (\a as -> a : concat as) -- concat ([a]:as)

countTree :: (a -> Bool) -> Tree a -> Integer
countTree f = foldTree (\a as -> sum $ fromBool (f a):as )
  where fromBool = toInteger . fromEnum

leaves :: Tree a -> [a]
leaves = foldTree (\a as -> if null as then [a] else concat as)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (T . f)

-- 3.
trimTree :: Int -> Tree a -> Tree a
trimTree 1 (T a _) = T a []
trimTree n (T a as) = T a $ map (trimTree $ n - 1) as

path :: [Int] -> Tree a -> a
path [] (T a _) = a
path (h:t) (T _ as) = path t $ as !! h

-- Ex 3

-- 1.
instance  Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree
    where tree 0 = arbitrary >>= \t -> return $ T t []
          tree n = do
            let k = round . log . fromIntegral $ n
            m <- choose (0, k)
            x <- arbitrary
            let leaf = arbitrary >>= \t -> return $ T t []
            let c = tree . round $ fromIntegral n / fromIntegral m
            let g = frequency [(4,c), (1,leaf)]
            childs <- vectorOf m g
            return $ T x childs

-- 2.
instance Show (a -> b) where
  show _ = "function"

prop_height t = heighTree t > 0 && heighTree t <= sizeTree t

prop_max t = inTree (maxTree t) t

prop_nodes t = all (`inTree` t) (nodes t)

prop_count f t = countTree f t <= sizeTree t

prop_size t = length (nodes t) == sizeTree t && g (length $ leaves t) (sizeTree t)
  where g 1 1 = True
        g l s = l < s

prop_map f t = all (\g -> g (mapTree f t) == g t) [sizeTree, heighTree]

prop_mapin f n t = elem n (nodes t) ==> elem (f n) (nodes $ mapTree f t)

prop_ax f t = all (\g -> (map f . g) t == (g . mapTree f) t) [nodes, leaves]

-- 3.
bird :: Tree Rational
bird = T 1 [mapTree (\x -> 1/(x+1)) bird, mapTree (\x -> 1/x + 1) bird]

-- 4.
fpath :: [Int] -> Tree a -> [a]
fpath [] (T a _) = [a]
fpath (h:t) (T a as) = a : fpath t (as !! h)

hasBird :: Rational -> Bool
hasBird q = findB bird True
  where
    findB :: Tree Rational -> Bool -> Bool
    findB (T x [l, r]) f
      | q == x = True
      | q < x = findB (if f then l else r) (not f)
      | q > x = findB (if f then r else l) (not f)

prop_path :: [Bool] -> Bool
prop_path l = path l' bird == path l' (trimTree (length l' + 1) bird)
  where l' = map fromEnum l -- [Bool] ~= [Bit]

prop_allnum n = n >=0 ==> fpath (take n zig) bird == [1..(toRational n + 1)]
  where zig = 1:0:zig

prop_fib n = n /= 0 ==> map numerator (fpath (replicate n' 0) bird) == take (n'+1) fib
  where fib = 1:1:zipWith (+) fib (tail fib)
        n' = abs n

prop_bird n = n /= 0 ==> hasBird (abs n)
