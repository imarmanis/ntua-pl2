import Data.Array
-- immutable array = pure

main :: IO ()
main = do
  line1 <- getLine
  let n = read line1 :: Int
  s <- getLine
  print (palincount n s)

palincount :: Int -> String -> Int
palincount n s = c_t ! (1,n)
  where
    s' = Data.Array.listArray (1, n) s
    count i j
      | i == j = 1
      | i > j  = 0
      | s' ! i == s' ! j =
        ((c_t ! (i+1,j)) `addm` (c_t ! (i,j-1))) `addm` 1
      | otherwise =
        ((c_t ! (i+1,j)) `addm` (c_t ! (i,j-1))) `subm` (c_t ! (i+1,j-1))
    c_t = Data.Array.listArray bd  [count i j | (i,j) <- Data.Array.range bd]
    bd = ((1,1), (n,n))
    addm x y = (mod $! (x + y)) 20130401
    subm x y = (mod $! (x - y)) 20130401
