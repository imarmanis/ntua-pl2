import Data.Vector as V
import Data.List as L

main :: IO ()
main = do
  line1 <- getLine
  let n = read line1 :: Int
  s <- getLine
  print (palincount n s)

palincount :: Int -> String -> Int
palincount n s = c_t ! 0 ! (n-1)
  where
    s' = V.fromList s
    count i j
      | i == j = 1
      | i > j  = 0
      | s' ! i == s' ! j =
        ((c_t ! (i+1) ! j) `addm` (c_t ! i ! (j-1))) `addm` 1
      | otherwise =
        ((c_t ! (i+1) !j) `addm` (c_t ! i !(j-1))) `subm` (c_t ! (i+1) !(j-1))
    c_t = V.fromList [fromList (L.map (count i) [0..(n-1)]) | i <- [0..(n-1)]]
    addm x y = (mod (x + y)) 20130401
    subm x y = (mod (x - y)) 20130401
