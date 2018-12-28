import Data.Array.ST
import Control.Monad.ST
import Control.Monad
-- STUArray impure
main :: IO ()
main = do
  line1 <- getLine
  let n = read line1 :: Int
  s <- getLine
  print (palincount n s)
  where
    palincount :: Int -> String -> Int
    palincount 1 _ = 1
    palincount n s = runST $ do
      prev <- newArray (0,n) 0 :: ST s (STUArray s Int Int)
      curr <- newArray (0,n-1) 1 :: ST s (STUArray s Int Int)
      next <- newArray (0,n-2) 0 :: ST s (STUArray s Int Int)
      outer prev curr next 1
      where
        sr = newListArray (0,n-1) s :: ST s (STUArray s Int Char)
        outer :: STUArray s Int Int -> STUArray s Int Int ->
                  STUArray s Int Int -> Int -> ST s Int
        outer pr cr nxt d
          | d == n  = readArray cr 0
          | otherwise = do
            inner pr cr nxt d
            outer cr nxt pr (d + 1)

        inner :: STUArray s Int Int -> STUArray s Int Int ->
                  STUArray s Int Int -> Int -> ST s ()
        inner p c nx dg =
          forM_ [0..(n-1-dg)] $ \i -> do
            s_a <- sr
            c1 <- readArray c i
            c2 <- readArray c (i+1)
            c3 <- readArray p (i+1)
            let j = i + dg
            s1 <- readArray s_a i
            s2 <- readArray s_a j
            let acc = if s1 == s2 then c1 `addm` c2 `addm` 1
                      else c1 `addm` c2 `subm` c3
            writeArray nx i acc
        addm x y = mod (x + y) 20130401
        subm x y = mod (x - y) 20130401
