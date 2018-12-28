import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.STRef
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
      s_a <- newListArray (0,n-1) s :: ST s (STUArray s Int Char)
      prevp <- (newArray (0,n) 0 :: ST s (STUArray s Int Int)) >>= newSTRef
      currp <- (newArray (0,n-1) 1 :: ST s (STUArray s Int Int)) >>= newSTRef
      nextp <- (newArray (0,n-2) 0 :: ST s (STUArray s Int Int)) >>= newSTRef
      forM_ [1..(n-1)] $ \d -> do
        prev <- readSTRef prevp
        curr <- readSTRef currp
        next <- readSTRef nextp
        forM_ [0..(n-1-d)] $ \i -> do
          c1 <- readArray curr i
          c2 <- readArray curr (i+1)
          c3 <- readArray prev (i+1)
          let j = i + d
          s1 <- readArray s_a i
          s2 <- readArray s_a j
          let acc = if s1 == s2 then c1 `addm` c2 `addm` 1
                    else c1 `addm` c2 `subm` c3
          writeArray next i acc
        writeSTRef prevp curr
        writeSTRef currp next
        writeSTRef nextp prev
      next <- readSTRef currp
      readArray next 0
    addm x y = mod (x + y) 20130401
    subm x y = mod (x - y) 20130401
