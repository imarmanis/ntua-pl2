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
      s_a <- newListArray (0,n-1) s :: ST s (STUArray s Int Char)
      prev <- newArray (0,n) 0 :: ST s (STUArray s Int Int)
      curr <- newArray (0,n-1) 1 :: ST s (STUArray s Int Int)
      next <- newArray (0,n-2) 0 :: ST s (STUArray s Int Int)
      (_, final,_) <- foldM (
        \(pr, cr, nx) d -> do
          forM_ [0..(n-1-d)] $ \i -> do
            c1 <- readArray cr i
            c2 <- readArray cr (i+1)
            c3 <- readArray pr (i+1)
            let j = i + d
            s1 <- readArray s_a i
            s2 <- readArray s_a j
            let acc = if s1 == s2 then c1 `addm` c2 `addm` 1
                      else c1 `addm` c2 `subm` c3
            writeArray nx i acc
          return (cr, nx, pr)
        ) (prev, curr, next) [1..(n-1)]
      readArray final 0
    addm x y = mod (x + y) 20130401
    subm x y = mod (x - y) 20130401
