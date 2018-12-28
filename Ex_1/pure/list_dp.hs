main :: IO ()
main = do
  line1 <- getLine
  let n = read line1 :: Int
  s <- getLine
  print (palincount n s)
    where
      palincount :: Int -> String -> Int
      palincount 1 _ = 1
      palincount n s = hcount (tail s)
                       (replicate n 1) (replicate (n+1) 0)
      -- |d1| = |d2| + 1
        where
          hcount :: String -> [Int] -> [Int] -> Int
          hcount [] [r] [_,_] = r
          hcount (hs:ts) cd1 (_:d2t) = nd1 `seq` hcount ts nd1 cd1
            where
              nd1 = dcount s (hs:ts) cd1 d2t
              -- |d1| = |d2|
              dcount :: String -> String -> [Int] -> [Int] -> [Int]
              dcount _ [] [_] [_] = []
              dcount (si:sis) (sj:sjs) (d1:d1':d1s) (d2:d2s) =
                let
                  h = if si == sj
                      then d1 `addm` d1' `addm` 1
                      else d1 `addm` d1' `subm` d2
                  t = dcount sis sjs (d1':d1s) d2s
                in h `seq` t `seq` (h:t)
              addm x y = mod (x + y) 20130401
              subm x y = mod (x - y) 20130401
