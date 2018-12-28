import Data.Vector.Unboxed as V
main :: IO ()
main = do
    line1 <- getLine
    let n = read line1 :: Int
    s <- getLine
    print (palincount n s)
        where
            palincount :: Int -> String -> Int
            palincount 1 _ = 1
            palincount n s = hcount 1 (Prelude.replicate n 1) (Prelude.replicate (n+1) 0)
                where
                    s_a = V.fromList  s
                    hcount :: Int -> [Int] -> [Int] -> Int
                    hcount _ [r] [_,_] = r
                    hcount x cur (_:prev) = new `seq` hcount (x+1) new cur
                        where
                            new = dcount 0 x cur prev
                            dcount :: Int -> Int -> [Int] -> [Int] -> [Int]
                            dcount _ _ [_] [_] = []
                            dcount i j (d1:d1':d1s) (d2:d2s) =
                                let h = if s_a ! i == s_a ! j
                                          then d1 `addm` d1' `addm` 1
                                          else d1 `addm` d1' `subm` d2
                                    t = dcount (i + 1) (j + 1) (d1':d1s) d2s
                                in h `seq` t `seq` (h:t)
                            addm a b = mod (a + b) 20130401
                            subm a b = mod (a - b) 20130401
