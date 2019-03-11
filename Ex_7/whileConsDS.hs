import Data.Char
import Text.Read
import Text.Read.Lex
import Control.Monad.Fix

-- Syntax

type Var = String

data C = Cskip | Cassign Var E | Cseq C C | Cfor E C | Cif E C C | Cwhile E C
data E = Ezero | Esucc E | Epred E | Eif E E E | Evar Var
       | Etrue | Efalse | Elt E E | Eeq E E | Enot E
       | Econs E E | Ehd E | Etl E

-- semantic domains

data D = DInt Integer | DBool Bool | DCons (D, D)
  deriving Eq

type S = Var -> D

-- semantic functions

semC :: C -> S -> S
semC Cskip s = s
semC (Cassign x e) s = update s x (semE e s)
semC (Cseq c1 c2) s = semC c2 (semC c1 s)
semC (Cfor e c) s = expon (semC c) n s
  where DInt n = semE e s
semC (Cif e c1 c2) s
    | b = semC c1 s
    | otherwise = semC c2 s
  where DBool b = semE e s
semC (Cwhile e c) s = fix bigF s
  where bigF f s'
            | b = f (semC c s')
            | otherwise = s'
          where DBool b = semE e s'

semE :: E -> S -> D
semE Ezero _ = DInt 0
semE (Esucc e) s = DInt $ n + 1
  where DInt n = semE e s
semE (Epred e) s = DInt $ n - 1
  where DInt n = semE e s
semE (Eif e e1 e2) s
    | b = semE e1 s
    | otherwise = semE e2 s
  where DBool b = semE e s
semE (Evar x) s = s x
semE Etrue _ = DBool True
semE Efalse _ = DBool False
semE (Elt e1 e2) s = DBool (n1 < n2)
  where DInt n1 = semE e1 s
        DInt n2 = semE e2 s
semE (Eeq e1 e2) s = DBool (n1 == n2)
  where DInt n1 = semE e1 s
        DInt n2 = semE e2 s
semE (Enot e) s = DBool (not b)
  where DBool b = semE e s
semE (Econs e1 e2) s = DCons (x1, x2)
  where x1 = semE e1 s
        x2 = semE e2 s
semE (Ehd e) s = hd
  where DCons (hd, _) = semE e s
semE (Etl e) s = tl
  where DCons (_, tl) = semE e s

-- auxiliary functions

expon :: (Num t, Eq t) => (b -> b) -> t -> b -> b
expon _ 0 = id
expon f n = f . expon f (n-1)

update :: S -> Var -> D -> S
update s x n y
  | x == y = n
  | otherwise = s y

-- Pretty-printing

instance Show E where
  showsPrec _ Ezero = ("0" ++)
  showsPrec _ (Esucc n) = ("succ " ++) . showsPrec 2 n
  showsPrec _ (Epred n) = ("pred " ++) . showsPrec 2 n
  showsPrec p (Eif e e1 e2) =
    showParen (p > 0) $
    ("if " ++) . showsPrec 1 e . (" then " ++) . showsPrec 1 e1 .
                                 (" else " ++) . shows e2
  showsPrec _ (Evar x) = (x ++)
  showsPrec _ Etrue = ("true" ++)
  showsPrec _ Efalse = ("false" ++)
  showsPrec p (Elt e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" < " ++) . showsPrec 2 e2
  showsPrec p (Eeq e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" = " ++) . showsPrec 2 e2
  showsPrec p (Enot e) =
    showParen (p > 2) $
    ("not " ++) . showsPrec 2 e
  showsPrec p (Econs e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" : " ++) . showsPrec 1 e2
  showsPrec p (Ehd e) =
    showParen (p > 2) $
    ("hd " ++) . showsPrec 2 e
  showsPrec p (Etl e) =
    showParen (p > 2) $
    ("tl " ++) . showsPrec 2 e

instance Show C where
  showsPrec _ Cskip = ("skip" ++)
  showsPrec _ (Cassign x e) = (x ++) . (" := " ++) . shows e
  showsPrec p (Cseq c1 c2) =
    showParen (p > 0) $
    showsPrec 1 c1 . ("; " ++) . shows c2
  showsPrec p (Cfor e c) =
    showParen (p > 1) $
    ("for " ++) . showsPrec 1 e . (" do " ++) . showsPrec 1 c
  showsPrec p (Cif e c1 c2) =
    showParen (p > 1) $
    ("if " ++) . showsPrec 1 e . (" then " ++) . showsPrec 2 c1 .
                                 (" else " ++) . showsPrec 1 c2
  showsPrec p (Cwhile e c) =
    showParen (p > 1) $
    ("while " ++) . showsPrec 1 e . (" do " ++) . showsPrec 1 c

-- Parsing

isVar :: String -> Bool
isVar x = all isAlpha x && notElem x keywords
  where keywords = ["zero", "succ", "true", "not", "skip",
                    "for", "if", "then", "else", "while", "do",
                    "true", "false", "hd", "tl"]

when :: Monad m => Bool -> m a -> m a
when True p = p
when False _ = fail "when failed"

instance Read E where
  readPrec = parens $
             prec 1 ( do
                e1 <- step readPrec
                Symbol ":" <- lexP
                e2 <- readPrec
                return (Econs e1 e2)) <++
             prec 1 ( do
                e1 <- step readPrec
                Symbol "<" <- lexP
                e2 <- step readPrec
                return (Elt e1 e2)) <++
             prec 1 ( do
                e1 <- step readPrec
                Symbol "=" <- lexP
                e2 <- step readPrec
                return (Eeq e1 e2)) <++
             (do
                Number n <- lexP
                when (numberToInteger n == Just 0) $
                  return Ezero) <++
             prec 2 ( do
                Ident "succ" <- lexP
                e <- readPrec
                return (Esucc e)) <++
             prec 2 ( do
                Ident "pred" <- lexP
                e <- readPrec
                return (Epred e)) <++
             prec 0 ( do
                Ident "if" <- lexP
                e <- step readPrec
                Ident "then" <- lexP
                e1 <- step readPrec
                Ident "else" <- lexP
                e2 <- readPrec
                return (Eif e e1 e2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $
                  return (Evar x)) <++
             (do
                Ident "true" <- lexP
                return Etrue) <++
             (do
                Ident "false" <- lexP
                return Efalse) <++
             prec 2 ( do
                Ident "not" <- lexP
                e <- readPrec
                return (Enot e)) <++
             prec 2 ( do
                Ident "hd" <- lexP
                e <- readPrec
                return (Ehd e)) <++
             prec 2 ( do
                Ident "tl" <- lexP
                e <- readPrec
                return (Etl e))

instance Read C where
  readPrec = parens $
             prec 0 ( do
                c1 <- step readPrec
                Punc ";" <- lexP
                c2 <- readPrec
                return (Cseq c1 c2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol ":=" <- lexP
                  e <- reset readPrec
                  return (Cassign x e)) <++
             (do
                Ident "skip" <- lexP
                return Cskip) <++
             prec 1 ( do
                Ident "if" <- lexP
                e <- readPrec
                Ident "then" <- lexP
                c1 <- step readPrec
                Ident "else" <- lexP
                c2 <- readPrec
                return (Cif e c1 c2)) <++
             prec 1 ( do
                Ident "for" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cfor e c)) <++
             prec 1 ( do
                Ident "while" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cwhile e c))

instance Show D where
  show (DInt n) = show n
  show (DBool True) = "true"
  show (DBool False) = "false"
  show (DCons (hd, tl)) = show hd ++ " : " ++ show tl

-- Main

main :: IO ()
main = getContents >>= run . read
  where run c = print $ semC c s0 "result"
        s0 x = error ("not initialized variable " ++ x)
