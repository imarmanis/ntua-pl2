import Text.Read
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import Control.Monad.State

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always :: Bool
always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec _ (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . shows e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec _ (Tvar alpha) = ("@" ++) . shows alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . shows tau)

-- Main program

type Constraints = [(Type, Type)]
type Enviroment = (Map.Map String Type)
type Unifier = [(Int, Type)]

newVar :: State Int Type
newVar = Tvar <$> (modify (+1) >> Control.Monad.State.get)

typeInfer :: Expr -> Maybe Type
typeInfer expr =
  let (t, c) = alacurry expr in
  applyUnifier t <$> unify c
  where
    alacurry :: Expr -> (Type, Constraints)
    alacurry ex = evalState (construct ex Map.empty) 0
      where
        construct :: Expr -> Enviroment -> State Int (Type, Constraints)
        construct (Evar s) env = return (env Map.! s, [])
        construct (Eabs s e) env = do
          a <- newVar
          (t, c) <- construct e (Map.insert s a env)
          return (Tfun a t, c)
        construct (Eapp e1 e2) env = do
          (t1, c1) <- construct e1 env
          (t2, c2) <- construct e2 env
          a <- newVar
          return (a, (t1,Tfun t2 a):(c2 ++ c1))

    unify :: Constraints -> Maybe Unifier
    unify [] = Just []
    unify ((t1,t2):c)
      | t1 == t2 = unify c
    unify ((Tvar a, t2):c)
      | not $ varInExpr a t2 = do
        u <- unify (replaceConstraints c (a, t2))
        return $ (a,t2):u
    unify ((t1, Tvar a):c) = unify ((Tvar a, t1):c)
    unify ((Tfun t11 t12, Tfun t21 t22):c) =
      unify((t11,t21):(t12,t22):c)
    unify _ = Nothing

    varInExpr :: Int -> Type -> Bool
    varInExpr a = vIe where
      vIe (Tvar b) = a == b
      vIe (Tfun t1 t2) = vIe t1 || vIe t2

    replaceConstraints :: Constraints -> (Int, Type) -> Constraints
    replaceConstraints c x = fmap rpl c where
      rpl (te1, te2) = (replaceVar te1 x, replaceVar te2 x)

    replaceVar :: Type -> (Int, Type) -> Type
    replaceVar t@(Tvar a) (b, t') = if a == b then t' else t
    replaceVar (Tfun t1 t2) x = Tfun (replaceVar t1 x) (replaceVar t2 x)

    applyUnifier ::  Type -> Unifier -> Type
    applyUnifier = foldl replaceVar

processExpr :: IO ()
processExpr = do
  s <- getLine
  case typeInfer $ read s of
    Just t -> print $ reorder t
    _ -> putStrLn "type error"
  where
    reorder :: Type -> Type
    reorder typ = evalState (rdr typ) (Map.empty, 0)
      where
        rdr :: Type -> State (Map.Map Int Int, Int) Type
        rdr (Tvar a) = do
          (imap, cnt) <- Control.Monad.State.get
          case Map.lookup a imap of
            Just b -> return $ Tvar b
            Nothing -> do
              put (Map.insert a cnt imap, cnt + 1)
              return $ Tvar cnt
        rdr (Tfun t1 t2) = liftM2 Tfun (rdr t1) (rdr t2)

main :: IO [()]
main = readLn >>= flip Monad.replicateM processExpr
