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

newtype Constraints = C [(Type, Type)]
newtype Enviroment = E (Map.Map String Type)
newtype Unifier = U (Map.Map Int Type)

newVar :: State Int Type
newVar = Tvar <$> (modify (+1) >> Control.Monad.State.get)

typeInfer :: Expr -> Maybe Type
typeInfer expr =
  let (t, c) = alacurry expr in
  flip applyUnifier t <$> unify c
  where
    alacurry :: Expr -> (Type, Constraints)
    alacurry ex = evalState (construct ex $ E Map.empty) 0
      where
        construct :: Expr -> Enviroment -> State Int (Type, Constraints)
        construct (Evar s) (E env) = return (env Map.! s, C [])
        construct (Eabs s e) (E env) = do
          a <- newVar
          (t, c) <- construct e (E $ Map.insert s a env)
          return (Tfun a t, c)
        construct (Eapp e1 e2) (E env) = do
          (t1, C c1) <- construct e1 (E env)
          (t2, C c2) <- construct e2 (E env)
          a <- newVar
          return (a, C $ (t1,Tfun t2 a):(c2 ++ c1))

    unify :: Constraints -> Maybe Unifier
    unify (C []) = Just (U Map.empty)
    unify (C ((t1,t2):c))
      | t1 == t2 = unify (C c)
    unify (C ((Tvar a, t2):c))
      | not $ varInExpr a t2 = do
        u <- unify (replace a t2 (C c))
        return $ compose u (a,t2)
    unify (C ((t1, Tvar a):c)) = unify (C ((Tvar a, t1):c))
    unify (C ((Tfun t11 t12, Tfun t21 t22):c)) =
      unify(C $ (t11,t21):(t12,t22):c)
    unify _ = Nothing

    compose :: Unifier -> (Int, Type) -> Unifier
    compose (U u) (k, t) =
      let new = applyUnifier (U u) t
      in U (Map.insert k new u)

    varInExpr :: Int -> Type -> Bool
    varInExpr a = vIe where
      vIe (Tvar b) = a == b
      vIe (Tfun t1 t2) = vIe t1 || vIe t2

    replace :: Int -> Type -> Constraints -> Constraints
    replace a  t (C c) = C (fmap  rpl c) where
      rpl (te1, te2) = (h te1, h te2) where
        h t'@(Tvar b) = if a == b then t else t'
        h (Tfun t1 t2) = Tfun (h t1) (h t2)

    applyUnifier ::  Unifier -> Type -> Type
    applyUnifier (U u) = repl where
      repl def@(Tvar a) = Map.findWithDefault def a u
      repl (Tfun t1 t2) = Tfun (repl t1) (repl t2)

processExpr :: IO ()
processExpr = do
  s <- getLine
  let e = read s :: Expr
  let mt = typeInfer e
  case mt of
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
        rdr (Tfun t1 t2) = do
          t1' <- rdr t1
          t2' <- rdr t2
          return (Tfun t1' t2')

main :: IO [()]
main = do
  n <- readLn
  Monad.replicateM n processExpr
