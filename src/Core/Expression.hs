{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Core.Expression where

import GHC.Natural
--import Test.QuickCheck

data Term v where
  Lam     :: v -> Term v -> Term v
  App     :: Term v -> Term v -> Term v
  Var     :: v -> Term v
  Nat     :: Term v
  Add1    :: Term v -> Term v
  Int     :: Term v
  Sub1    :: Term v -> Term v
  NatRec  :: Term v -> Term v -> Term v
  Nil     :: Term v
  Cons    :: Term v -> Term v -> Term v
  ListRec :: Term v -> Term v -> Term v
  Pair    :: Term v -> Term v -> Term v
  Proj1   :: Term v -> Term v
  Proj2   :: Term v -> Term v
  deriving (Eq, Functor, Show)


data Type where
  TyNat    :: Type
  TyInt    :: Type
  TyList   :: Type -> Type
  TyArr    :: Type -> Type -> Type
  TyProd   :: Type -> Type -> Type
  deriving stock (Eq, Show)


-- consider making a context term type and putting it all in a map
data Context v where
  CtxtEmpty  :: Context v
  (:.)       :: Context v -> (v, Type) -> Context v
  



{-
instance Arbitrary a => Arbitrary (Term a) where
  arbitrary =
    do
      i <- choose (0,10)
      buildTerm i
    where
      buildTerm :: Int -> Gen (Term a)
      buildTerm i
        | i <= 2    = arbitrary >>= pure . Var
        | i <= 7    = Lam <$> arbitrary <*> arbitrary <*> arbitrary
        | otherwise = App <$> arbitrary <*> arbitrary
 

-- Note: This might be changed to something actually useful, who is to say.
type Scope f a = f a

data Var a = B Int | F a
  deriving (Eq)

var :: a -> Var a
var = F


freeVars :: (Ord a) => Term a -> Set a
freeVars (Kind _)         = mempty
freeVars (Var a)          = singleton a
freeVars (App l r)        = freeVars l <> freeVars r
freeVars (Lam nm ty expr) = freeVars ty <> nm `delete` freeVars expr
freeVars (Pi  nm ty expr) = freeVars ty <> nm `delete` freeVars expr
freeVars (Sig nm ty expr) = freeVars ty <> nm `delete` freeVars expr


toLocallyNameless :: forall a . (Ord a) => Term a -> Term (Var a)
toLocallyNameless = go mempty
  where
    go :: Map a Int -> Term a -> Term (Var a)
    go env = \case
      Kind k1    -> Kind k1
      v@(Var a)  ->
        case a `lookup` env of
          Just bv -> Var (B bv)
          Nothing -> Var (F a)
      a@(App l r) -> App (go env l) (go env r)
      Lam n t e   ->
        let
          env' = insert n 0 (M.map (+ 1) env)
        in
          Lam (var n) (go env' t) (go env' e)
      Pi n t e    ->
        let
          env' = insert n 0 (M.map (+ 1) env)
        in
          Pi (var n) (go env' t) (go env' e)

fromLocallyNameless :: forall a . (Ord a) => Term (Var a) -> Term a
fromLocallyNameless = go mempty
  where
    go :: Map Int a -> Term (Var a) -> Term a
    go env = \case
      Kind k1 -> Kind k1
      Var v ->
        case v of
          F a  -> Var a
          B bv -> case bv `lookup` env of
            Just name -> Var name
            Nothing   -> error $ "Found bound variable with binding:" <> (show bv)
      App l r -> App (go env l) (go env r)
      Lam n t e ->
        case n of
          B bv -> error $ "Found unnamed variable at binding site" <> (show bv)
          F v  ->
            let
              env' = insert 0 v (mapKeysMonotonic (+ 1) env)
            in
              Lam v (go env' t) (go env' e)
      Pi n t e ->
        case n of
          B bv -> error $ "Found unnamed variable at binding site" <> (show bv)
          F v  ->
            let
              env' = insert 0 v (mapKeysMonotonic (+ 1) env)
            in
              Pi v (go env' t) (go env' e)

-- |
-- Open takes a term with an outer binder and instantiates that binder
-- with a given term. If this term is a variable then this is the usual
-- open operator.
open :: forall a . Term (Var a) -> Scope Term (Var a) -> Term (Var a)
open image = go 0
  where
    go :: Int -> Term (Var a) -> Term (Var a)
    go outer =
      \case
        Kind k1 -> Kind k1
        Var v ->
          case v of
            B bv | bv == outer -> image
                 | otherwise -> Var (B bv)
            F v -> Var (F v)
        App l r -> App (go outer l) (go outer r)
        Lam n t b -> Lam n (go (outer + 1) t) (go (outer + 1) b)
        Pi n t b -> Pi n (go (outer + 1) t) (go (outer + 1) b)

  -- To do: add open for a collection of variables
-- |
-- Close takes a term and a given free variable and converts that to an
-- outer binder. This can be uses to abstract a variable.

  -- To do : use this in the converstion to LN
close :: a -> Term (Var a) -> Scope Term (Var a)
close = undefined


-- |
-- substitute is just a convenient short-hand for open.
substitute :: Term (Var a) -> Scope Term (Var a) -> Term (Var a)
substitute = open


whnfLN :: Term (Var a) -> Term (Var a)
whnfLN term = go term []
  where
    go :: Term (Var a) -> [Term (Var a)] -> Term (Var a)
    go t as =
      case (# t, as #) of
        (# (App l r), args #)
          -> go l (r : args )
        (# (Lam _ _ body) , a:args #)
          -> go (substitute a body) args
        (# lam, args #)
          -> foldl' App lam as          


whnf :: (Ord a) => Term a -> Term a
whnf = fromLocallyNameless . whnfLN . toLocallyNameless


-- |
-- Reduce to normal form using a [WHAT] strategy.
nfLN :: Term (Var a) -> Term (Var a)
nfLN term = go term []
  where
    go :: Term (Var a) -> [Term (Var a)] -> Term (Var a)
    go t as =
      case (# t, as #) of
        (# (App l r), args #)
          -> go l (r : args)
        (# (Lam _ _ body) , a:args #)
          -> go (substitute a body) args
        (# lam, args #)
          -> foldl' App lam (fmap nfLN as)


nf :: (Ord a) => Term a -> Term a
nf = fromLocallyNameless . nfLN . toLocallyNameless


         
          
    
  
        
        
         
        
      
      

--}      
      


