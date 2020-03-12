{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Core.TypeChecker where

import Core.Expression
import Data.Text

-- To do: improve this
newtype TyCheck e a = TyCheck {runTyCheck :: Either e a}
  deriving stock (Show, Eq, Functor)
  deriving newtype (Applicative, Monad)

class ErrorType e where
  report :: String -> e -- 



lookupVar :: (Eq v) => v -> Context v -> TyCheck e Type
lookupVar var = undefined



inferType :: (Eq v) => Context v -> Term v -> TyCheck e Type
inferType ctxt =
  \case
    Var v ->
      do
        lookupVar v ctxt


checkType :: Context v -> Term v -> Type -> TyCheck e ()
checkType ctxt ty term =
  \case
    
  

