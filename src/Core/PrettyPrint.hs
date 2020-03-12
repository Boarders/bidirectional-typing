{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Untyped.PrettyPrint
  ( printExpr
  , printAsTree
  , printBlock
  )
  where

import Data.Foldable
import Data.Text (unwords, Text, unpack)
import Prelude hiding (unwords, replicate)
import Data.Tree

import Untyped.Expression

printBlock :: Block -> Text
printBlock (name, expr) =
  fold
    [ (id name)
    , " = "
    , printExpr 0 expr
    ]


printExpr :: Int -> Expression -> Text
printExpr _ =
    \case
      Var v -> v
--      Let l body -> error $ "to do" <> show l <> show body
      App l r -> unwords ["(" <>printExpr 0 l, printExpr 0 r <> ")"]
      Lam name body ->
        let
          (lams, rest) = collectLambdas body [name]
          printNames :: [Text] -> Text
          printNames = unwords . reverse
        in
        unwords ["λ", printNames lams, "→ ", printExpr 0 rest]
  where
    collectLambdas :: Expression -> [Text] -> ([Text], Expression)
    collectLambdas (Lam name' body') names = collectLambdas body' (name' : names)
    collectLambdas e               names   = (names, e)


toTree :: Expression -> Tree String
toTree = unfoldTree go
  where
    go :: Expression -> (String, [Expression])
    go =
      \case
        Var v -> (unpack v, [])
        App l r  -> ("┐", [l, r])
        Lam name body -> ("λ " <> unpack name, [body])
 --       Let _ _ -> error "to do"


printAsTree :: Expression -> String
printAsTree = drawTree . toTree
