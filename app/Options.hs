module Options where

import Text.Megaparsec
import Control.Monad.Reader
import Control.Monad.IO.Class


type Parser = Parsec Void Text


data Calculus = Untyped | SimplyTyped

data Options =
  Options
  { Calculus :: Untyped | SimplyTyped
  , NF   :: Bool
  , WHNF :: Bool
  , Term :: Bool
  , Tree :: Bool
  , SKI  :: Bool
  }


data DisplayOpt = NF | WHNF | TERM | TREE | SKI | OPTS

class Display T where
  nf   :: T -> String
  whnf :: T -> String
  term :: T -> String
  tree :: T -> String
  ski  :: T -> String
  opts :: T -> String


parseOpt :: Text -> DisplayOpt
parseOpt =
  choice
    [ try (text ":nf"  ) *> NF
    , try (text ":whnf") *> WHNF
    , try (text ":term") *> TERM
    , try (text ":tree") *> TREE
    , try (text ":ski ") *> SKI
    , try (text ""     ) *> Opts
    ]

parseInput
  :: forall t m . (MonadIO m, Display t)
  => Text
  -> Parser t
  -> ReaderT Options (InputT IO) ()
parseInput text termParser =
  do
    globalOpts <- ask

    opt <- parseOpt
    t   <- termParser



displayOpt :: (Display t) => DisplayOpt -> t -> String
displayOpt opt t =
    case opt of
      NF   -> outputStrLn . nf   $ t
      WHNF -> outputStrLn . whnf $ t
      TERM -> outputStrLn . term $ t
      TREE -> outputStrLn . tree $ t
      SKI  -> outputStrLn . ski  $ t
      OPTS -> displayOptions globalOpts t


displayOptions :: (Display t) => Options -> t -> String
displayOptions = undefined


outputWithSpace :: (a -> String) -> a -> InputT IO ()
outputWithSpace display a =
  do
    outputStrLn $ mempty
    outputStrLn $ display a
    outputStrLn $ mempty
