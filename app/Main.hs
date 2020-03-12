module Main where

import System.Console.Haskeline
import Data.Foldable
import System.Console.ANSI
import Control.Monad.IO.Class
import CoC.Expression
import Text.Megaparsec
import Data.Text (Text, unpack)
import Data.Void
import Data.String (fromString)



main :: IO ()
main = runInputT pigeSettings repl
   where
       repl :: InputT IO ()
       repl = header >> mainLoop

       header :: InputT IO ()
       header =
         do
           outputStrLn ""
           welcomeMessage
           drawBirdie
           quitInfo
           
       
       mainLoop :: InputT IO ()
       mainLoop = do
           minput <- getInputLine "\x1F426  "
           case minput of
               Nothing -> return ()
               Just ":quit" -> return ()
               Just ":q"    -> return ()
--               Just input ->
--                   case parseInput input of
--                     Left bun ->
--                       do
--                         outputStrLn "Parse error: "
--                         outputStrLn (errorBundlePretty bun)
--                         mainLoop
--                     Right expr ->
--                       do
--                         expressionOutput expr
--                         mainLoop
--

--parseInput :: String -> Either (ParseErrorBundle Text Void) Expression
--parseInput inp = parse parseExpr "" (fromString inp)


pigeSettings :: MonadIO m => Settings m
pigeSettings = Settings
  { complete       = completeFilename
  , historyFile    = Just ".pige-playground"
  , autoAddHistory = True
  }


outputWithSpace :: String -> InputT IO ()
outputWithSpace str =
  do
    outputStrLn $ mempty
    outputStrLn $ str
    outputStrLn $ mempty

{-
expressionOutput :: Expression -> InputT IO ()
expressionOutput e
  = traverse_ outputWithSpace (outputContent e)


outputContent :: Expression -> [String]
outputContent e =
  [ "Lambda Expression is: "
  , unpack $ printExpr 0 e
  , "The Tree representation is: "
  , printAsTree e
  , "The SKI term is: "
  , printSKI (toSKIRep e)
  ]
-}

drawBirdie :: InputT IO ()
drawBirdie =
  do
    liftIO $ setSGR [SetColor Foreground Vivid Blue]
    liftIO $ putStrLn birdie
    liftIO $ setSGR [Reset]  -- Reset to default colour scheme
    liftIO $ putStrLn ""


birdie :: String
birdie = unlines
  [ "                               .-''-. "
  , "                              / ,    \\"
  , "                          .-'`(o)    ;"
  , "                         '-==.       |"
  , "                              `._...-;-."
  , "                               )--\"\"\"   `-."
  , "                              /   .        `-."
  , "                             /   /      `.    `-."
  , "                             |   \\    ;   \\      `-._________"
  , "                             |    \\    `.`.;          -------`."
  , "                              \\    `-.   \\\\          `---...|"
  , "                               `.     `-. ```\\.--'._   `---...|"
  , "                                 `-.....7`-.))\\     `-._`-.. /"
  , "                                   `._\\ /   `-`         `-.,'"
  , "                                     / /"
  , "                                    /=(_"
  , "                                 -./--' `"
  , "                               ,^-(_"
  , "                               ,--' `"
  ]


welcomeMessage :: InputT IO ()
welcomeMessage =
  do
    liftIO $ setSGR [SetColor Foreground Vivid Red]
    liftIO $ putStrLn (replicate 28 ' ' <> "Ahoy sailor!")
    liftIO $ setSGR [Reset]  -- Reset to default colour scheme
    liftIO $ putStrLn ""


quitInfo :: InputT IO ()
quitInfo =
  do
    liftIO $ setSGR [SetColor Foreground Vivid Green]
    liftIO $ putStrLn "Type :quit or :q to exit!"
    liftIO $ setSGR [Reset]  -- Reset to default colour scheme
    liftIO $ putStrLn ""

