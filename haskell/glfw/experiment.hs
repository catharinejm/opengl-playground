import System.Environment
import System.Exit
import System.IO

import Data.List
import Data.Functor.Identity

import Control.Monad.State

data Env = Env { pants :: Bool
               , numLegs :: Int
               }
         deriving Show

initialEnv = Env False 0

parse :: (Monad m) => [String] -> StateT Env m ()
parse [] = return ()
parse (a:as) | Just pants <- stripPrefix "pants=" a = do
                 env <- get
                 put env {pants = (pants == "yes")}
                 parse as
             | Just n <- fmap readInt (stripPrefix "numLegs=" a) = do
                 env <- get
                 put env {numLegs = n}
                 parse as
             | True = parse as
  where
    readInt s | [(n, _)] <- reads s :: [(Int, String)] = n
              | True = 0

mainLoop :: StateT Env IO ()
mainLoop = do
  env <- get
  liftIO $ showPrompt env
  input <- liftIO getLine
  parse [input]
  mainLoop
  where
    showPrompt env = do print env
                        putStr "> "
                        hFlush stdout

mainLoop2 :: StateT Env IO ()
mainLoop2 = get >>= \env ->
                     (liftIO $ showPrompt env) >>
                     (liftIO getLine) >>= \input ->
                                           (parse [input]) >>
                                           mainLoop2
  where
    showPrompt env = (print env) >>
                     (putStr "> ") >>
                     (hFlush stdout)

main :: IO ()
main = do
  args <- getArgs
  evalStateT mainLoop2 $ execState (parse args) initialEnv
