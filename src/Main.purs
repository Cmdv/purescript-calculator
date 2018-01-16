module Main where


import Prelude
import Eval (Env, initEnv, eval)
import Parser (parse)
import Syntax (Cmd(..), (:=), Expr)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.StrMap (alter)
import Data.Tuple (Tuple(..))

import Node.SimpleRepl (Repl, runRepl, setPrompt, readLine, putStrLn)
import Node.ReadLine (READLINE)


main :: forall e. Eff ( console :: CONSOLE, readline :: READLINE | e ) Unit
main = runRepl do
  setPrompt "> "
  loop initEnv

loop :: forall e. Env -> Repl e Unit
loop e = do
  input <- readLine
  case input of
    "quit" -> pure unit
    _ -> do
      { env, str } <- evalCmd env input
      putStrLn str
      loop env
