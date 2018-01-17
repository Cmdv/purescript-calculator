module Main where


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.StrMap (alter, toUnfoldable)
import Data.Tuple (Tuple(..))
import Eval (Env, initEnv, eval)
import Node.ReadLine (READLINE)
import Node.SimpleRepl (Repl, runRepl, setPrompt, readLine, putStrLn)
import Parser (parse)
import Syntax (Cmd(..), (:=), Expr)


main ∷ forall e. Eff ( console ∷ CONSOLE, readline ∷ READLINE | e ) Unit
main = runRepl do
  setPrompt "> "
  loop initEnv

loop ∷ forall e. Env → Repl e Unit
loop e = do
  input ← readLine
  case input of
    "quit" → pure unit
    _ → do
      { env, str } <- evalCmd e input
      putStrLn str
      loop env

evalCmd ∷ forall e. Env → String → Repl e { env ∷ Env, str ∷ String }
evalCmd e input = case parse input of
  Left err → pure { env: e, str: show err }
  Right (name := val) → case eval e val of
    Left err → pure { env: e, str: show err }
    Right expr → do
      let env = upsert e name expr
      setPrompt $ pprint env <> "\n> "
      pure { env, str: name <> " defined" }
  Right (Eval expr) → case eval e expr of
    Left err → pure { env: e, str: show err }
    Right exp → pure { env: e, str: "\x1b[34m" <> show exp <> "\x1b[0m" }

pprint ∷ Env → String
pprint e =
  let list = ?a e -- used to be toList ¯\_(ツ)_/¯
      untupled = map (\key val → key <> " := " <> show val) list -- input used to be (Tuple key val)
   in intercalate ", " untupled

upsert ∷ Env → String → Expr → Env
upsert e k v = alter f k e
  where
  f _ = Just v
