{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repl where

import Data
import Parser
import Eval

import Control.Monad.Except
import Control.Monad.State
import Data.Char(chr)
import qualified Data.Map as M

type Context = M.Map Name E

newtype Env a = Env { unEnv :: ExceptT String (StateT Context IO) a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadState Context,MonadError String)

runEnv :: Env a -> IO ()
runEnv = (() <$) . flip runStateT M.empty . runExceptT . unEnv

bind :: Name -> E -> Env ()
bind n e = M.insert n e <$> get >>= put

subnames :: E -> Env E
subnames e@(Var _) = return e
subnames (Abs a b) = Abs a <$> subnames b
subnames (App f x) = subnames f >>= (<$> subnames x) . App
subnames (Nam n)   = M.lookup n <$> get >>= errorMaybe "unbound name"

errorMaybe :: String -> Maybe a -> Env a
errorMaybe xs Nothing  = throwError xs
errorMaybe xs (Just x) = return x

repl :: Env ()
repl = catchError go ((>> repl) . liftIO . putStrLn . ("*** Error: " ++))
  where
    go = do
      liftIO $ putStr (chr 0x3bb:"> ")
      input <- parse input <$> liftIO getLine >>= errorMaybe "invalid input"
      case input of
        (Command xs)  -> case xs of
          "q" -> return ()
          _   -> repl
        (Binding n e) -> reduce <$> subnames e >>= bind n >> repl
        (Expr e)      -> reduce <$> subnames e >>= liftIO . print >> repl

main = runEnv repl
