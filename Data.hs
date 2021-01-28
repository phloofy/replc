module Data(Anon(..),Name(..),E(..)) where

import Data.Char(chr)

newtype Anon = Anon String deriving (Eq,Ord)
newtype Name = Name String deriving (Eq,Ord)

instance Show Anon where show (Anon s) = s
instance Show Name where show (Name s) = s  

data E
  = Var Anon
  | Abs Anon E
  | App E E
  | Nam Name
  | Tmp E E

instance Show E where
  show (Var a)   = show a
  show (Abs a b) = concat [[chr 0x3bb],show a,showBody b]
    where
      showBody e@(Abs a b) = concat [show a,showBody b]
      showBody e           = concat [".",show e]
  show (App f x) = concat [showFun f,showArg x]
    where
      showFun e@(Abs _ _) = concat ["(",show e,")"]
      showFun e           = show e
      showArg e@(Var _)   = show e
      showArg e@(Nam _)   = show e
      showArg e           = concat ["(",show e,")"]
