module Eval(reduce) where

import Data
import Parser

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

freevars :: E -> S.Set Anon
freevars = go S.empty
  where
    go s (Var a)   = if S.member a s then S.empty else S.singleton a
    go s (Abs a b) = go (S.insert a s) b
    go s (App f x) = S.union (go s f) (go s x)

genanon :: Anon -> S.Set Anon -> Anon
genanon (Anon a) s = fromJust $ find (not . flip S.member s) as
  where 
    as = map (Anon . (a ++)) $ scanl (flip (:)) "\'" (repeat '\'')

convert :: Anon -> Anon -> E -> E
convert a r e = go e
  where
    go e@(Var a') = if a == a' then Var r        else e
    go (Abs a' b) = if a == a' then Abs r (go b) else Abs a (go b)
    go (App f x)  = App (go f) (go x)    

substitute :: Anon -> E -> E -> E
substitute a (Var a') e | a == a' = e
substitute a r e = go $ if S.member a s then convert a (genanon a s) e else e
  where
    go e@(Var a')     = if a == a' then r else e
    go e@(Abs a' b)
      | a == a'       = e
      | S.member a' s = let (Abs a b) = convert a' (genanon a' s) e in Abs a (go b)
      | otherwise     = Abs a' (go b)
    go e@(App f x)    = App (go f) (go x)    
    s = freevars r

reduce :: E -> E
reduce e@(Var _) = e
reduce (Abs a b) = Abs a (reduce b)
reduce (App f x) = case reduce f of
  (Abs a b) -> reduce $ substitute a x b
  _         -> App f (reduce x)
