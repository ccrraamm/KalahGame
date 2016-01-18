module Consecutive where

{-
    Tipuri pentru care unele valori pot fi consecutive, într-un sens dependent
    de context.
-}
class Consecutive a where
    (><) :: a -> a -> Bool
