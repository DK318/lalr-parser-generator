module LALRGeneration.ParserModel where

import qualified Data.Map as M
import qualified Data.Set as S

data Rule = Rule { getLeft  :: String
                 , getRight :: [String]
                 } deriving (Show, Eq, Ord)

data LR1Item = LR1Item { getRule       :: Rule
                       , getPos        :: Int
                       , getLookaheads :: S.Set String
                       } deriving (Show, Eq, Ord)

data LR0Item = LR0Item { getRule0 :: Rule
                       , getPos0  :: Int
                       } deriving (Show, Eq, Ord)

data Edge a = Edge { getFrom :: a
                   , getTo   :: String
                   } deriving (Show, Eq, Ord)

type EdgeNFA = Edge LR1Item

type EdgeDFA = Edge (S.Set LR1Item)

type NFA = M.Map LR1Item (S.Set EdgeNFA)

type DFA = M.Map (S.Set LR1Item) (S.Set EdgeDFA)

