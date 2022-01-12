{-# LANGUAGE ScopedTypeVariables #-}
module LALRGeneration.ParserGenerator where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), ask, runReader)
import Control.Monad.Trans.State (State, StateT, execState, execStateT, get, modify)
import qualified Data.Bifunctor as Bi
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import LALRGeneration.ParserModel (DFA, Edge (Edge, getFrom, getTo), EdgeDFA, EdgeNFA,
                                   LR0Item (LR0Item), LR1Item (LR1Item), NFA, Rule (Rule, getRight))
import Debug.Trace

data GrammarEnv = GrammarEnv { termsAndNonTerms :: [String]
                             , nonTerms         :: S.Set String
                             , rules            :: M.Map String (S.Set [String])
                             , startNFA         :: LR1Item
                             , terminalRule     :: Rule
                             }

type EnvT = ReaderT GrammarEnv
type Env = Reader GrammarEnv

ruleToNum :: Env (M.Map Rule Int)
ruleToNum = do
  env <- ask
  return $ M.fromList $ zip (uncurry Rule <$> concatMap (\(from, to) -> [(from, to') | to' <- to]) (Bi.second S.toList <$> M.toList (rules env))) [0..]

numToRule :: Env (M.Map Int Rule)
numToRule = do
  env <- ask
  let lst = uncurry Rule <$> concatMap (\(from, to) -> [(from, to') | to' <- to]) (Bi.second S.toList <$> M.toList (rules env))
      sz = length lst
  return $ M.fromList $ zip [0..sz] lst

startDFA :: Env (S.Set LR1Item)
startDFA = do
  env <- ask
  let start = startNFA env
  nfa <- generateNFA
  return $ squash $ epsClosure (S.singleton start) nfa

letterToNum :: Env (M.Map String Int)
letterToNum = do
  env <- ask
  return $ M.fromList (zip (termsAndNonTerms env) [0..])

type StateST s a = StateT (STRef s a) (ST s)

getFirst :: GrammarEnv -> M.Map String (S.Set String) -> String -> S.Set String
getFirst env f str = if str `S.member` (nonTerms env) then f M.! str else S.singleton str

first' :: forall s. EnvT (StateST s ((M.Map String (S.Set String), Bool))) ()
first' = do
  ref <- lift $ get
  lift $ lift $ modifySTRef ref (Bi.second $ const False)
  env <- ask
  let cycle :: (String, [String]) -> StateST s ((M.Map String (S.Set String), Bool)) ()
      cycle (a, lst) = do
        (m, _) <- lift $ readSTRef ref
        let sets = map (getFirst env m) lst
            sets' = if null sets then [S.singleton ""] else sets
            (fsts, next) = span (S.member "") sets'
            nextFst = if null next then S.empty else head next
            aSet = m M.! a
            alphaSet = foldl1 S.union (nextFst : fsts)
            aUnited = aSet `S.union` alphaSet
            changed = aUnited /= aSet
        mp <- get
        mp' <- lift $ readSTRef mp
        lift $ modifySTRef ref (Bi.first $ M.insert a aUnited)
        when changed $
          lift $ modifySTRef ref (Bi.second $ const True)
      rules' = concatMap (\(a, alpha) -> [(a, alpha') | alpha' <- alpha]) (Bi.second S.toList <$> M.toList (rules env))
  lift $ mapM_ cycle rules'
  (_, changed) <- lift $ lift $ readSTRef ref
  when changed first'

firstMap :: Env (M.Map String (S.Set String))
firstMap = do
  env <- ask
  let fst' = runReaderT first' env
  return $ runST $ do
    ref <- newSTRef (M.fromList $ zip (S.toList (nonTerms env)) (repeat S.empty), True)
    execStateT fst' ref
    (m, _) <- readSTRef ref
    return m

first :: String -> Env (S.Set String)
first str = do
  env <- ask
  fstMap <- firstMap
  return $ if str `S.member` (nonTerms env) then fstMap M.! str else S.singleton str

generateEdges :: LR1Item -> Env (S.Set EdgeNFA)
generateEdges (LR1Item rule pos lookaheads) = do
  env <- ask
  let edges = S.singleton (Edge (LR1Item rule (pos + 1) lookaheads) (getRight rule !! pos))
  if getRight rule !! pos `S.member` (nonTerms env) then do
    let lft = getRight rule !! pos
        next = if pos + 1 == length (getRight rule) then "" else getRight rule !! (pos + 1)
    la' <- first next
    let la = if "" `S.member` la' then lookaheads `S.union` (S.delete "" la') else la'
        items = [LR1Item (Rule lft r) 0 la | r <- S.toList $ (rules env) M.! lft]
    return $ (S.fromList (uncurry Edge <$> zip items (repeat ""))) `S.union` edges
  else return edges

generateNFA' :: LR1Item -> EnvT (State NFA) ()
generateNFA' item@(LR1Item rule pos lookaheads) =
  if pos == (length $ getRight rule) then
    lift $ modify (M.insert item S.empty)
  else do
    env <- ask
    nfa <- lift $ get
    unless (item `M.member` nfa) $ do
      let edges = runReader (generateEdges item) env
      lift $ modify (M.insert item edges)
      mapM_ (generateNFA' . getFrom) edges

generateNFA :: Env NFA
generateNFA = do
  env <- ask
  let nfa = startNFA env
      state = runReaderT (generateNFA' nfa) env
  return $ execState state M.empty

epsClosure :: S.Set LR1Item -> NFA -> S.Set LR1Item
epsClosure nodes nfa =
  let findEpsNeighbours :: LR1Item -> [LR1Item]
      findEpsNeighbours item = map getFrom (filter (null . getTo) (S.toList $ nfa M.! item))

      epsClosure' :: LR1Item -> State (S.Set LR1Item) ()
      epsClosure' item = do
        visited <- get
        unless (item `S.member` visited) $ do
          modify (S.insert item)
          mapM_ epsClosure' (findEpsNeighbours item)
  in foldl' S.union S.empty (map (\item -> execState (epsClosure' item) S.empty) (S.toList nodes))

squash :: S.Set LR1Item -> S.Set LR1Item
squash items =
  let list = S.singleton <$> (S.toList items)
  in foldl' S.union S.empty (map (\item -> foldl1 merge [item' | item' <- list, isSameCores item item']) list)

generateDFANotSquashed :: Env DFA
generateDFANotSquashed = do
  env <- ask
  nfa <- generateNFA
  let getTransition' :: LR1Item -> String -> S.Set LR1Item
      getTransition' item c = S.fromList $ map getFrom (filter ((== c) . getTo) (S.toList $ nfa M.! item))

      getTransition :: S.Set LR1Item -> String -> S.Set LR1Item
      getTransition items c = foldl' S.union S.empty (map (`getTransition'` c) (S.toList items))

      generateDFA' :: S.Set LR1Item -> State DFA ()
      generateDFA' items = do
        dfa <- get
        unless (items `M.member` dfa) $ do
          let edges = filter (not . S.null . getFrom) (uncurry Edge <$> zip (map (\c -> epsClosure (getTransition items c) nfa) (termsAndNonTerms env)) (termsAndNonTerms env))
          modify (M.insert items (S.fromList edges))
          mapM_ (generateDFA' . getFrom) edges
      start = startNFA env
  return $ execState (generateDFA' $ epsClosure (S.singleton start) nfa) M.empty

generateDFA :: Env DFA
generateDFA = do
  env <- ask
  dfaNS <- generateDFANotSquashed
  let nodeToSquashed :: M.Map (S.Set LR1Item) (S.Set LR1Item)
      nodeToSquashed = M.fromList [(item, squash item) | item <- M.keys dfaNS]

      generate :: S.Set LR1Item -> State DFA ()
      generate nodes = do
        visited <- get
        let squashed = nodeToSquashed M.! nodes
        unless (squashed `M.member` visited) $ do
          let edges = S.toList $ dfaNS M.! nodes
              edges' = S.fromList $ map (\edge -> edge { getFrom = nodeToSquashed M.! (getFrom edge) }) edges
          modify (M.insert squashed edges')
          mapM_ (generate . getFrom) edges
      start = startNFA env
  nfa <- generateNFA
  return $ execState (generate $ epsClosure (S.singleton start) nfa) M.empty

isSameCores :: S.Set LR1Item -> S.Set LR1Item -> Bool
isSameCores lhs rhs =
  let core :: S.Set LR1Item -> S.Set LR0Item
      core node = S.fromList $ map (\(LR1Item rule pos _) -> LR0Item rule pos) (S.toList node)
  in core lhs == core rhs

merge :: S.Set LR1Item -> S.Set LR1Item -> S.Set LR1Item
merge lhs rhs = S.fromList $ zipWith (\(LR1Item rule pos la) (LR1Item _ _ la') -> LR1Item rule pos (la `S.union` la')) (S.toList lhs) (S.toList rhs)

lr1itemToMerged :: Env (M.Map (S.Set LR1Item) (S.Set LR1Item))
lr1itemToMerged = do
  dfa <- generateDFA
  let keys = M.keys dfa
      elems = (\item -> foldl1 (\(item, item1) (_, item2) -> (item, item1 `merge` item2)) [(item, item') | item' <- keys, isSameCores item item']) <$> keys
  return $ M.fromList elems

generateLALRDFA :: Env DFA
generateLALRDFA = do
  dfa <- generateDFA
  toMerged <- lr1itemToMerged
  let generateLALRDFA' :: S.Set LR1Item -> State DFA ()
      generateLALRDFA' node = do
        visited <- get
        let node' = toMerged M.! node
        unless (node' `M.member` visited) $ do
          let edges = S.toList $ dfa M.! node
              edges' = S.fromList $ map (\edge -> edge { getFrom = toMerged M.! (getFrom edge) }) edges
          modify (M.insert node' edges')
          mapM_ (generateLALRDFA' . getFrom) edges
  start <- startDFA
  return $ execState (generateLALRDFA' start) M.empty

data Cell
  = Shift !Int
  | Reduce !Int
  | Goto !Int
  | Accept
  | Error
  deriving (Eq, Show)

lalrStateToNum :: Env (M.Map (S.Set LR1Item) Int)
lalrStateToNum = do
  lalrDFA <- generateLALRDFA
  return $ M.fromList $ zip (M.keys lalrDFA) [0..]

numToLalrState :: Env (M.Map Int (S.Set LR1Item))
numToLalrState = do
  lalrDFA <- generateLALRDFA
  let sz = M.size lalrDFA
  return $ M.fromList $ zip [0..sz] (M.keys lalrDFA)

generateRow :: forall s. (S.Set LR1Item, S.Set EdgeDFA) -> EnvT (ST s) (VM.STVector s Cell)
generateRow (items, edges) = do
  env <- ask
  let toNum = runReader letterToNum env
      termRule = terminalRule env
      rToNum = runReader ruleToNum env
      lalrState = runReader lalrStateToNum env
  row <- VM.new (M.size toNum)
  VM.set row Error
  let setReduce :: LR1Item -> ST s ()
      setReduce (LR1Item rule pos lookaheads) =
        when (pos == (length $ getRight rule)) $ do
          let cell = if rule == termRule then Accept else Reduce $ rToNum M.! rule
          mapM_ (\c -> VM.write row (toNum M.! c) cell) (S.toList lookaheads)

      setShiftOrGoto :: EdgeDFA -> ST s ()
      setShiftOrGoto (Edge item c) = do
        let ctor = if c `S.member` (nonTerms env) then Goto else Shift
        VM.write row (toNum M.! c) (ctor $ lalrState M.! item)
  lift $ mapM_ setReduce (S.toList items)
  lift $ mapM_ setShiftOrGoto (S.toList edges)
  return row

generateTable' :: forall s. EnvT (ST s) (VM.STVector s (V.Vector Cell))
generateTable' = do
  env <- ask
  let lalrState = runReader lalrStateToNum env
      lalrDFA = runReader generateLALRDFA env
  vec <- VM.new (M.size lalrState)
  let func :: (S.Set LR1Item, S.Set EdgeDFA) -> EnvT (ST s) ()
      func el@(item, _) = do
        row <- generateRow el
        row' <- V.freeze row
        VM.write vec (lalrState M.! item) row'
  mapM_ func (M.toList lalrDFA)
  return vec

generateTable :: Env (V.Vector (V.Vector Cell))
generateTable = do
  env <- ask
  let vec = runReaderT generateTable' env
  return $ V.create vec
