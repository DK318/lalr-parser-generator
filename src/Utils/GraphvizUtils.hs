module Utils.GraphvizUtils where

import CodeGeneration.CodeGenerator (getGrammarEnv)
import CodeGeneration.GeneratorLexer (scanTokens)
import CodeGeneration.GeneratorParser (parseGrammar)
import Control.Monad.Trans.Reader (runReader)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G
import Data.List (intercalate)
import Data.List.Index (insertAt)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import LALRGeneration.ParserGenerator (generateLALRDFA)
import qualified LALRGeneration.ParserModel as PM

data VertexType = NonTerminal | Terminal deriving (Eq, Show)

type Vertex = (G.Node, (String, VertexType))
type Edge = (G.Node, G.Node, String)

getGraph :: PM.DFA -> ([Vertex], [Edge])
getGraph dfa =
  let nodeToNum = M.fromList $ zip (M.keys dfa) [0..]
      showItem :: PM.LR1Item -> String
      showItem (PM.LR1Item (PM.Rule from to) pos la) = "[" <> from <> " --> " <> unwords (insertAt pos "." ((\str -> if null str then "eps" else str) <$> to)) <> ", {" <> (unwords $ (\str -> if null str then "eps" else str) <$> (S.toList la)) <> "}]"
      isTerminal :: PM.LR1Item -> Bool
      isTerminal (PM.LR1Item (PM.Rule from to) pos la) = pos == length to
      itemToVertex :: S.Set PM.LR1Item -> Vertex
      itemToVertex items =
        let lst = S.toList items
            num = nodeToNum M.! items
            label = intercalate "\n" (showItem <$> lst)
            typ = if any isTerminal lst then Terminal else NonTerminal
        in (num, (label, typ))
      getEdges :: S.Set PM.LR1Item -> [Edge]
      getEdges items =
        let fromNum = nodeToNum M.! items
            edges = S.toList $ dfa M.! items
        in [(fromNum, nodeToNum M.! PM.getFrom edge, PM.getTo edge) | edge <- edges]
      items = M.keys dfa
      vertices = itemToVertex <$> items
      edges = concatMap getEdges items
  in (vertices, edges)

graphParams :: G.GraphvizParams G.Node (String, VertexType) String () (String, VertexType)
graphParams = G.defaultParams {
        G.fmtNode = \(v, (str, vt)) -> case vt of
            NonTerminal -> attributes (G.RGB 0 0 0) str
            Terminal    -> attributes (G.RGB 255 0 0) str,
        G.fmtEdge = \(v1, v2, str) -> attributes (G.RGB 0 0 0) str
        }
    where
        attributes color str = [G.Color $ G.toColorList [color], G.Label $ G.StrLabel $ T.pack str]

showGraph :: FilePath -> IO ()
showGraph path = do
    str <- readFile path
    case parseGrammar $ scanTokens str of
        Left _ -> print "Your grammar file is bad!"
        Right file -> do
            let env = getGrammarEnv file
                dfa = runReader generateLALRDFA env
                (vs, es) = getGraph dfa
                params = graphParams
                dotGraph = G.graphElemsToDot params vs es
                dotText = G.printDotGraph dotGraph
            T.writeFile "sample.dot" dotText
