{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CodeGeneration.CodeGenerator where

import CodeGeneration.GrammarModel (AttributeField (..), Attributes (getAttrFields, getAttrName),
                                    GrammarFile (..), ParserRule (..),
                                    RuleRight (RuleRight, getRuleNames),
                                    StartNonTerm (getField, getStart),
                                    TokenDescription (TokenDescription, getTokenName))
import Control.Monad.Trans.Reader (runReader)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import LALRGeneration.ParserGenerator (GrammarEnv (GrammarEnv), generateTable, lalrStateToNum,
                                       numToLalrState, startDFA)
import LALRGeneration.ParserModel (LR1Item (LR1Item), Rule (Rule))
import Prettyprinter (Doc, Pretty (pretty), encloseSep, indent, list, sep, tupled, vsep, (<+>))
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

prettySig :: String -> [Doc x] -> Doc x
prettySig str types = pretty str <+> (sep . zipWith (<+>) ("::" : repeat "->")) types

lexerLine :: TokenDescription -> Doc x
lexerLine (TokenDescription token regex) = foldl1 (<+>) [ "| str =~"
                                     , pretty $ show ('^' : regex)
                                     , "= let (_, parsed, ret) = (str =~"
                                     , pretty $ show ('^' : regex)
                                     , ":: (String, String, String)) in"
                                     , "(" <> (pretty $ show token) <> ","
                                     , "parsed, ret)"
                                     ]

lexerSkipWhitespacesLine :: TokenDescription -> Doc x
lexerSkipWhitespacesLine (TokenDescription token regex) = foldl1 (<+>) [ "| str =~ \"^[[:space:]]+\""
                                                                       , "= let (_, parsed, ret) = (str =~"
                                                                       , pretty $ show ('^' : regex)
                                                                       , ":: (String, String, String)) in"
                                                                       , "(" <> (pretty $ show token) <> ","
                                                                       , "parsed, ret)"
                                                                       ]

lexerLines :: [TokenDescription] -> Bool -> Doc x
lexerLines lst spaceIgnored = vsep $ (lexerLine <$> lst)
                                  ++ (if spaceIgnored then ["| str =~ \"^[[:space:]]+\" = let (_, parsed, ret) = (str =~ \"^[[:space:]]+\" :: (String, String, String)) in nextToken ret"] else [""])
                                  ++ ["| otherwise = error \"Unexpected token\""]

generateNextToken :: GrammarFile -> Doc x
generateNextToken file = vsep [ prettySig "nextToken" ["String", "(String, String, String)"]
                              , "nextToken str"
                              , indent 2 (lexerLines (TokenDescription "$" "$" : getTokenDescriptions file) (getIgnoreWhitespaces file))
                              ]

getNonTerms :: GrammarFile -> [String]
getNonTerms file = ((getStart . getStartNonTerm) file ++ "0") : (getRuleLeft <$> getParserRules file)

getTermsAndNonTerms :: GrammarFile -> [String]
getTermsAndNonTerms file =
  let tokens = "$" : (getTokenName <$> getTokenDescriptions file)
  in tokens ++ getNonTerms file

generateTermsAndNonTerms :: GrammarFile -> Doc x
generateTermsAndNonTerms file = vsep [ prettySig "termsAndNonTerms" ["[String]"]
                                     , "termsAndNonTerms =" <+> indent 0 (list ((pretty . show) <$> getTermsAndNonTerms file))
                                     ]

generateNonTerms :: GrammarFile -> Doc x
generateNonTerms file = vsep [ prettySig "nonTerms" ["S.Set String"]
                             , "nonTerms = S.fromList" <+> indent 0 (list ((pretty . show) <$> getNonTerms file))
                             ]

ruleItem :: ParserRule -> Doc x
ruleItem (ParserRule from to) =
  let rights = getRuleNames <$> to
      rightsDoc = list ((\lst -> list ((pretty . show) <$> lst)) <$> rights)
  in tupled [(pretty . show) from, "S.fromList" <+> rightsDoc]

generateRules :: GrammarFile -> Doc x
generateRules file =
  let start = (getStart . getStartNonTerm) file
      startRule = ParserRule (start ++ "0") [RuleRight [start] []]
      mp = "M.fromList" <+> (list $ ruleItem <$> (startRule : getParserRules file))
  in vsep [ prettySig "rules" ["M.Map String (S.Set [String])"]
          , "rules =" <+> indent 0 mp
          ]

generateTerminalRule :: GrammarFile -> Doc x
generateTerminalRule file =
  let start = (getStart . getStartNonTerm) file
  in  vsep [ prettySig "terminalRule" ["Rule"]
           , "terminalRule = Rule" <+> (pretty $ show (start ++ "0")) <+> (pretty $ "[" <> show start <> "]")
           ]

generateAttributes :: forall x. GrammarFile -> Doc x
generateAttributes file =
  let fieldToDoc (AttributeField name typ) = (pretty name) <+> "::" <+> (pretty typ)
      fieldsToDoc :: [AttributeField] -> Doc x
      fieldsToDoc fields = encloseSep "{ " " }" ", " (fieldToDoc <$> fields)
      attrName = getAttrName $ getAttributes file
      attrFields = getAttrFields $ getAttributes file
  in foldl1 (<+>) ["data"
                  , pretty attrName
                  , "="
                  , pretty attrName
                  , fieldsToDoc attrFields
                  ]

generateEmptyAttributes :: GrammarFile -> Doc x
generateEmptyAttributes file =
  let attrName = getAttrName $ getAttributes file
      attrFields = getAttrFields $ getAttributes file
  in vsep [ prettySig "emptyAttrs" [pretty attrName]
          , "emptyAttrs =" <+> (pretty attrName) <+> indent 0 (encloseSep "{ " " }" ", " [(pretty $ getFieldName field) <+> "= undefined" | field <- attrFields])
          ]

preprocessAttrAction :: String -> String
preprocessAttrAction act =
  let matchesAttr = getAllTextMatches (act =~ ("\\$[0-9]+\\.[a-zA-Z]+" :: String)) :: [String]
      replaceToAttr :: String -> T.Text
      replaceToAttr str =
        let (_, _, _, [num, name]) = str =~ ("\\$([0-9]+)\\.([a-zA-Z]+)" :: String) :: (String, String, String, [String])
        in T.pack $ "(" ++ name ++ " $ fst $ attrs !! " ++ num ++ ")"

      replaceFunc :: [String] -> (String -> T.Text) -> T.Text -> T.Text
      replaceFunc lst f = foldl' (.) id ((\t -> T.replace t (f $ T.unpack t)) <$> (T.pack <$> lst))

      replacedAttrs = replaceFunc matchesAttr replaceToAttr (T.pack act)
      matchedVals = getAllTextMatches ((T.unpack replacedAttrs) =~ ("\\$[0-9]+" :: String)) :: [String]
      replaceToVals :: String -> T.Text
      replaceToVals str =
        let (_, _, _, [num]) = str =~ ("\\$([0-9]+)" :: String) :: (String, String, String, [String])
        in T.pack $ "(snd $ attrs !! " ++ num ++ ")"

      replacedVals = replaceFunc matchedVals replaceToVals replacedAttrs
      (left, right) = T.break (== '=') replacedVals
      leftProcessed = T.replace "$$." "" left
      rightMatched = getAllTextMatches ((T.unpack right) =~ ("\\$\\$\\.[a-zA-Z]+" :: String)) :: [String]
      replaceSelf :: String -> T.Text
      replaceSelf str =
        let (_, _, _, [name]) = str =~ ("\\$\\$\\.([a-zA-Z]+)" :: String) :: (String, String, String, [String])
        in T.pack $ "(" ++ name ++ " selfAttrs)"
      rightProcessed = replaceFunc rightMatched replaceSelf right
  in T.unpack $ leftProcessed <> rightProcessed

parserRuleToDoc :: forall x. String -> ParserRule -> Doc x
parserRuleToDoc attrsName (ParserRule from to) =
  let generateLine :: String -> RuleRight -> Doc x
      generateLine from (RuleRight to act) = foldl1 (<+>) [ "reduceWithAttrs (Rule"
                                                          , pretty $ show from
                                                          , indent 0 (list ((pretty . show) <$> to))
                                                          , ") attrs = let selfAttrs ="
                                                          , pretty attrsName
                                                          , encloseSep "{ " " }" ", " ((pretty . preprocessAttrAction) <$> act)
                                                          , "in selfAttrs"
                                                          ]
  in vsep [generateLine from right | right <- to]

generateReduceWithAttrs :: GrammarFile -> Doc x
generateReduceWithAttrs file =
  let attrsName = getAttrName $ getAttributes file
      rules = getParserRules file
      start = getStartNonTerm file
      startRule = ParserRule (getStart start ++ "0") [RuleRight [getStart start] ["$$." ++ getField start ++ " = $0." ++ getField start]]
  in vsep [ prettySig "reduceWithAttrs" ["Rule", "[(Attrs, String)]", "Attrs"]
          , vsep $ parserRuleToDoc attrsName <$> startRule : rules
          , "reduceWithAttrs _ _ = undefined"
          ]

getGrammarEnv :: GrammarFile -> GrammarEnv
getGrammarEnv file =
  let tant = getTermsAndNonTerms file
      nt = S.fromList $ getNonTerms file
      start = getStart $ getStartNonTerm file
      startRule = (start ++ "0", S.singleton [start])
      rls = M.fromList (startRule : [(getRuleLeft rl, S.fromList $ getRuleNames <$> getRuleRights rl) | rl <- getParserRules file])
      termRule = Rule (start ++ "0") [start]
      stNFA = LR1Item termRule 0 (S.singleton "$")
  in GrammarEnv tant nt rls stNFA termRule

generateTableCode :: GrammarFile -> Doc x
generateTableCode file =
  let env = getGrammarEnv file
      vec = V.toList <$> (V.toList $ runReader generateTable env)
  in vsep [ prettySig "table" ["V.Vector (V.Vector Cell)"]
          , "table = V.fromList $ V.fromList <$>" <+> (pretty . show) vec
          ]

generateLalrStateToNum :: GrammarFile -> Doc x
generateLalrStateToNum file =
  let env = getGrammarEnv file
      mp = runReader lalrStateToNum env
      mpShow = M.toList $ M.mapKeys S.toList mp
      mpReplaced = T.replace "fromList" "S.fromList" (T.pack $ show mpShow)
  in vsep [ prettySig "lalrStateToNum" ["M.Map (S.Set LR1Item) Int"]
          , "lalrStateToNum = M.mapKeys S.fromList (M.fromList" <+> pretty mpReplaced <+> ")"
          ]

generateNumToLalrState :: GrammarFile -> Doc x
generateNumToLalrState file =
  let env = getGrammarEnv file
      mp = runReader numToLalrState env
      mpShow = M.toList $ M.map S.toList mp
      mpReplaced = T.replace "fromList" "S.fromList" (T.pack $ show mpShow)
  in vsep [ prettySig "numToLalrState" ["M.Map Int (S.Set LR1Item)"]
          , "numToLalrState = M.map S.fromList (M.fromList" <+> pretty mpReplaced <+> ")"
          ]

generateStartDFA :: GrammarFile -> Doc x
generateStartDFA file =
  let env = getGrammarEnv file
      set = runReader startDFA env
      setShow = S.toList set
      setReplaced = T.replace "fromList" "S.fromList" (T.pack $ show setShow)
  in vsep [ prettySig "startDFA" ["S.Set LR1Item"]
          , "startDFA = S.fromList" <+> pretty setReplaced
          ]

generateParsingFunction :: GrammarFile -> Doc x
generateParsingFunction file =
  let fields = getAttrFields $ getAttributes file
      start = getField $ getStartNonTerm file
      funcName = getParseName file
      [typ] = [getFieldType t | t <- fields, getFieldName t == start]
  in vsep [ prettySig funcName ["String", "Either String" <+> pretty typ]
          , pretty funcName
            <+> "str ="
            <+> pretty start
            <+> "<$> evalState (runExceptT (parseString str)) [(startDFA, (emptyAttrs, \"\"))]"
          ]

generateHeader :: GrammarFile -> Doc x
generateHeader file = vsep [ pretty $ getStartBlock file
                           , "import Control.Monad.Trans.State"
                           , "import Data.Char"
                           , "import qualified Data.Set as S"
                           , "import qualified Data.Map as M"
                           , "import qualified Data.Vector as V"
                           , "import qualified Data.Bifunctor as Bi"
                           , "import Text.Regex.TDFA ((=~))"
                           , "import Control.Monad"
                           , "import Control.Monad.Trans.Class"
                           , "import Control.Monad.Trans.Except"
                           ]

generateParser :: GrammarFile -> Doc x -> Doc x
generateParser file template = vsep [ generateHeader file
                                    , generateNextToken file
                                    , generateTermsAndNonTerms file
                                    , generateNonTerms file
                                    , generateRules file
                                    , generateTerminalRule file
                                    , generateAttributes file
                                    , generateEmptyAttributes file
                                    , generateReduceWithAttrs file
                                    , generateTableCode file
                                    , generateLalrStateToNum file
                                    , generateNumToLalrState file
                                    , generateStartDFA file
                                    , generateParsingFunction file
                                    , template
                                    ]
