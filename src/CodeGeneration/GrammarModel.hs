module CodeGeneration.GrammarModel where

data GrammarFile
  = GrammarFile { getStartBlock        :: !String
                , getAttributes        :: !Attributes
                , getIgnoreWhitespaces :: !Bool
                , getParseName         :: !String
                , getStartNonTerm      :: !StartNonTerm
                , getTokenDescriptions :: [TokenDescription]
                , getParserRules       :: [ParserRule]
                } deriving (Show, Eq)

data Attributes
  = Attributes { getAttrName   :: !String
               , getAttrFields :: [AttributeField]
               } deriving (Show, Eq)

data AttributeField
  = AttributeField { getFieldName :: !String
                   , getFieldType :: !String
                   } deriving (Show, Eq)

data StartNonTerm
  = StartNonTerm { getStart :: !String
                 , getField :: !String
                 } deriving (Show, Eq)

data TokenDescription
  = TokenDescription { getTokenName :: !String
                     , getRegex     :: !String
                     } deriving (Show, Eq)

data ParserRule
  = ParserRule { getRuleLeft   :: !String
               , getRuleRights :: [RuleRight]
               } deriving (Show, Eq)

data RuleRight
  = RuleRight { getRuleNames   :: [String]
              , getAttrActions :: [String]
              } deriving (Show, Eq)
