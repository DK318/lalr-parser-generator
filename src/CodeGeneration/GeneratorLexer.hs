module CodeGeneration.GeneratorLexer where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Trans.State (State, execState, modify)
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.List.Extra (trim)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

data Token
    = TokenLCBracket
    | TokenRCBracket
    | TokenAType
    | TokenAttr
    | TokenSName
    | TokenIgnoreWhitespaces
    | TokenStart
    | TokenPToken
    | TokenDoubleP
    | TokenColon
    | TokenSemicolon
    | TokenStick
    | TokenQuotes
    | TokenEmpty
    | TokenNameToken !String
    | TokenWord !String
    | TokenAttributeAction !String
    | TokenRegex !String
    | TokenHaskellCode !String
    deriving (Eq, Show)

scanHaskellCode :: String -> State [Token] String
scanHaskellCode str
  | str =~ "^{([^{}]|\n)+}" = do
    let (_, parsed, ret) = str =~ "^{([^{}]|\n)+}" :: (String, String, String)
        code = init $ drop 1 parsed
    modify (flip (++) [TokenLCBracket, TokenHaskellCode code, TokenRCBracket])
    return ret
  | otherwise = error "Lexical error"

scanTokenList :: String -> State [Token] String
scanTokenList str
  | str =~ "^%token[[:space:]]*\n(\n|.)*%%" = do
    let all = str =~ "^%token[[:space:]]*\n(\n|.)*%%" :: String
        parsed' = drop (length "%token") all
        parsed = T.pack $ dropWhile isSpace (take (length parsed' - 2) parsed')
        parts = filter (not . null) (T.unpack <$> T.split (== '\n') parsed)
        func str =
          let [x, y] = words str
          in [TokenNameToken x, TokenRegex y]
        (_, _, ret) = str =~ "^%token[[:space:]]*\n(\n|.)*%%" :: (String, String, String)
    modify (flip (++) (TokenPToken : concatMap func parts ++ [TokenDoubleP]))
    return ret
  | otherwise = error "Lexical error"

scanTokenHelper :: String -> String -> Token -> State [Token] String
scanTokenHelper str parsed token = modify (flip (++) [token]) >> (return $ drop (length parsed) str)

parseAttributes :: String -> String -> State [Token] String
parseAttributes str' str = do
  let parsed = init $ drop 1 str
      parts = filter (not . null) (trim . T.unpack . T.replace (T.pack "\n") (T.pack " ") <$> T.split (== ';') (T.pack parsed))
      tokens = intercalate [TokenSemicolon] (map (\str -> [TokenAttributeAction str]) parts)
  modify (flip (++) (TokenLCBracket : tokens ++ [TokenRCBracket]))
  return $ drop (length str) str'

parseTokenWord :: String -> String -> State [Token] String
parseTokenWord str parsed = do
  let word = init $ drop 1 parsed
  modify (flip (++) [TokenQuotes, TokenNameToken word, TokenQuotes])
  return $ drop (length parsed) str

scanToken' :: String -> State [Token] String
scanToken' str
  | str =~ "^{([^{}]|\n)+}" && elem '$' (str =~ "^{([^{}]|\n)+}" :: String) = parseAttributes str (str =~ "^{([^{}]|\n)+}" :: String)
  | "{- empty -}" `isPrefixOf` str = scanTokenHelper str "{- empty -}" TokenEmpty
  | "{" `isPrefixOf` str = scanTokenHelper str "{" TokenLCBracket
  | "}" `isPrefixOf` str = scanTokenHelper str "}" TokenRCBracket
  | "%attributetype" `isPrefixOf` str = scanTokenHelper str "%attributetype" TokenAType
  | "%attribute" `isPrefixOf` str= scanTokenHelper str "%attribute" TokenAttr
  | "%ignorewhitespaces" `isPrefixOf` str = scanTokenHelper str "%ignorewhitespaces" TokenIgnoreWhitespaces
  | "%name" `isPrefixOf` str= scanTokenHelper str "%name" TokenSName
  | "%start" `isPrefixOf` str = scanTokenHelper str "%start" TokenStart
  | ":" `isPrefixOf` str = scanTokenHelper str ":" TokenColon
  | "|" `isPrefixOf` str = scanTokenHelper str "|" TokenStick
  | str =~ "^\"[^\"]+\"" = let parsed = str =~ "^\"[^\"]+\"" :: String in parseTokenWord str parsed
  | str =~ "^[a-zA-Z]+" = let parsed = str =~ "^[a-zA-Z]+" :: String in scanTokenHelper str parsed (TokenWord parsed)
  | null str = return ""
  | otherwise = error "Lexical error"

scanToken :: String -> State [Token] String
scanToken = scanToken' . skipWhitespaces

skipWhitespaces :: String -> String
skipWhitespaces = dropWhile (liftA2 (||) isSpace (== '\n'))

scanTokens' :: String -> State [Token] String
scanTokens' str = do
  str' <- scanHaskellCode str
  let loop :: String -> State [Token] String
      loop str
        | "%token" `isPrefixOf` (skipWhitespaces str) = return str
        | otherwise = scanToken str >>= loop
  str'' <- loop str'
  str''' <- scanTokenList str''
  let loop2 :: String -> State [Token] String
      loop2 str
        | null $ skipWhitespaces str = return ""
        | otherwise = scanToken str >>= loop2
  loop2 str'''

scanTokens :: String -> [Token]
scanTokens str = execState (scanTokens' $ trim str) []
