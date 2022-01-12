{
module CodeGeneration.GeneratorParser where

import CodeGeneration.GeneratorLexer
import CodeGeneration.GrammarModel
}

%name parseGrammar
%tokentype { Token }
%error { parseError }
%monad { Either [Token] } { >>= } { return }

%token
    '{'                         { TokenLCBracket }
    '}'                         { TokenRCBracket }
    '%attributetype'            { TokenAType }
    '%attribute'                { TokenAttr }
    '%ignorewhitespaces'        { TokenIgnoreWhitespaces }
    '%name'                     { TokenSName }
    '%start'                    { TokenStart }
    '%token'                    { TokenPToken }
    '%%'                        { TokenDoubleP }
    ':'                         { TokenColon }
    ';'                         { TokenSemicolon }
    '|'                         { TokenStick }
    '"'                         { TokenQuotes }
    'eps'                       { TokenEmpty }
    TOKENNAME                   { TokenNameToken $$ }
    WORD                        { TokenWord $$ }
    ATTRIBUTEACTION             { TokenAttributeAction $$ }
    REGEX                       { TokenRegex $$ }
    HASKELLCODE                 { TokenHaskellCode $$ }
%%

File
    : StartBlock Attributes IgnoreWhitespaces ParseName StartNonTermAndField Lexer Grammar { GrammarFile $1 $2 $3 $4 $5 $6 $7 }

StartBlock
    : '{' HASKELLCODE '}' { $2 }

Attributes
    : AttributeName AttributeFields { Attributes $1 $2 }

AttributeName
    : '%attributetype' '{' WORD '}' { $3 }

AttributeFields
    : '%attribute' WORD '{' WORD '}' AttributeFieldsC { (AttributeField $2 $4) : $6 }

AttributeFieldsC
    : '%attribute' WORD '{' WORD '}' AttributeFieldsC { (AttributeField $2 $4) : $6 }
    | {- empty -}                                     { [] }

IgnoreWhitespaces
    : '%ignorewhitespaces' { True }
    | {- empty -}          { False }

ParseName
    : '%name' WORD { $2 }

StartNonTermAndField
    : '%start' WORD WORD { StartNonTerm $2 $3 }

Lexer
    : '%token' TokenDescription '%%' { $2 }

TokenDescription
    : TOKENNAME REGEX TokenDescriptionC { (TokenDescription $1 $2) : $3 }

TokenDescriptionC
    : TOKENNAME REGEX TokenDescriptionC { (TokenDescription $1 $2) : $3 }
    | {- empty -}                       { [] }

Grammar
    : Rule GrammarC { $1 : $2 }

GrammarC
    : Rule GrammarC { $1 : $2 }
    | {- empty -}   { [] }

Rule
    : WORD ':' RuleRights { ParserRule $1 $3 }

RuleRights
    : Names AttributeAction RuleRightsC { (RuleRight $1 $2) : $3 }

RuleRightsC
    : '|' Names AttributeAction RuleRightsC { (RuleRight $2 $3) : $4 }
    | {- empty -}                           { [] }

Names
    : Name NamesC { $1 : $2 }
    | 'eps' { [] }

NamesC
    : Name NamesC { $1 : $2 }
    | {- empty -} { [] }

Name
    : WORD              { $1 }
    | '"' TOKENNAME '"' { $2 }

AttributeAction
    : '{' Actions '}' { $2 }

Actions
    : ATTRIBUTEACTION ActionsC { $1 : $2 }

ActionsC
    : ';' ATTRIBUTEACTION ActionsC { $2 : $3 }
    | {- empty -}              { [] }

{
parseError = Left
}