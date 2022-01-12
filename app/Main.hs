module Main where

import CodeGeneration.CodeGenerator (generateParser)
import CodeGeneration.GeneratorLexer (scanTokens)
import CodeGeneration.GeneratorParser (parseGrammar)
import Commandline.CommandlineParser (Input (Input), inputParser)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))
import Prettyprinter (Pretty (pretty))

main :: IO ()
main = act =<< execParser opts
    where opts = info (inputParser <**> helper)
            (  fullDesc
            <> progDesc "Generate parser from your grammar"
            <> header "LALR parser generator"  )

act :: Input -> IO ()
act (Input from to) = do
    grammar <- readFile from
    case parseGrammar $ scanTokens grammar of
        Left _ -> print "Your grammar file is bad!"
        Right file -> do
            template <- readFile "templates/ParserTemplate"
            let parser = generateParser file (pretty template)
            writeFile to (show parser)
