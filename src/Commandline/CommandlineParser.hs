module Commandline.CommandlineParser where

import Options.Applicative (Parser, argument, help, long, metavar, short, str, strOption)

data Input = Input { getGrammarFilepath :: !FilePath
                   , getOutputFilepath  :: !FilePath
                   }

inputParser :: Parser Input
inputParser = Input
           <$> argument str
               (  help "Path to input grammar"
               <> metavar "INPUTDIR"  )
           <*> strOption
               (  long "output"
               <> short 'o'
               <> metavar "OUTPUTDIR"
               <> help "Path to output file"  )
