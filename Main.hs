-- Diversity
-- By G.W. Schwartz

-- Takes a fasta file and return the diversity of a certain order and
-- window length (to split into fragments) by position.

-- Built in
import qualified Data.Map as M

-- Cabal
import Options.Applicative

-- Local
import Types
import FastaParse
import GenerateDiversity
import Print

-- Command line arguments
data Options = Options { inputLabel        :: String
                       , inputOrder        :: Double
                       , inputWindow       :: Int
                       , inputFasta        :: String
                       , removeN           :: Bool
                       , outputRarefaction :: String
                       , output            :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "input-label"
         <> short 'l'
         <> metavar "LABEL"
         <> value ""
         <> help "The label for this particular dataset (to differentiate\
                 \ the file in batch analyses)" )
      <*> option
          ( long "input-order"
         <> short 'r'
         <> metavar "[1]|INT"
         <> value 1
         <> help "The order of true diversity" )
      <*> option
          ( long "input-window"
         <> short 'w'
         <> metavar "[1]|INT"
         <> value 1
         <> help "The length of the sliding window for generating fragments" )
      <*> strOption
          ( long "input-fasta"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The fasta file containing the germlines and clones" )
      <*> switch
          ( long "remove-N"
         <> short 'n'
         <> help "Remove 'N' and 'n' characters" )
      <*> strOption
          ( long "output-rarefaction"
         <> short 'O'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the rarefaction values (the percent\
                 \ of the rarefaction curve that is above 95% of the height of\
                 \ the rarefaction curve)" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the diversities at each position" )

generateDiversity :: Options -> IO ()
generateDiversity opts = do
    contents <- readFile . inputFasta $ opts
    let label        = inputLabel opts
    let order        = inputOrder opts
    let window       = inputWindow opts
    let nFlag        = removeN opts

    let fastaListN   = fastaParser contents
    let fastaList    = if nFlag then removeNs fastaListN else fastaListN
    let positionMap  = generatePositionMap window fastaList

    writeFile (output opts) . printDiversity label order window $ positionMap

    if (null . outputRarefaction $ opts)
        then return ()
        else writeFile (outputRarefaction opts) $
            printRarefaction label window positionMap

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file"
     <> header "Diversity, Gregory W. Schwartz" )
