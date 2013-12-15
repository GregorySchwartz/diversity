-- Compare Diversity with Mutation Counts
-- By G.W. Schwartz

-- Takes a DW2 fasta file and generates the mutation counts of the clones
-- from each listed germline in the file per position in a dataframe type
-- format. For use with comparing the counts with the diversities generated
-- already.

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
data Options = Options { inputLabel  :: String
                       , inputOrder  :: Double
                       , inputWindow :: Int
                       , inputFasta  :: String
                       , removeN     :: Bool
                       , output      :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "inputLabel"
         <> short 'l'
         <> metavar "LABEL"
         <> value ""
         <> help "The label for this particular dataset" )
      <*> option
          ( long "inputOrder"
         <> short 'r'
         <> metavar "ORDER"
         <> value 1
         <> help "The order of true diversity" )
      <*> option
          ( long "inputWindow"
         <> short 'w'
         <> metavar "WINDOW"
         <> value 1
         <> help "The length of the sliding window for generating fragments" )
      <*> strOption
          ( long "inputFasta"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The fasta file containing the germlines and clones" )
      <*> switch
          ( long "removeN"
         <> short 'n'
         <> help "Remove 'N' and 'n' characters" )
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
    print positionMap

    writeFile (output opts) . printDiversity label order window $ positionMap

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file"
     <> header "Generate Diversity, Gregory W. Schwartz" )
