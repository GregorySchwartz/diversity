-- Diversity
-- By G.W. Schwartz

{- | Takes a fasta file and return the diversity of a certain order and
window length (to split into fragments) by position.
-}

-- Built-in
import Data.List

-- Cabal
import Options.Applicative
import Data.Fasta.String.Parse

-- Local
import Diversity.GenerateDiversity
import Diversity.Print

-- Command line arguments
data Options = Options { inputLabel             :: String
                       , inputOrder             :: Double
                       , inputWindow            :: Int
                       , inputFasta             :: String
                       , fastBin                :: Bool
                       , removeN                :: Bool
                       , wholeSeq               :: Bool
                       , list                   :: Bool
                       , outputRarefaction      :: String
                       , outputRarefactionCurve :: String
                       , output                 :: String
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
      <*> option auto
          ( long "input-order"
         <> short 'r'
         <> metavar "[1]|INT"
         <> value 1
         <> help "The order of true diversity" )
      <*> option auto
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
          ( long "fast-bin"
         <> short 'f'
         <> help "Whether to use a much faster, but approximated, binomial\
                 \ coefficient for the rarefaction analysis" )
      <*> switch
          ( long "remove-N"
         <> short 'n'
         <> help "Remove 'N' and 'n' characters" )
      <*> switch
          ( long "whole-sequence"
         <> short 'a'
         <> help "Ignore window length and only analyze the entire sequence for\
                 \ diversity and rarefaction curves." )
      <*> switch
          ( long "list"
         <> short 'L'
         <> help "Analyze a diversity of species in a list separated by lines\
                 \ instead of a fasta file" )
      <*> strOption
          ( long "output-rarefaction"
         <> short 'O'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the rarefaction values (the percent\
                 \ of the rarefaction curve that is above 95% of the height of\
                 \ the rarefaction curve)" )
      <*> strOption
          ( long "output-rarefaction-curve"
         <> short 'c'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the rarefaction curve" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the diversities at each position" )

generateDiversity :: Options -> IO ()
generateDiversity opts = do
    contentsFile <- readFile . inputFasta $ opts
    let lineIt       = unlines . (:) ">" . intersperse ">" . lines
        contents     = if (list opts) then lineIt contentsFile else contentsFile
        label        = inputLabel opts
        order        = inputOrder opts
        window       = inputWindow opts
        nFlag        = removeN opts
        whole        = wholeSeq opts

        fastaListN   = parseFasta contents
        fastaList    = if nFlag then removeNs fastaListN else fastaListN
        positionMap  = generatePositionMap whole window fastaList

    if (null . output $ opts)
        then return ()
        else writeFile (output opts)
           . printDiversity label order window
           $ positionMap
    if (null . outputRarefaction $ opts)
        then return ()
        else writeFile (outputRarefaction opts) $
            printRarefaction (fastBin opts) label window positionMap
    if (null . outputRarefactionCurve $ opts)
        then return ()
        else writeFile (outputRarefactionCurve opts) $
            printRarefactionCurve (fastBin opts) label window positionMap

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file"
     <> header "Diversity, Gregory W. Schwartz" )
