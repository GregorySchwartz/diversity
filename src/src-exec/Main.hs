-- Diversity
-- By G.W. Schwartz

{- | Takes a fasta file and return the diversity of a certain order and
window length (to split into fragments) by position.
-}

-- Built-in
import Data.List

-- Cabal
import Options.Applicative
import Data.Fasta.String

-- Local
import Math.Diversity.GenerateDiversity
import Math.Diversity.Print

-- Command line arguments
data Options = Options { inputLabel             :: String
                       , inputOrder             :: Double
                       , inputWindow            :: Int
                       , inputFasta             :: String
                       , inputSampleField       :: Int
                       , inputSubsampling       :: String
                       , fastBin                :: Bool
                       , runs                   :: Int
                       , removeN                :: Bool
                       , wholeSeq               :: Bool
                       , list                   :: Bool
                       , sample                 :: Bool
                       , rarefactionDF          :: Bool
                       , std                    :: Bool
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
      <*> option auto
          ( long "input-sample-field"
         <> short 'S'
         <> metavar "INT"
         <> value 1
         <> help "The index for the sample ID in the header separated by '|'\
                 \ (1 indexed)" )
      <*> strOption
          ( long "input-subsampling"
         <> short 'I'
         <> metavar "INT INT"
         <> value "1 1"
         <> help "The start point and interval of subsamples in the\
                 \ rarefaction curve. For instance, '1 1' would be 1, 2, 3, ...\
                 \ '2 6' would be 2, 8, 14, ... Note: input is a string so\
                 \ use quotations around the entry and it always has the\
                 \ number of subsamples overall as the last point" )
      <*> switch
          ( long "fast-bin"
         <> short 'f'
         <> help "Whether to use a much faster, but approximated, binomial\
                 \ coefficient for the rarefaction analysis. This method\
                 \ results in NaNs for larger numbers, so in that case you\
                 \ you should use the slower, more accurate default method" )
      <*> option auto
          ( long "input-runs"
         <> short 'R'
         <> metavar "INT"
         <> value 0
         <> help "The number of runs for empirical resampling rarefaction.\
                 \ This method does not compute the theoretical, it reports the\
                 \ actual median and median absolute deviation (MAD) values of\
                 \ this many runs. If this value is not 0, empirical\
                 \ rarefaction is automatically enabled (individual based only,\
                 \ not for sample based)" )
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
      <*> switch
          ( long "sample"
         <> short 's'
         <> help "Whether to use sample based rarefaction (requires sample ID\
                 \ field from input-sample-field)" )
      <*> switch
          ( long "rarefaction-df"
         <> short 'd'
         <> help "Whether to output the rarefaction curve as a data frame" )
      <*> switch
          ( long "std"
         <> short 't'
         <> help "Whether to output to stdout or to a file if no file is\
                 \ supplied" )
      <*> strOption
          ( long "output-rarefaction"
         <> short 'O'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the rarefaction values (the percent\
                 \ of the rarefaction curve that is above 95% of the height of\
                 \ the rarefaction curve). Expects a string, so you need a\
                 \ string even with std" )
      <*> strOption
          ( long "output-rarefaction-curve"
         <> short 'c'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the rarefaction curve. Expects a\
                 \ a string, so you need a string even with std" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The csv file containing the diversities at each position.\
                 \ expects a string, so you need a string wven with std" )

generateDiversity :: Options -> IO ()
generateDiversity opts = do
    contentsFile    <- if null . inputFasta $ opts
                        then getContents
                        else readFile . inputFasta $ opts
    let contents     = if list opts
                        then map ( \x -> FastaSequence { fastaHeader = ""
                                                       , fastaSeq    = x } )
                             . lines
                        else parseFasta
        label        = inputLabel opts
        order        = inputOrder opts
        window       = inputWindow opts
        nFlag        = removeN opts
        whole        = wholeSeq opts
        start        = read . head . words . inputSubsampling $ opts
        interval     = read . last . words . inputSubsampling $ opts

        fastaList    = if nFlag then removeNs . contents else contents
        positionMap  = generatePositionMap
                       (sample opts)
                       (inputSampleField opts)
                       whole
                       window
                     . fastaList

        howToOutput x = if std opts then putStrLn else writeFile x

    if (null . output $ opts)
        then return ()
        else howToOutput (output opts)
           . printDiversity label order window
           . positionMap
           $ contentsFile
    if (null . outputRarefaction $ opts)
        then return ()
        else do
            s <- printRarefaction
                 (sample opts)
                 (fastBin opts)
                 (runs opts)
                 start
                 interval
                 label
                 window
               . positionMap
               $ contentsFile
            howToOutput (outputRarefaction opts) s
    if (null . outputRarefactionCurve $ opts)
        then return ()
        else do
            s <- printRarefactionCurve
                 (rarefactionDF opts)
                 (sample opts)
                 (fastBin opts)
                 (runs opts)
                 start
                 interval
                 label
                 window
               . positionMap
               $ contentsFile
            howToOutput (outputRarefactionCurve opts) s

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file" )
