-- Diversity
-- By G.W. Schwartz

{- | Takes a fasta file and return the diversity of a certain order and
window length (to split into fragments) by position.
-}

-- Built-in
import Data.List
import Control.Monad (forever, unless)
import qualified Data.Map.Strict as Map
import qualified System.IO as IO

-- Cabal
import Options.Applicative
import Data.Fasta.String
import Pipes
import qualified Pipes.Prelude as P

-- Local
import Math.Diversity.Types
import Math.Diversity.GenerateDiversity
import Math.Diversity.Print

-- Command line arguments
data Options = Options { inputLabel             :: String
                       , inputOrder             :: Double
                       , inputWindow            :: Int
                       , inputFasta             :: String
                       , inputSampleField       :: Int
                       , inputCountField        :: Int
                       , inputSubsampling       :: String
                       , inputG                 :: Double
                       , fastBin                :: Bool
                       , runs                   :: Int
                       , gapsFlag               :: Bool
                       , removeNBool            :: Bool
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
      <*> option auto
          ( long "input-count-field"
         <> short 'C'
         <> metavar "INT"
         <> value 0
         <> help "The index for the number of this type in the header separated\
                 \ by '|' (1 indexed). Used if there are multiple copies\
                 \ of one entry, so a '4' in the header would indicate that\
                 \ this entity occurred 4 times. Defaults to 0, meaning that\
                 \ this field is ignored and count each sequence as occurring\
                 \ just once" )
      <*> strOption
          ( long "input-subsampling"
         <> short 'I'
         <> metavar "INT INT (INT)"
         <> value "1 1"
         <> help "The start point, interval, and optional endpoint of\
                 \ subsamples in the rarefaction curve.\
                 \ For instance, '1 1 4' would be 1, 2, 3, 4\
                 \ '2 6 14' would be 2, 8, 14, ... Note: input is a string so\
                 \ use quotations around the entry and it always includes the\
                 \ number of subsamples overall in the result. Excluding the\
                 \ endpoint results in the number of samples the endpoint, so\
                 \ '1 1' would be 1, 2, 3, ..., N " )
      <*> option auto
          ( long "input-g"
         <> short 'g'
         <> metavar "Double"
         <> value 0.95
         <> help "Used for calculating the number of individuals\
                 \ (or samples) needed before the proportion g of the total\
                 \ number of estimated species is reached.\
                 \ Sobs / Sest < g < 1" )
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
          ( long "keep-gaps"
         <> short 'G'
         <> help "Do not remove '.' and '-' characters from the analysis. This\
                 \ flag will thus treat these characters as additional entities\
                 \ rather than be ignored as artificial biological gaps in\
                 \ a sequence" )
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
                 \ expects a string, so you need a string even with std" )

parseSampling :: (Num a, Read a) => String -> [a]
parseSampling = map read . parsing . words
  where
    parsing [x, y] = [x, y, "0"]
    parsing x      = x

pipesPositionMap :: Options -> IO PositionMap
pipesPositionMap opts = do
    h <- if null . inputFasta $ opts
            then return IO.stdin
            else IO.openFile (inputFasta opts) IO.ReadMode
    runEffect
          $ P.fold (Map.unionWith (Map.unionWith (+))) Map.empty id
          $ P.fromHandle h
        >-> toFastaSequence (list opts) h
        >-> P.map ( generatePositionMap
                    (gapsFlag opts)
                    (sample opts)
                    (inputSampleField opts)
                    (inputCountField opts)
                    (wholeSeq opts)
                    (inputWindow opts)
                  . removals )
  where
    toFastaSequence True _ = P.map ( \x -> FastaSequence { fastaHeader = ""
                                                        , fastaSeq = x } )
    toFastaSequence False h = pipesFasta h
    removals              = if removeNBool opts then removeN else id

generateDiversity :: Options -> IO ()
generateDiversity opts = do
    let contents     = parseFasta
        label        = inputLabel opts
        order        = inputOrder opts
        window       = inputWindow opts
        nFlag        = removeNBool opts
        whole        = wholeSeq opts
        [start, interval, end] = parseSampling . inputSubsampling $ opts
        howToOutput x = if std opts then putStrLn else writeFile x

    positionMap <- pipesPositionMap opts

    unless (null . output $ opts)
         $ howToOutput (output opts)
         . printDiversity label order window
         $ positionMap
    unless (null . outputRarefaction $ opts)
         $ do
            s <- printRarefaction
                 (sample opts)
                 (fastBin opts)
                 (runs opts)
                 start
                 interval
                 end
                 (inputG opts)
                 label
                 window
                 positionMap
            howToOutput (outputRarefaction opts) s
    unless (null . outputRarefactionCurve $ opts)
         $ do
            s <- printRarefactionCurve
                 (rarefactionDF opts)
                 (sample opts)
                 (fastBin opts)
                 (runs opts)
                 start
                 interval
                 end
                 label
                 window
                 positionMap
            howToOutput (outputRarefactionCurve opts) s

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file" )
