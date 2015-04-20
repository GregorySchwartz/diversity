-- Diversity
-- By G.W. Schwartz

{- | Takes a fasta file and return the diversity of a certain order and
window length (to split into fragments) by position.
-}

{-# LANGUAGE BangPatterns #-}

-- Built-in
import Data.List
import Control.Monad (forever)
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
                       , inputSubsampling       :: String
                       , fastBin                :: Bool
                       , runs                   :: Int
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

pipesFasta :: (MonadIO m) => IO.Handle -> Pipe String FastaSequence m ()
pipesFasta h = do
    first <- await
    getRest first ""
  where
    getRest x !acc = do
        eof <- liftIO $ IO.hIsEOF h
        if eof
            then yield FastaSequence { fastaHeader = tail x
                                     , fastaSeq    = filter
                                                     (`notElem` "\n\r ")
                                                     acc }
            else do
                y <- await
                if take 1 y == ">"
                    then do
                        yield FastaSequence { fastaHeader = tail x
                                            , fastaSeq    = filter
                                                            (`notElem` "\n\r ")
                                                            acc }
                        getRest y ""
                    else getRest x (acc ++ y)

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
                    (sample opts)
                    (inputSampleField opts)
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
        start        = read . head . words . inputSubsampling $ opts
        interval     = read . last . words . inputSubsampling $ opts
        howToOutput x = if std opts then putStrLn else writeFile x

    positionMap <- pipesPositionMap opts

    if (null . output $ opts)
        then return ()
        else howToOutput (output opts)
           . printDiversity label order window
           $ positionMap
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
                 positionMap
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
                 positionMap
            howToOutput (outputRarefactionCurve opts) s

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file" )
