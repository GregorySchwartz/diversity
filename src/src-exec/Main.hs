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
                 \ use quotations around the entry" )
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
    contentsFile    <- if (null . inputFasta $ opts)
                        then getContents
                        else readFile . inputFasta $ opts
    let lineIt       = unlines . (:) ">" . intersperse ">" . lines
        contents     = if (list opts) then lineIt contentsFile else contentsFile
        label        = inputLabel opts
        order        = inputOrder opts
        window       = inputWindow opts
        nFlag        = removeN opts
        whole        = wholeSeq opts
        start        = read . head . words . inputSubsampling $ opts
        interval     = read . last . words . inputSubsampling $ opts

        fastaListN   = parseFasta contents
        fastaList    = if nFlag then removeNs fastaListN else fastaListN
        positionMap  = generatePositionMap
                       (sample opts)
                       (inputSampleField opts)
                       whole
                       window
                       fastaList

    if (null . output $ opts)
        then
            if std opts
                then putStrLn $ printDiversity label order window positionMap
                else return ()
        else writeFile (output opts)
           . printDiversity label order window
           $ positionMap
    if (null . outputRarefaction $ opts)
        then
            if std opts
                then putStrLn
                   $ printRarefaction
                     (sample opts)
                     (fastBin opts)
                     start
                     interval
                     label
                     window
                     positionMap
                else return ()
        else writeFile (outputRarefaction opts)
           $ printRarefaction
             (sample opts)
             (fastBin opts)
             start
             interval
             label
             window
             positionMap
    if (null . outputRarefactionCurve $ opts)
        then
            if std opts
                then putStrLn
                   $ printRarefactionCurve
                     (rarefactionDF opts)
                     (sample opts)
                     (fastBin opts)
                     start
                     interval
                     label
                     window
                     positionMap
                else return ()
        else writeFile (outputRarefactionCurve opts) $
            printRarefactionCurve
            (rarefactionDF opts)
            (sample opts)
            (fastBin opts)
            start
            interval
            label
            window
            positionMap

main :: IO ()
main = execParser opts >>= generateDiversity
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Return the diversity at each position for all sequences in a\
                 \ fasta file"
     <> header "Diversity, Gregory W. Schwartz" )
