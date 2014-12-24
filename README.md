Diversity
=========

Gregory W. Schwartz

This program will take a fasta file and find the diversity and rarefaction value
(percent of the rarefaction curve that is above 95% of the curve). The sequences
must be aligned.

Depends on "fasta": https://github.com/GregorySchwartz/fasta.git 

Now on hackage, so you can install with just (need cabal-install and ghc):
```
cabal update
cabal install diversity
```

Find diversity of each position in a collection of sequences:
```
diversity -i input.fasta -o output.csv -w 1 -o 1
```

Find diversity of a list of entities (a text file with an entity per line)
```
diversity -i input.txt -o output.csv -o 1 -L -a
```

```
Diversity, Gregory W. Schwartz

Usage: diversity [-l|--input-label LABEL] [-r|--input-order [1]|INT]
                 [-w|--input-window [1]|INT] [-i|--input-fasta FILE]
                 [-S|--input-sample-field INT] [-I|--input-subsampling INT INT]
                 [-f|--fast-bin] [-n|--remove-N] [-a|--whole-sequence]
                 [-L|--list] [-s|--sample] [-d|--rarefaction-df]
                 [-O|--output-rarefaction FILE]
                 [-c|--output-rarefaction-curve FILE] [-o|--output FILE]
  Return the diversity at each position for all sequences in a fasta file

Available options:
  -h,--help                Show this help text
  -l,--input-label LABEL   The label for this particular dataset (to
                           differentiate the file in batch analyses)
  -r,--input-order [1]|INT The order of true diversity
  -w,--input-window [1]|INT
                           The length of the sliding window for generating
                           fragments
  -i,--input-fasta FILE    The fasta file containing the germlines and clones
  -S,--input-sample-field INT
                           The index for the sample ID in the header separated
                           by '|' (1 indexed)
  -I,--input-subsampling INT INT
                           The start point and interval of subsamples in the
                           rarefaction curve. For instance, '1 1' would be 1, 2,
                           3, ... '2 6' would be 2, 8, 14, ...
  -f,--fast-bin            Whether to use a much faster, but approximated,
                           binomial coefficient for the rarefaction analysis
  -n,--remove-N            Remove 'N' and 'n' characters
  -a,--whole-sequence      Ignore window length and only analyze the entire
                           sequence for diversity and rarefaction curves.
  -L,--list                Analyze a diversity of species in a list separated by
                           lines instead of a fasta file
  -s,--sample              Whether to use sample based rarefaction (requires
                           sample ID field from input-sample-field)
  -d,--rarefaction-df      Whether to output the rarefaction curve as a data
                           frame
  -O,--output-rarefaction FILE
                           The csv file containing the rarefaction values (the
                           percent of the rarefaction curve that is above 95% of
                           the height of the rarefaction curve)
  -c,--output-rarefaction-curve FILE
                           The csv file containing the rarefaction curve
  -o,--output FILE         The csv file containing the diversities at each
                           position
```
