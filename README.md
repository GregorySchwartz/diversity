Diversity
=========

Gregory W. Schwartz

This program will take a fasta file and find the diversity and rarefaction value
(percent of the rarefaction curve that is above 95% of the curve). The sequences
must be aligned.

To install:
```
cabal configure
cabal build
cabal install
```

```
Diversity, Gregory W. Schwartz

Usage: diversity [-l|--input-label LABEL] [-r|--input-order [1]|INT] [-w|--input-window [1]|INT] [-i|--input-fasta FILE] [-n|--remove-N] [-O|--output-rarefaction FILE] [-o|--output FILE]
  Return the diversity at each position for all sequences in a fasta file

Available options:
  -h,--help                Show this help text
  -l,--input-label LABEL   The label for this particular dataset (to differentiate the file in batch analyses)
  -r,--input-order [1]|INT The order of true diversity
  -w,--input-window [1]|INT The length of the sliding window for generating fragments
  -i,--input-fasta FILE    The fasta file containing the germlines and clones
  -n,--remove-N            Remove 'N' and 'n' characters
  -O,--output-rarefaction FILE The csv file containing the rarefaction values (the percent of the rarefaction curve that is above 95% of the height of the rarefaction curve)
  -o,--output FILE         The csv file containing the diversities at each position
```
