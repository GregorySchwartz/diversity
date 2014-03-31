Diversity
=========

Gregory W. Schwartz

```
Diversity, Gregory W. Schwartz

Usage: diversity [-l|--input-label LABEL] [-r|--input-order [1]|INT] [-w|--input-window [1]|INT] [-i|--input-fasta FILE] [-n|--remove-N] [-O|--output-rarefaction FILE] [-o|--output FILE]
  Return the diversity at each position for all sequences in a fasta file

Available options:
  -h,--help                Show this help text
  -l,--input-label LABEL   The label for this particular dataset
  -r,--input-order [1]|INT The order of true diversity
  -w,--input-window [1]|INT The length of the sliding window for generating fragments
  -i,--input-fasta FILE    The fasta file containing the germlines and clones
  -n,--remove-N            Remove 'N' and 'n' characters
  -O,--output-rarefaction FILE The csv file containing the rarefaction curves at each position
  -o,--output FILE         The csv file containing the diversities at each position
```
