Diversity
=========

Gregory W. Schwartz

This program will take a fasta file and find the diversity and rarefaction value
(percent of the rarefaction curve that is above 95% of the curve). The sequences
must be aligned.

Depends on "fasta": https://github.com/GregorySchwartz/fasta.git 

## Citation

**Please cite this paper if you are using this program**

[Conserved variation: identifying patterns of stability and variability in BCR
and TCR V genes with different diversity and richness metrics](http://www.ncbi.nlm.nih.gov/pubmed/23735612)

## Installation

We should always be using sandboxes with cabal, so let's make a directory for
the binary and create a sandbox:

```
mkdir diversity
cd diversity
cabal sandbox init
```

Now we can easily update cabal and install the package!

```
cabal update
cabal install diversity
```

The binary will be found at ./.cabal-sandbox/bin/diversity

## Quick Tutorial

Let's say we have a bunch of made up sequences,

```
>1
AAT
>2
AAT
>3
A-G
>4
--G
>5
TCG
>6
TTG
>7
TAG
>8
GGG
```

that we want to know the diversity of at each position. We can put those
sequences in a file `input.fasta` and check the diversity at each position:

```
diversity -i input.fasta -o output.csv -w 1 -o 1
```

We can see in `output.csv` that each position has a diversity associated with
it. This diversity is calculated using order 1 in this case with a window length
of 1, meaning that at each position we are looking at a single character, in
this case a nucleotide. Gaps are ignored at each position.

Now, what if we wanted to find the diversity using a sliding window of two
nucleotides? That is, position 1 would analyze `AA AA AG N/A TC TT TA GG`, where
gaps are artificial and so we skip over them and we don't start on a gap. We can
achieve this by setting the window length to 2:

```
diversity -i input.fasta -o output.csv -w 2 -r 1
```

The output is similar, but now there are only positions 1 and 2 as we cannot
start at 3 if there is no fragment of length 2 or more. If we wanted to look at
the entire sequence, we could forego the `-w` flag and just use `a` to treat the
entire sequence as a single fragment. This analysis would be useful for just
finding the diversity of a collection of sequences ignoring position.

We can also find the diversity of any kind of entity: say we had `input.txt`
containing:

```
cat
cat
cat
dog
dog
cat
parrot
```

We can find the diversity of species in this file using:

```
diversity -i input.txt -o output.csv -L -a
```

We are saying that we just have a list of entities separated by line (`-L`) and
we want to look at the entire entity, not positional (`-a`).

To look at the rarefaction curves and see if we have sampled enough species, we
can use

```
diversity -i input.txt -o output.csv -c rarefaction_output.csv -L -a
```

to get the rarefaction curve at each position (in this case just the entire
entity). We can use `-f` to speed up this calculation, but at the cost of
accuracy. In addition, **`-f` cannot work on large data sets (they will show up
as NaN). In this case, you must use the slower, but more accurate default
method.** The output for the curve is similar to the diversity output, with
label, window, position, and weight columns. However, there are three additional
columns: subsample, expected_richness, and mad. The rarefaction curve tells us
if we have sufficiently sampled enough if the curve plateaus. To check if the
curve levels off, we can plot the subsample column as the x axis and the
expected_richness column as the y axis. The mad column is only for empirical
richness, described below. The other file generated with `-O` contains a
percent_above column, which tells us the percent of subsamples that are above
95% of the height of the curve. This can give us a quick glance at the curve to
know if the curve "plateaus" (unless there are NaNs, which result in a nonsense
percentage). You must be careful when looking at this percentage, however, as it
might be high due to a very low number of sequences, say 2 or 3, as it's a
percent of those low values so it might be artificially high in those cases, but
you can tell by the weight. There is also an additional sampling column, which
says how many more samples are needed to get the proportion g of the estimated
richness in the assemblege. That estimated number is included under the S_est
column, where the observed richness is in the richness column. If the additional
sampling has a value of NaN, the number of entities in two samples is probably 0
so the value cannot be calculated.

For very large samples, the theoretical calculation of rarefaction may take
approximately an eternity to execute with a high probability of memory issues
(however there have been performance improvements so use this paragraph only if
needed). To overcome this issue, we look to empirical rarefaction. We can
literally take random subsamples from the population in -R runs, resulting in a
median of the values as well as the median absolute deviation for variation in
the runs. **You must use this method (in individual based rarefaction only,
triggered automatically when the number of random runs, -R, is specified) or
else you will run into performance issues with large data sets.**

## Further Reading

**For an overview of diversity**

[Entropy and diversity](http://onlinelibrary.wiley.com/doi/10.1111/j.2006.0030-1299.14714.x/abstract)

**For a positional and sliding window use case that uses this program**

[Conserved variation: identifying patterns of stability and variability in BCR
and TCR V genes with different diversity and richness metrics](http://www.ncbi.nlm.nih.gov/pubmed/23735612)

[Germline Amino Acid Diversity in B Cell Receptors is a Good Predictor of
Somatic Selection Pressures](http://www.ncbi.nlm.nih.gov/pubmed/24265630)

## Usage

```
Diversity, Gregory W. Schwartz

Usage: diversity [-l|--input-label LABEL] [-r|--input-order [1]|INT]
                 [-w|--input-window [1]|INT] [-i|--input-fasta FILE]
                 [-S|--input-sample-field INT] [-I|--input-subsampling INT INT]
                 [-f|--fast-bin] [-n|--remove-N] [-a|--whole-sequence]
                 [-L|--list] [-s|--sample] [-d|--rarefaction-df] [-t|--std]
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
                           3, ... '2 6' would be 2, 8, 14, ... Note: input is a
                           string so use quotations around the entry and it
                           always has the number of subsamples overall as the
                           last point
  -f,--fast-bin            Whether to use a much faster, but approximated,
                           binomial coefficient for the rarefaction analysis.
                           This method results in NaNs for larger numbers, so in
                           that case you you should use the slower, more
                           accurate default method
  -n,--remove-N            Remove 'N' and 'n' characters
  -a,--whole-sequence      Ignore window length and only analyze the entire
                           sequence for diversity and rarefaction curves.
  -L,--list                Analyze a diversity of species in a list separated by
                           lines instead of a fasta file
  -s,--sample              Whether to use sample based rarefaction (requires
                           sample ID field from input-sample-field)
  -d,--rarefaction-df      Whether to output the rarefaction curve as a data
                           frame
  -t,--std                 Whether to output to stdout or to a file if no file
                           is supplied
  -O,--output-rarefaction FILE
                           The csv file containing the rarefaction values (the
                           percent of the rarefaction curve that is above 95% of
                           the height of the rarefaction curve). Expects a
                           string, so you need a string even with std
  -c,--output-rarefaction-curve FILE
                           The csv file containing the rarefaction curve.
                           Expects a a string, so you need a string even with
                           std
  -o,--output FILE         The csv file containing the diversities at each
                           position. expects a string, so you need a string even
                           with std
```
