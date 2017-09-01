Diversity
=========

Gregory W. Schwartz

This program will take a fasta file and find the diversity and rarefaction curve. The sequences must be aligned.

Depends on "fasta": https://github.com/GregorySchwartz/fasta.git 

## Citation

**Please cite this paper if you are using this program**

[Conserved variation: identifying patterns of stability and variability in BCR
and TCR V genes with different diversity and richness metrics](http://www.ncbi.nlm.nih.gov/pubmed/23735612)

## Installation

Thanks to stack installation is a lot easier:

```
stack install diversity
```

The binary should now be in your path (local bin).

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
diversity -i input.fasta -o output.csv -w 1 -r 1
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
richness in the assemblage. That estimated number is included under the S_est
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
Usage: diversity [-l|--input-label LABEL] [-r|--input-order [1]|INT]
                 [-w|--input-window [1]|INT] [-i|--input-fasta FILE]
                 [-S|--input-sample-field INT] [-C|--input-count-field INT]
                 [-I|--input-subsampling INT INT (INT)] [-g|--input-g Double]
                 [-f|--fast-bin] [-R|--input-runs INT] [-G|--keep-gaps]
                 [-n|--remove-N] [-a|--whole-sequence] [-L|--list] [-s|--sample]
                 [-d|--rarefaction-df] [-t|--std] [-O|--output-rarefaction FILE]
                 [-c|--output-rarefaction-curve FILE] [-o|--output FILE]
  Quantify the diversity of a population

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
  -C,--input-count-field INT
                           The index for the number of this type in the header
                           separated by '|' (1 indexed). Used if there are
                           multiple copies of one entry, so a '4' in the header
                           would indicate that this entity occurred 4 times.
                           Defaults to 0, meaning that this field is ignored and
                           count each sequence as occurring just once
  -I,--input-subsampling INT INT (INT)
                           The start point, interval, and optional endpoint of
                           subsamples in the rarefaction curve. For instance, '1
                           1 4' would be 1, 2, 3, 4 '2 6 14' would be 2, 8, 14,
                           ... Note: input is a string so use quotations around
                           the entry and it always includes the number of
                           subsamples overall in the result. Excluding the
                           endpoint results in the number of samples the
                           endpoint, so '1 1' would be 1, 2, 3, ..., N
  -g,--input-g Double      Used for calculating the number of individuals (or
                           samples) needed before the proportion g of the total
                           number of estimated species is reached. Sobs / Sest <
                           g < 1
  -f,--fast-bin            Whether to use a much faster, but approximated,
                           binomial coefficient for the rarefaction analysis.
                           This method results in NaNs for larger numbers, so in
                           that case you you should use the slower, more
                           accurate default method
  -R,--input-runs INT      The number of runs for empirical resampling
                           rarefaction. This method does not compute the
                           theoretical, it reports the actual median and median
                           absolute deviation (MAD) values of this many runs. If
                           this value is not 0, empirical rarefaction is
                           automatically enabled (individual based only, not for
                           sample based)
  -G,--keep-gaps           Do not remove '.' and '-' characters from the
                           analysis. This flag will thus treat these characters
                           as additional entities rather than be ignored as
                           artificial biological gaps in a sequence
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
