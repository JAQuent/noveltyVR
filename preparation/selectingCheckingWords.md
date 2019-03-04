Selecting and checking living/non-living words
================
Jörn Alexander Quent
March 04, 2019

Aim
===

The aim of this document is selecting words for the project noveltyVR, in which we are planning to study the effect of novelty on words that were either weakly shallowly or strongly deeply encoded. In the shallow encoding condition, participants will have to decide whether the first and the last letter of a word are in alphabetical order as in Otten et al. (2001). In the deep encoding, participants need to decide whether something is animate/inanimate. A subset of words will be selected (kindly shared by Leun Otten), compared based on their properties found in the [MRC Psycholinguistic Database](http://websites.psychology.uwa.edu.au/school/MRCDatabase/uwa_mrc.htm) and finally split into three lists.

Counting words
==============

Here, I count the number of words for each category (animancy, alphabetical) that are included in the file that was provided to me.

``` r
# Loading words used in https://academic.oup.com/brain/article/124/2/399/402300
words        <- read.table('U:/Projects/noveltyVR/preparation/ignore_wordlist_original_allcodes_fromOttten2001.txt', 
                           sep = '\t', 
                           header = TRUE)
names(words)  <- c('word', 'animacy', 'syllables', 'alphabetical')
words$word    <- as.character(words$word)
words$animacy <- factor(words$animacy, 
                       c(1, 0) ,
                       labels = c('animate', 'inanimate'))
words$alphabetical <- factor(words$alphabetical, 
                       c(1, 0) ,
                       labels = c('alphabetical', 'non-alphabetical'))

# How many words are there per category
table(words$animacy)
```

    ## 
    ##   animate inanimate 
    ##       275       277

``` r
table(words$syllables)
```

    ## 
    ##   1   2   3   4 
    ## 181 287  80   4

``` r
table(words$alphabetical)
```

    ## 
    ##     alphabetical non-alphabetical 
    ##              291              261

Finding word properties
=======================

In the code below, I find the properties in the data base that are available for the words that I use.

``` r
# Calculating word length
words$wordLength  <- nchar(words$word)

# Loading MRC Psycholinguistic data base 
# See http://websites.psychology.uwa.edu.au/school/MRCDatabase/mrc2.html
mrcDatabase <- read.csv('U:/Projects/noveltyVR/preparation/ignoreFiles/WordStim/mrcOnly.txt', 
                        header = TRUE ,
                        fill = TRUE,
                        as.is = TRUE , 
                        na.strings = '', 
                        sep = '\t')

# MRC properties
words$K.F.FREQ   <- NA_real_
words$K.F.NCATS  <- NA_real_
words$K.F.NSAMP  <- NA_real_
words$T.L.FREQ   <- NA_real_
words$BROWN.FREQ <- NA_real_
words$FAM        <- NA_real_
words$CONC       <- NA_real_
words$IMAG       <- NA_real_
words$MEANC      <- NA_real_
words$MEANP      <- NA_real_
words$AOA        <- NA_real_
# Looking properties up in MRC database
for(i in 1:dim(words)[1]){
  words$K.F.FREQ[i]   <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'K.F.FREQ']
  words$K.F.NCATS [i] <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'K.F.NCATS']
  words$K.F.NSAMP[i]  <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'K.F.NSAMP']
  words$T.L.FREQ[i]   <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'T.L.FREQ']
  words$BROWN.FREQ[i] <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'BROWN.FREQ']
  words$FAM[i]        <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'FAM']
  words$CONC[i]       <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'CONC']
  words$IMAG[i]       <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'IMAG']
  words$MEANC[i]      <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'MEANC']
  words$MEANP[i]      <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'MEANP']
  words$AOA [i]       <- mrcDatabase[which(mrcDatabase$WORD == words[i, 1]),][1,'AOA']
}

# Replace any zero with NA
words[words == 0] <- NA_real_ 
```

Split into four lists
=====================

Before splitting words into three lists, it is important find out how many words are animate and alphabetical, inanimate and alphabetical and so on in order to to select an equal number of words for all possible combinations.

``` r
combinations <- rep(1, dim(words)[1])
combinations[which(words$animacy == 'animate'   & words$alphabetical =='non-alphabetical')] <- 2
combinations[which(words$animacy == 'inanimate' & words$alphabetical =='alphabetical')]     <- 3
combinations[which(words$animacy == 'inanimate' & words$alphabetical =='non-alphabetical')] <- 4
words$combination <- combinations
table(words$combination)
```

    ## 
    ##   1   2   3   4 
    ## 124 151 167 110

``` r
# Words per list (max = min(table(words$combination)))
wordsPerCat <- 108 # There 110 inanimate/non-alphabetical words therefore we choose 108 because it's dividable by 3, which is important because we need three lists for shallow, deep and new words. Those two list again need to be dividedable by two, so they can be split in half to be assigned to the first and the second block of the ABBA design.
listlength <- (wordsPerCat/3)*4
```

Next, 108 words are now randomly selected from each combination and then assigned a list. This number is chosen so that each list (144 words per list) can be further split in half but still having the same number for all possible combinations.

``` r
# Setting seed
set.seed(392)

# Selecting a random sample from each combination
comb1 <- words[sample(which(words$combination == 1), wordsPerCat),]
comb2 <- words[sample(which(words$combination == 2), wordsPerCat),]
comb3 <- words[sample(which(words$combination == 3), wordsPerCat),]
comb4 <- words[sample(which(words$combination == 4), wordsPerCat),]

# Shuffle order of comb
comb1 <- comb1[sample(1:wordsPerCat),]
comb2 <- comb2[sample(1:wordsPerCat),]
comb3 <- comb3[sample(1:wordsPerCat),]
comb4 <- comb4[sample(1:wordsPerCat),]

# Splitting into four lists
list1 <- rbind(comb1[1:(wordsPerCat/3),], 
               comb2[1:(wordsPerCat/3),],
               comb3[1:(wordsPerCat/3),],
               comb4[1:(wordsPerCat/3),])
list2 <- rbind(comb1[(wordsPerCat/3 + 1):(wordsPerCat/3*2),], 
               comb2[(wordsPerCat/3 + 1):(wordsPerCat/3*2),],
               comb3[(wordsPerCat/3 + 1):(wordsPerCat/3*2),],
               comb4[(wordsPerCat/3 + 1):(wordsPerCat/3*2),])
list3 <- rbind(comb1[(wordsPerCat/3*2 + 1):(wordsPerCat),], 
               comb2[(wordsPerCat/3*2 + 1):(wordsPerCat),],
               comb3[(wordsPerCat/3*2 + 1):(wordsPerCat),],
               comb4[(wordsPerCat/3*2 + 1):(wordsPerCat),])

# Concatenating lists again
wordsLists      <- rbind(list1, list2, list3)
wordsLists$list <- factor(rep(1:3, each = listlength))
```

Inspecting properties of words in those lists
=============================================

Below, I check whether any of the three lists differs on any of the relevant properties. A positive Bayes factor (BF) indicates evidence in favour of the hypothesis that the are no differences.

Note that a lot of values in the database are 0, which are here treated as NA explaining the huge variability of the number of data points below.

![](selectingCheckingWords_files/figure-markdown_github/inspectingWordlists-1.png)![](selectingCheckingWords_files/figure-markdown_github/inspectingWordlists-2.png)

All Bayes factors above favour the hypothesis that there are no differences between the word lists. Therefore, we are going to use them in our experiment.

Summary stats for all lists:

| Metric      | List1               | List2               | List3               |
|:------------|:--------------------|:--------------------|:--------------------|
| AOA         | 359.51 (SD = 90.04) | 355.1 (SD = 107.67) | 324.86 (SD = 97.3)  |
| BROWN.FREQ  | 3.08 (SD = 3.35)    | 2.89 (SD = 4)       | 2.62 (SD = 2.91)    |
| CONC        | 574.23 (SD = 51.36) | 582.86 (SD = 36.43) | 573.96 (SD = 51.34) |
| FAM         | 490.51 (SD = 60.25) | 484.18 (SD = 59.69) | 489.07 (SD = 63.22) |
| IMAG        | 567.28 (SD = 46.67) | 571.09 (SD = 39.13) | 568 (SD = 54.96)    |
| K.F.FREQ    | 10.98 (SD = 18.96)  | 9.14 (SD = 11.3)    | 11.05 (SD = 18.52)  |
| K.F.NCATS   | 3.83 (SD = 2.76)    | 3.78 (SD = 2.77)    | 3.94 (SD = 2.93)    |
| K.F.NSAMP   | 6.62 (SD = 9.08)    | 5.91 (SD = 7.84)    | 6.88 (SD = 10.98)   |
| MEANC       | 428.49 (SD = 43.63) | 423.75 (SD = 45.55) | 425.42 (SD = 47.57) |
| MEANP       | 644.25 (SD = 56.56) | 636.44 (SD = 75.53) | 653.33 (SD = 68.32) |
| T.L.FREQ    | 81.96 (SD = 103.67) | 88.94 (SD = 158.36) | 105.54 (SD = 215.3) |
| Word length | 5.67 (SD = 1.2)     | 5.83 (SD = 1.34)    | 5.83 (SD = 1.26)    |

Writing text files
------------------

As all checks are completed without a problem, the input files for the task are created below.

``` r
# Word list 1
write.table(subset(wordsLists, wordsLists$list == 1, select = c('word', 'animacy', 'alphabetical')),
          'wordList_1.txt',
          row.names = FALSE,
          col.names = FALSE,
          quote     = FALSE,
          sep       = ' ')

# Word list 2
write.table(subset(wordsLists, wordsLists$list == 2, select = c('word', 'animacy', 'alphabetical')),
          'wordList_2.txt',
          row.names = FALSE,
          col.names = FALSE,
          quote     = FALSE,
          sep       = ' ')

# Word list 3
write.table(subset(wordsLists, wordsLists$list == 3, select = c('word', 'animacy', 'alphabetical')),
          'wordList_3.txt',
          row.names = FALSE,
          col.names = FALSE,
          quote     = FALSE,
          sep       = ' ')
```

References
==========

Otten, L. J., Henson, R. N. A., & Rugg, M. D. (2001). Depth of processing effects on neural correlates of memory encoding: Relationship between findings from across- and within-task comparisons. Brain, 124(2), 399–412. <https://doi.org/10.1093/brain/124.2.399>
