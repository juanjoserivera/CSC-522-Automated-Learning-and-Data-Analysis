# Spring 2019 - ALDA Project - Email Authorship Identification

## Dataset
Enron dataset, About 500,000 MIME files from 150 authors.
https://www.cs.cmu.edu/~./enron/

## Used Libraries

* stylo
* pmar
* stopwords
* wordcloud
* caTools
* caret
* plyr
* uniqtag
* devtools
* rlist

## Project Structure

```bash
Python
└── Cleaning.py
R
├── RObjects
│   └── *.rds
├── corpus
│   └── *.txt
├── data
│   └── *.csv
├── plots
│   └── *.png  
├── P09_Authorship_Idenfication_functions.R
├── P09_DataSet_Analysis.R
├── P09_Perform_Experimentation.R
├── P09_Presentation_Info.R
├── P09_library_loader.R
├── P09_load_corpus.R
└── P09_perform_delta.R
```

## Relevant .RDS files

 File | Definition
 -|-
 R/RObjects/feature_vector_bag_of_words.rds|Result of Feature Vector extraction over the whole corpus using BoW.
 R/RObjects/feature_vector_ngrams_3000.rds|Result of Feature Vector extraction over the whole corpus using ngram 3000.
 R/RObjects/feature_vector_ngrams_6000.rds|Result of Feature Vector extraction over the whole corpus using ngram 6000.
 R/RObjects/bow.rds| Matrix that contains experimentation result for 10 runs using BoW.
 R/RObjects/bow_smart.rds| Matrix that contains experimentation result for 10 runs using BoW removing "Smart" set of stopwords.
 R/RObjects/bow_snowball.rds| Matrix that contains experimentation result for 10 runs using BoW removing "Snowball" set of stopwords.
 R/RObjects/ngram_3000.rds| Matrix that contains experimentation result for 10 runs using ngram with 3000 features.  
 R/RObjects/ngram_6000.rds| Matrix that contains experimentation result for 10 runs using ngram with 6000 features.  



## How to delete large files on directory(linux/unix)?
```
find . -type f -print -delete

```
## How to load R Object(.rds) in r?
```r
ft_bow_gpu<-readRDS('./RObjects/rstudio-gpu/feature_vector_bag_of_words_gpu.rds')
ft_ngram_gpu<-readRDS('./RObjects/rstudio-gpu/feature_vector_ngrams_gpu.rds')

ft_bow_36<-readRDS('./RObjects/rstudio-36Cores/feature_vector_bag_of_words_36Cores.rds')
ft_ngram_36<-readRDS('./RObjects/rstudio-36Cores/feature_vector_ngrams_36Cores.rds')
```

## How to use RStudio with AWS?
Using Amazon Machine Image (AMI), Here the steps:

http://www.louisaslett.com/RStudio_AMI/
