# Josefine_Mutzenbacher_replication

Scripts and corpora for the replication of the study:  
Laura Untner and Murray G. Hall, "Josefine Mutzenbacher. Ein Pornographischer Roman von Ernst Klein?", *Jahrbuch Für Internationale Germanistik*, 15.1 (2023), pp. 11–28.  
Based on the ["stylo" R package](https://github.com/computationalstylistics/stylo).

## Scripts

All analyses are performed by the **"stylometric_analyses.R"** script.

Features for the analyses are directly taken from the **"Analysis_configuration.csv"** file. Please open and modify it if you want to perform different analyses.

### Available features

- *my_corpus* defines the name of the folder on which to run the analysis
- *my_distance* defines the stylometric distance with which to run the analysis (more info on available distances: https://github.com/computationalstylistics/stylo_howto/blob/master/stylo_howto.pdf)
- *MFW_base* defines the number of most frequent words (MFW) for the analysis
- *MFW_evolution_analysis*, if set to TRUE, performs analysis of the evolution of distances of the candidate authors with varying MFW (up to MFW_base)
- *word_importance_analysis*, if set to TRUE, performs analysis of the importance of words in the attribution to Klein and Salten
- *word_removal_analysis*, if set to TRUE, compares the results of stylometric analyses including or excluding selected words
- *words_to_remove* lists the words to remove from the analysis (separated by spaces); leave it blank to perform an analysis with all MFW

## Corpora

Due to copyright limitations with the work of Ernst Klein, we are not allowed to share the texts used for the analysis. In any case, they should be included in the **"corpus_full"** and **"corpus_small"** folders (see respective **README.md** files for more information).

## Requirements

R version 4.4.1 (2024-06-14) -- "Race for Your Life"  
Required R libraries:

- stylo (0.7.5)
- magrittr (2.0.3)
- ggplot2 (3.5.1)
- tidyverse (2.0.0)
- ggpubr (0.6.0)
