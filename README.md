# ExpressExtras

## Introduction
ExpressExtras is an R package that provides helper functions for running the RNASeq pipeline prior to being hosted on the Shiny App [NGSViewer](https://github.com/Morriseylab/NGSViewer)

## Requirements
```
install.packages(c("dplyr","tidyr"))

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("org.Mm.eg.db","EnsDb.Mmusculus.v75","org.Hs.eg.db","org.Rn.eg.db","EnsDb.Hsapiens.v75","sva","topGO"))

```

## Installation 
```
devtools::install_github('Morriseylab/ExpressExtras')
```
