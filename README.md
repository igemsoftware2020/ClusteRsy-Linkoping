# ClusteRsy


## Welcome to our R shiny based app for transcriptome analysis.
This tool is developed by the Linköping iGEM team of 2020. Down below you can find all of the dependencies needed to run the developer version of the app.

We have included MODifieR, a R package for disease module identification. Enrichment analysis such as disease analysis, gene ontology analysis and pathway analysis using the Clusterprofiler package. We also provide visualization of the results as well as a database to store all the input and output data.

To run the developer version of this app there are a few dependencies that needs to added.



## __Installation__


##### This app was developed using R version 3.6.X and is recommended when running the developer version.

#### MODifieR

MODifieR requires Python 3 with additional Python libraries scipy, sqlite3, numpy and networkx. They are all included within [Anaconda](https://docs.anaconda.com/anaconda/install/)

In addition to this some R packages are required as well.
```R
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("AnnotationDbi",
                       "MODA",
                       "STRINGdb",
                       "limma",
                       "org.Hs.eg.db",
                       "foreach",
                       "doParallel",
                       "Rcpp",
                       "dynamicTreeCut",
                       "flashClust",
                       "reticulate",
                       "plyr",
                       "parallel",
                       "igraph",
                       "WGCNA",
                       "RSQLite",
                       "devtools",
                       "stackoverflow",
                       "preprocessCore",
                       "DESeq2",
                       "edgeR",
                       "openxlsx",
                       "ggplot2",
                       "ggdendro",
                       "ggrepel"),
                     version = "3.8")
```

Once these has been installed you can install MODifieR from GitHub:

```
devtools::install_git(url = "https://gitlab.com/Gustafsson-lab/MODifieR.git")
```

#### MODifieRDB
For the database to work an extended version of MODifieR needs to be installed from GitHub:

```
devtools::install_git(url = "https://github.com/ddeweerd/MODifieRDB.git")
```
#### Clusterprofiler
Enrichment analysis is provided with the Clusterprofiler package and can be installed from BiocManager:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("clusterProfiler")
```

#### CRAN dependencies
All of the CRAN dependencies has been taken care of is found in the 02_dev.R file. The only thing you will need to do is to CRAN install golem with
```
install.packages("golem")
```

Finally, now your done with all the installation needed to run our App.
Once you start it up, do an Install and Restart found in the Build tab and then you can start the app using run_app()
