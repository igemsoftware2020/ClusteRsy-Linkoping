# ClusteRsy


## Welcome to our R shiny based app for transcriptome analysis.
This tool was developed by the Linköping iGEM team of 2020. Down below you can find all of the dependencies needed to run the developer version of the app.

---
[<img height="32" width="32" src="https://cdn.jsdelivr.net/npm/simple-icons@v3/icons/youtube.svg" />](https://www.youtube.com/channel/UCLzs3_Txac7oKbWv5Xl6v-w/featured)  Video tutorials on how to use ClusteRsy can be found on our youtube

[<img height="30" width="30" src="https://raw.githubusercontent.com/iconic/open-iconic/master/svg/globe.svg"/>](https://2020.igem.org/Team:Linkoping) Read more about our project on our Wiki

---

### __Contributors__

__Adam Lång - Teamleader and full stack developer__

__Jake P - Full stack developer__

__Alexander Johansson - R shiny developer__

__Erika Mattsson - R shiny developer__

__Lucas Porcile - R shiny developer__

__Ronja Höglund - HTML/CSS developer__

---

# __Docker__
We have prepared a docker image, this is easy to set up and the software will be up and running in no time at all! Please follow the instructions below. 

### __How to run the docker image__

1.  First you need to [download Docker](https://www.docker.com/get-started) to your local machine. 

2. Open up your terminal then run the following command 

```
docker pull liuigem/clustersy_app
```

3. Once the download is complete you can simply run the app by running the following command in the terminal

```
docker run -d --rm -p 3838:3838 liuigem/clustersy_app
```

The docker image is now running locally and it can be found either in the docker desktop application or visit your preferred web browser and then type: 

```
http://localhost:3838/
``` 
ClusteRsy should now be up and running! 

## __Run the app in R__
It's also possible to run the developers version of the app using R. To do this please follow the instructions below.

## __Preparation__
Before the software can be used there are a couple of dependencies that needs to be installed. Please follow the steps in __Installation__ 

__MODifieR__

We have included MODifieR, a R package for disease module identification. Enrichment analysis such as disease analysis, gene ontology analysis and pathway analysis using the Clusterprofiler package. We also provide visualization of the results as well as a database to store all the input and output data.


To run the developer version of this app there are a few dependencies that needs to added.


## __Installation__


##### This app was developed using R version 3.6.X and is recommended when running the developer version.

#### MODifieR

[MODifieR](https://gitlab.com/Gustafsson-lab/MODifieR) requires Python 3 with additional Python libraries scipy, sqlite3, numpy and networkx. They are all included within [Anaconda](https://docs.anaconda.com/anaconda/install/)

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
Enrichment analysis is provided with the [Clusterprofiler](https://bioconductor.org/packages/release/bioc/vignettes/clusterProfiler/inst/doc/clusterProfiler.html) package and can be installed from BiocManager:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("clusterProfiler")
```

It's now time to install the actual software. It's provided as a R-package and it can easily be installed from GitHub within

```R
devtools::install_git(url = "https://github.com/igemsoftwareadmin/ClusteRsy-Linkoping.git")
```

__How to run the app__
Once everything has been installed you can simply run the following using the console in R. 

```R
ClusteRsy::run_app()
```

### __Set up database__
We provide a SQL database. You can either create a new one or you can use our database that we used during modeling. A PPI network (STRING v.11 filtered to only contain human genes and a score > 700) is included by default.  

__1. How to set up our database__ 

Here you can [download](https://www.dropbox.com/s/z731ksu1mryfbt6/igem.db?dl=0) our database. Once this has been downloaded, go to the ClusteRsy library folder and place it in the database folder. 

__2. How to set up a new database__

If you want to use a new database only containing the default PPI network then simply skip step 1 and use in the R console.
```R
ClusteRsy::run_app()
``` 

__Please note that the first time you start the app it will load for a while, this is because a clique database i.e a database containing all of the possible cliques for the PPI network is being built. (approx. ~5mins)__
