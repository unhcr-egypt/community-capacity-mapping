## I added all packages of the progres-analysis repository

## A curated list of packages
## @edouard_lgp

##This should detect and install missing packages before loading them –
packages <- c(
  
  ##################################################################
  ### Packages for Premodeling Stage
  
  ## Data Manipulation
  "lubridate","date","gdata","zoo", ## playing with date
  "ggseas", ## seasonal adjustemnt with GGplot2
  "dplyr",  "data.table", "doBy","tidyr", ## Data manipulation
  "reshape2", # package to easily melt data to long form
  "stringr","stringdist","stringi", ## string manipulation
  
  ## Missing value imputation
  "missForest",  "missMDA", "Amelia",
  
  
  ## Outlier Detection
  "outliers",  "evir",
  
  ## Feature Selection
  "features",  "RRF", 
  "Boruta", # wrapper for feature selection algorythm
  
  ## Dimension Reduction
  "CCP", # Dimension Reduction
  "FactoMineR", "ade4",  ## multivariate analysis - MCA
  
  
  ##### Packages for Visualisation
  "lattice", # Visualisation
  "ggplot2", ## advanced graphics
  "ggrepel", ## getting nice labels in ggplot2
  "ggvis", ## interactive grammar of graphics
  "ggthemes", ## Customised themes for ggplot2: excel, stata, economist, tufte, wall street journal...
  "grid", "gridExtra", # Assembel graphcis together
  "gtable", #Arrange 'Grobs' in Tables
  "vcd", # Visualisation of categorical data
  "RColorBrewer", # a package offering color palette from
  "scales", #Scale Functions for Visualization
  "extrafont", ##" load additional font
  "hexbin", ## Hexagrid viz
  "xkcd", ## Style from the xkcd comics
  "scatterplot3d",
  "corrplot", # Visualiation of correlation Matrix
  "igraph", #network analysis and visualisation
  "ellipse",  ## drawing ellipses and ellipse-like confidence regions
  "factoextra", ## Visualize the Results of Multivariate Data Analyses
  
  ##### Packages for Mapping  
  "sp","maptools","rgdal","rgeos", ## standard Geo manipulation packages
  "ggmap", ## get background from webmapping API
  "raster","cartography", ## packages used for the maps --
  "classInt",  ## used for univariate classification
  "deldir", # delaunay triangulation & Voronoi  
  "viridis", # Default Color Maps from 'matplotlib'
  "fields", ## Tools for Spatial Data
  
  ##################################################################
  ### Packages for Modeling Stage  
  
  
  "Hmisc", # generate a detailled describtion of a given dataset
  "gbm", # Generalized Boosted Regression Models
  "car", ## ## Companion to Applied Regression
  "rminer", "CORElearn",  # ordinal Regression
  "caret", # Gradient Boosting & AdaBoost 
  "bigRR",  ## Classification
  
  
  
  "e1071", #SVM (Support Vector Machine)
  "knncat", # KNN (K- Nearest Neighbors)
  "randomForest", # randomForest
  "stats", # Dimensionality Reduction Algorithms princomp
  ## Time Series
  "forecast", "ltsa",
  
  # survival analysis
  "survival", "BaSTA",
  "pastecs", #Analysis of Space-Time Ecological Series
  
  # Lasso and Elastic-Net Regularized Generalized Linear Models
  "glmnet", 
  "lme4", # Linear Mixed-Effects Models
  
  "MASS", 
  "VGAM", #Vector Generalized Linear and Additive Models
  "aod", ## Analysis of Overdispersed Data
  "fmsb",
  ## Cluster analysis
  "cluster", "cba", "Rankcluster", 
  
  ##################################################################
  ### Packages for Post Modeling Stage  
  
  "lmtest", # Testing Linear Regression Models
  
  "gvlma", #Global Validation of Linear Models Assumptions
  
  "lsmeans", "comparison", #general Model Validation
  "regtest", "ACD", #Regression validation
  
  "binomTools","Daim", ## classification validation
  "clusteval","sigclust", ## Clustering valisation
  
  "pROC","timeROC", # ROC Analysis  
  
  ## Recursive Partitioning and Regression Trees
  "rpart", "rpart.plot",
  
  ##################################################################
  ### Packages for Survey data management  
  "sampling", ## Survey Sampling
  "survey",  ##Analysis of Complex Survey Samples
  
  ##################################################################
  ### Other Packages
  
  
  "psych", ## Procedures for Psychological, Psychometric, and Personality Research
  
  "Benchmarking", #Benchmark and Frontier Analysis Using Data Envelopmenbt Aanalysis
  
  "pwr", # Power Analysis allows  to determine the sample size required to detect an effect of a given size with a given degree of confidence.
  
  ## text mining
  "tm", "twitteR" , 
  "wordcloud", #Word Clouds
  "LDAvis", # Interactive Visualization of Topic Models
  
  "AER",  # Applied economtrics with R
  
  "formatR", #  used to format the code
  
  "parallel", ## Improve performance
  "Rcpp", ## used to compile some pacjckages
  
  "foreign", ## read data from SPSS, SAS or Stata
  "sqldf", "RODBC", "RMongo","RSQLite", ## Direct connection with databases
  
  "rJava", "XLConnect", ## Read and write excel files
  "readxl", ## Read Excel files
  
  "httr", "rjson","jsonlite", ## get data from API
  "XML", "xml2", ## Manipulation of xml  
  
  "RCurl", ##used to download files from API -install CURL before and separately
  "devtools", # package used to load packages hosted in github -- 
  
  "gmailr", # Access gmail api
  
  "rattle", ## GUI for data mining
  
  "treemap",
  
  "data.tree",
  "jsonlite",
  "extrafontdb",
  "Rttf2pt1",
  "radarchart",
  "doBy"
  
)

## identify packages not installed yet
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

rm(packages)
library(doBy)
library(radarchart)
library(Rttf2pt1)
library(extrafontdb)
library(treemap)
library(fmsb)

library(data.tree)
library(jsonlite)

#gpclibPermit()

# loads packages into memory
library(stringr)
library(stringi)
#library(lattice)
#library(rattle)
library(car)
library(plyr)
library(ggplot2) ## The grammar of graphics!
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
#library(raster) ## Managing raster dataset
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning
library(lubridate)
library(date)
library(gdata)
library(gridExtra)
library(scales)
#library(formatR)
#library(RGtk2)
#library(gWidgetsRGtk2)
library(readxl)
library(plyr)
#library(xlsx)
library(FactoMineR)

theme_plot <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),
      
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA), 
      panel.spacing = unit(c(-.1,0.2,0.02,0.2), "cm"),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "#a8a8a8", size = 0.2),
      panel.grid.major.x = element_blank(),
      
      plot.background = element_rect(fill = "white", color = NA), 
      plot.title = element_text(family="Calibri", hjust = 0, color = "blue", size = 24),
      plot.subtitle = element_text(hjust = 0, color = "#4e4d47", size = 18, debug = F),
      plot.margin = unit(c(.5,.5,1,.5), "cm"),
      plot.caption = element_text(size = 14, hjust = 1, color = "#939184"),
      
      axis.text.y = element_text(size=14, color="#696969"),
      axis.text.x = element_text(size=14, color="#3f3f3f"),
      axis.line.y = element_line(color="#3f3f3f"),
      axis.line.x = element_line(color="transparent"),
      axis.ticks.x = element_line(colour = "white", size = 0.1),
      axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2),
      
      legend.title = element_text(size = 1, colour = "white"),
      
      ...
    )
}
