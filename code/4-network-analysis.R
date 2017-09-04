## Load data ------------
data.or <- read.csv("data/data.csv", sep=",", encoding="UTF-8", na.strings="n/a")
data.process <- data.frame(data.or)

### Network Visualisation ------
# http://kateto.net/network-visualization

## Install package ---
if(! "igraph" %in% installed.packages()) install.packages("igraph", depend = TRUE)
if(! "network" %in% installed.packages()) install.packages("network", depend = TRUE)
if(! "sna" %in% installed.packages()) install.packages("sna", depend = TRUE)
if(! "visNetwork" %in% installed.packages()) install.packages("visNetwork", depend = TRUE)
if(! "threejs" %in% installed.packages()) install.packages("threejs", depend = TRUE)
if(! "networkD3" %in% installed.packages()) install.packages("networkD3", depend = TRUE)
if(! "ndtv" %in% installed.packages()) install.packages("ndtv", depend = TRUE)

library("igraph")
library("network")
library("sna")
library("visNetwork")
library("threejs")
library("networkD3")
library("ndtv")
