## Load data ------------
data.or <- read.csv("data/data.csv", sep=",", encoding="UTF-8", na.strings="n/a")
data.process <- data.frame(data.or)

### Network Visualisation ------
# http://kateto.net/network-visualization

## Install package ---
install.packages("igraph")
install.packages("network")
install.packages("sna")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")
