## Draft for association rules
## Needs to be finalized (parameter need to be adjusted) with larger dataset

#### http://michael.hahsler.net/research/arules_RUG_2015/demo/

if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
library(arules)
library(arulesViz)

data.or <- read.csv("data/data.csv", sep=";", encoding="UTF-8", na.strings="n/a")
data.process <- data.frame(data.or)


d <- data.process[, c("country","governorate","community_type","structure","membership","service_priority","workarea_priority", "size")]
trans <- as(d, "transactions")
trans
summary(trans)
dim(d)
dim(trans)
itemLabels(trans)
as(trans[1:3, 1:19], "matrix")
itemFrequencyPlot(trans, topN=2,  cex.names=.5)

## similarity
d <- dissimilarity(sample(trans, 3), method = "phi", which = "items")
d[is.na(d)] <- 1 # get rid of missing values
pdf(file="data/similarity.pdf", width=25)
plot(hclust(d), cex=.5)
dev.off()

nrow(trans)
500/nrow(trans)

##--------------------------------------
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
inspect(head(sort(itemsets), n=3))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=3))

plot(head(sort(itemsets, by = "lift"), n=3), method = "graph", control=list(cex=.8))

##--------------------------------------

trans <- trans[,-pmatch("pf_hcuff", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=3))

plot(head(sort(itemsets, by = "lift"), n=3), method = "graph", control=list(cex=.8))

##--------------------------------------


trans <- trans[,-pmatch("rf_bulg", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=3))

plot(head(sort(itemsets, by = "lift"), n=3), method = "graph", control=list(cex=.8))

##--------------------------------------

trans <- trans[,-pmatch("detailcm=CPW", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=19))

plot(head(sort(itemsets, by = "lift"), n=3), method = "graph", control=list(cex=.8))

##--------------------------------------

trans <- trans[,-pmatch("rf_vcrim", colnames(trans))]
trans <- trans[,-pmatch("rf_vcact", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=19))

plot(head(sort(itemsets, by = "lift"), n=3), method="graph", control=list(cex=.8))

##--------------------------------------

