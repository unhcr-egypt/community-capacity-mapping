## Load data ------------
data.or <- read.csv("data/data.csv", sep=",", encoding="UTF-8", na.strings="n/a")
data.process <- data.frame(data.or)

## Association rules --------------------
#### http://michael.hahsler.net/research/arules_RUG_2015/demo/

if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
library(arules)
library(arulesViz)


#names(data.process)
d <- data.process[, c("surveyor_details.geographics.country",
                      "surveyor_details.geographics.governorate",
                      "mapping_part.demographics.community_type",
                      "mapping_part.demographics.community_details.structure",
                      "mapping_part.demographics.community_details.membership",
                      "mapping_part.capacities.community_work.service_priority",
                      "mapping_part.capacities.community_work.workarea_priority",
                      "mapping_part.demographics.community_details.size")]#

## Convert to transactions - itemMatrix in sparse format -----------------
## items  are binary dummy variables for nominal values
trans <- as(d, "transactions")
#trans
#summary(trans)
dim(d)
dim(trans)
#itemLabels(trans)

#as(trans[1:3, 1:19], "matrix")
pdf(file="out/itemFrequencyPlot.pdf", width=25)
itemFrequencyPlot(trans, topN=2,  cex.names=.5)
dev.off()

## Similarity --------------
# use positively correlated (Phi coefficient) items.
# dissimilarity() converts the correlation into distances.
# Note: we use sample to speed up and plot as large PDF
# resulting dendrogram
d1 <- dissimilarity(sample(trans, 3), method = "phi", which = "items")
d1[is.na(d1)] <- 1 # get rid of missing values
pdf(file="out/similarity.pdf", width=25)
plot(hclust(d1), cex=.5)
dev.off()

#nrow(trans)
#500/nrow(trans)

### Mine Frequent Itemsets --------------------------------------
# Find an interesting support
itemsets <- apriori(trans, parameter = list(target = "frequent", supp=0.001, minlen = 2, maxlen=4))
#inspect(head(sort(itemsets), n=3))

## Add an additional quality measure
quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
#inspect(head(sort(itemsets, by = "lift"), n=3))

## Plot itemsets as a graph.
# Different subgroups with items that are related to each other can be identified.
pdf(file="out/graph1.pdf", width=10)
plot(head(sort(itemsets, by = "lift"), n=nrow(d)), method = "graph", control=list(cex=.6))
dev.off()


## Remove certain variables or modalities to make the graph more legible ------
#trans2 <- trans[,-pmatch("car-to-remove", colnames(trans))]
#itemsets2 <- apriori(trans2, parameter = list(target = "frequent", supp=0.001, minlen = 2, maxlen=4))
#quality(itemsets2)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
#inspect(head(sort(itemsets, by = "lift"), n=3))
#pdf(file="out/graph2.pdf", width=25)
#plot(head(sort(itemsets2, by = "lift"), n=3), method = "graph", control=list(cex=.8))
#dev.off()

### Mine Association Rules -----------------

r <- apriori(trans, parameter = list(supp=0.001, maxlen=4))
inspect(head(sort(r, by="lift"), n=10))
## Default is a scatter plot.
## Dark (red) dots are interesting since they represent rules with high lift.
pdf(file="out/association-rule.pdf", width=25)
plot(r)
dev.off()
## Try interactive plot
plot(r, interactive=TRUE)

## Now subset mined rules
#r_summon <- subset(r, subset = items %pin% "sumissue")
#r_summon
#inspect(head(sort(r_summon, by="lift"), n=10))
# itemFrequencyPlot(items(r_summon), topN=20)
# plot(head(sort(r_summon, by="lift"), 50), method="graph", control=list(cex=.7))

