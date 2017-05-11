## In this script all possible variables of the 'community assets 
## and capacity mapping' kobo project are visualized in bar charts, 
## textual details are combined to useful lists

## Used date: Kopo xls form -> data/form.csv <- static as reference 
##            Kobo data     -> data/data.csv <- needs to be added and updated
## (here only a dummy data set is used)

## Once you use real data be aware of data protection issues, don't add real data.csv to Github


#######################################
### TABLE OF CONTENT

##  1) Data load

##  ----------(to be filled by surveyor)----------------
##  2) Introductory questions 
##  2.1) Status of surveyor
##  2.2) Country where mapping takes place
##  2.3) Governorate/District where mapping takes place
##  2.4) City where mapping takes place
##  2.5) Type of collaborating community in this mapping
##  2.6) List of collaborating entities
##  ----------------------------------------------------

##  3) Community demographics & structure
##  3.1) Community type (formal/informal/individual)
##  3.2) Demographics of communities (informal & formal)
##  3.2.1) Way of establishment
##  3.2.2) Community structure
##  3.2.3) Membership requirements
##  3.2.4) Members
##  3.2.5) Community size
##  3.2.6) Proportion of female, elderly, children within the community
##  3.2.7) Ethical standards in the community
##  3.3) Demographics of engaged individuals
##  3.3.1) Sex
##  3.3.2) Age*
##  3.3.3) Nationality

##  4) Community work & services
##  4.1) Working area
##       Prioritized working area
##  4.2) Community services
##       Prioritized community services
##  4.3) Recognition of engaged individual (entity = individual) within the community
##  4.4) Skills of enganged individual (entity = individual)
##  4.5) Talents of enganged individual (entity = individual)

##  5) Referrals*

##  6) Targeted groups
##  6.1) Percent of targeted groups
##  6.2) Specific groups of concern

##  7) Active community members
##  7.1) Type of active community members
##  7.2) Duration of community work

##  8) Communication channels
##  8.1) Type of used communication shannels
##  8.2) List of manual entries of specific communication channels

##  9) Meeting space
##  9.1) Type of meeting space
##  9.2) Type of access to meeting space
##  9.3) Conduciveness of meeting space

## 10) Community challenges

## 11) Community collaboration
## 11.1) Reward to motivate community work
## 11.2) Collaboration with UNHCR

## 12) Contact details (for internal use only)
#######################################


############################################################################################
############################################################################################
## 1) load data
data.or <- read.csv("data/data.csv", sep=";", encoding="UTF-8", na.strings="n/a")
data.process <- data.frame(data.or)

form.or <- read.csv("data/form.csv", sep=";", encoding="UTF-8", na.strings="n/a")



############################################################################################
############################################################################################
## 2) Community introductory questions (to be filled by surveyor)
data.intro <- data.process[,5:21]

## 2.1) Status of surveyor
df.statussurveyor <- data.intro[,2]
data <- data.frame(table(df.statussurveyor))
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "association"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.statussurveyor)
#######################################
## PLOT community types in percent
p.statussurveyor <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.statussurveyor)


## 2.2) Country where mapping takes place
df.mapcountry <- data.intro[,4]
data <- data.frame(table(df.mapcountry))
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "adm0"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.mapcountry)
#######################################
## PLOT countries in percent
p.mapcountry <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.mapcountry)

## 2.3) Governorate/District where mapping takes place
df.mapgovernorate <- data.intro[,5]
data <- data.frame(table(df.mapgovernorate))
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "adm1"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.mapgovernorate)
#######################################
## PLOT governorates in percent
p.mapgovernorate <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.mapgovernorate)

## 2.4) City where mapping takes place
df.mapcity <- data.intro[,6]
data <- data.frame(table(df.mapcity))
colnames(data) <- c("var", "freq")
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.mapcity)
#######################################
## PLOT cities in percent
p.mapcity <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.mapcity)


## 2.5) Type of collaborating community in this mapping
df.collabunit <- data.intro[,8:16]
colnames(df.collabunit) <- c("Community members", "Interest-based organization", "Agency", "Authority",
                             "Informal community group", "Professional association", "Non-Governmental organization",
                             "None", "Community-based organization")
  data <- reshape(df.collabunit, 
                varying = c("Community members", "Interest-based organization", "Agency", "Authority",
                            "Informal community group", "Professional association", "Non-Governmental organization",
                            "None", "Community-based organization"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Community members", "Interest-based organization", "Agency", "Authority",
                          "Informal community group", "Professional association", "Non-Governmental organization",
                          "None", "Community-based organization"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.collabunit)
#######################################
## PLOT type of collaborating entity in this mapping in percent
p.collabunit <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.collabunit)


## 2.6) List of collaborating entities 
df.unitslist <- data.process[,c(1,5,6,8:10,21:22)]   
colnames(df.unitslist) <- c("Date", "Name of surveyor", "Status of surveyor", "Country", "Governorate", "City", "Name of collaborating entity", "Type of collaborating entity")
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.8)),
  colhead = list(fg_params=list(cex = 0.9)),
  rowhead = list(fg_params=list(cex = 0.8)))
p.unitslist <- tableGrob(df.unitslist, theme=mytheme)
plot(p.unitslist)
rm(df.unitslist)  


rm(data.intro)
############################################################################################
############################################################################################
## 3) Community demographics & structure
data.demogr <- data.process[,22:62]

## 3.1) Community type (formal/informal/individual)
df.commtype <- data.demogr[,1]
data <- data.frame(table(df.commtype))
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "groupcategories"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.commtype)
#######################################
## PLOT community types in percent
p.commtype <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.commtype)

## 3.2) Demographics of communities (informal & formal)
## 3.2.1) Way of establishment
df.establishment <- data.demogr[,7:16]
colnames(df.establishment) <- c("With support of fund provider", "By the government", 
                                "as a professional association", "Through traditional leaders", "With support of umbrella organization",
                                "as a public institution", "Through religious leaders", "Initiated by interested group of people",
                                "With support of diaspora members", "Through individual/family initiative")
data <- reshape(df.establishment, 
                varying = c("With support of fund provider", "By the government", 
                            "as a professional association", "Through traditional leaders", "With support of umbrella organization",
                            "as a public institution", "Through religious leaders", "Initiated by interested group of people",
                            "With support of diaspora members", "Through individual/family initiative"), 
                v.names = "freq",
                timevar = "var", 
                times = c("With support of fund provider", "By the government", 
                          "as a professional association", "Through traditional leaders", "With support of umbrella organization",
                          "as a public institution", "Through religious leaders", "Initiated by interested group of people",
                          "With support of diaspora members", "Through individual/family initiative"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.establishment)
#######################################
## PLOT way of establishment in percent
p.establishment <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.establishment)

## 3.2.2) Community structure
df.structure <- data.demogr[,17]
data <- data.frame(table(df.structure))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "structure"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.structure)
#######################################
## PLOT community structure in percent
p.structure <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.structure)

## 3.2.3) Membership requirements
df.membership <- data.demogr[,18]
data <- data.frame(table(df.membership))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "membership"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.membership)
#######################################
## PLOT membership requirements in percent
p.membership <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.membership)


## 3.2.4) Members
df.members <- data.demogr[,20:26]
colnames(df.members) <- c("Refugees/Asylum-seekers", "Internally displaced persons",
                          "Diaspora members", "Legally residing foreign nationals",
                          "Persons in mixed movement situtations", "Returnees", "Host community members")
data <- reshape(df.members, 
                varying = c("Refugees/Asylum-seekers", "Internally displaced persons",
                            "Diaspora members", "Legally residing foreign nationals",
                            "Persons in mixed movement situtations", "Returnees", "Host community members"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Refugees/Asylum-seekers", "Internally displaced persons",
                          "Diaspora members", "Legally residing foreign nationals",
                          "Persons in mixed movement situtations", "Returnees", "Host community members"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.members)
#######################################
## PLOT community members in percent
p.members <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.members)


## 3.2.5) Community size
df.size <- data.demogr[,27]
data <- data.frame(table(df.size))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "number_members"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.size)
#######################################
## PLOT community size in percent
p.size <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.size)

## 3.2.6) Proportion of female, elderly, children within the community
df.dependencies <- data.demogr[,28:30]
colnames(df.dependencies) <- c("Female", "Elderly", "Children")
data <- reshape(df.dependencies, 
                varying = c("Female", "Elderly", "Children"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Female", "Elderly", "Children"), 
                new.row.names = 1:2000,
                direction = "long")
data["id"] <- NULL
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.dependencies)
#######################################
## PLOT specific community groups in percent
p.dependencies <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.dependencies)


## 3.2.7) Ethical standards in the community
df.ethics <- data.demogr[,32:36]
colnames(df.ethics) <- c("Once complaint is received", "Through code of conduct",
                               "Through agreed practices", "Have not thought of it",
                               "We plan to adopt minimum behavioural standard")
data <- reshape(df.ethics, 
                varying = c("Once complaint is received", "Through code of conduct",
                            "Through agreed practices", "Have not thought of it",
                            "We plan to adopt minimum behavioural standard"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Once complaint is received", "Through code of conduct",
                          "Through agreed practices", "Have not thought of it",
                          "We plan to adopt minimum behavioural standard"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.ethics)
#######################################
## PLOT community's ethical standards in percent
p.ethics <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.ethics)

## 3.3) Demographics of engaged individuals
## 3.3.1) Sex
df.indsex <- data.demogr[,38]
data <- data.frame(table(df.indsex))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "sex"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.indsex)
#######################################
## PLOT indivisual's sex in percent
p.indsex <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.indsex)

# ## 3.3.2) Age
# df.indage <- data.frame(data.demogr[,39])
# data <- data.frame(df.indage[!apply(df.indage == "", 1, all),])
# data$age <- 
# colnames(data) <- c("age")
# data <- merge(data, form.or, by = "name")
# 
# rm(df.indage)
# #######################################
# ## PLOT indivisual's age in percent
# p.indage <- ggplot(data=data, aes(x=label, y=freq)) + 
#   geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
#   theme_plot() +
#   theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
#         panel.grid.major = element_blank(),
#         axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
#         axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
#         axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
#         axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
#   scale_y_continuous(labels=percent, lim=c(0,1)) +
#   guides(fill=FALSE) +
#   xlab("") + ylab("") +
#   ggtitle(label="")+
#   coord_flip()
# plot(p.indsex)


## 3.3.3) Nationality
df.indnationality <- data.demogr[,40]
data <- data.frame(table(df.indnationality))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "nationality"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.indnationality)
#######################################
## PLOT individual's nationality in percent
p.indnationality <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.indnationality)


## 3.3.4) Individual's legal status
df.indstatus <- data.demogr[,41]
data <- data.frame(table(df.indstatus))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "member_status"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.indstatus)
#######################################
## PLOT indivdual's legal status in percent
p.indstatus <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.indstatus)


rm(data.demogr)
############################################################################################
############################################################################################
## 4) Community work & services
data.commwork <- data.process[,63:162]

## 4.1) Working area
df.workarea <- data.commwork[,25:41]
colnames(df.workarea) <- c("Education", "Child Protection", "Housing/land issues", "Water/Sanitation/Hygiene",
                           "Health/Nutrition", "Gender empowerment", "Food and household materials",
                           "Mental Health & Psychosocial Support", "Social Cohesion", "Detention monitoring and alternatives to detention",
                           "Birth registration and other documentation", "Livelihood & Income generation",
                           "Legal Aid/Support", "Specific group empowerment", "Protection and human rights",
                           "Integration or Reintegration", "Access to justice")

data <- reshape(df.workarea, 
                varying = c("Education", "Child Protection", "Housing/land issues", "Water/Sanitation/Hygiene",
                            "Health/Nutrition", "Gender empowerment", "Food and household materials",
                            "Mental Health & Psychosocial Support", "Social Cohesion", "Detention monitoring and alternatives to detention",
                            "Birth registration and other documentation", "Livelihood & Income generation", 
                            "Legal Aid/Support", "Specific group empowerment", "Protection and human rights",
                            "Integration or Reintegration", "Access to justice"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Education", "Child Protection", "Housing/land issues", "Water/Sanitation/Hygiene",
                          "Health/Nutrition", "Gender empowerment", "Food and household materials",
                          "Mental Health & Psychosocial Support", "Social Cohesion", "Detention monitoring and alternatives to detention",
                          "Birth registration and other documentation", "Livelihood & Income generation", 
                          "Legal Aid/Support", "Specific group empowerment", "Protection and human rights",
                          "Integration or Reintegration", "Access to justice"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.workarea)
#######################################
## PLOT working area in percent
p.workarea <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.workarea)

## Prioritizes working area
workarea.prio <- data.frame(data.commwork[,42])
data <- data.frame(table(workarea.prio))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "workarea"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(workarea.prio)
#######################################
## PLOT indivisual's nationality in percent
p.workarea.prio <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.workarea.prio)



## 4.2) Community services
df.services <- data.commwork[,2:22]
colnames(df.services) <- c("Sharing information on availability of and accessibility to the services", "Awareness activities",
"Community dialogue", "Organizing or participating in community events ",
"Recreational activities", "Skill and knowledge development support",
"Identifying persons with specific needs", "Supporting for alternative dispute resolution",
"Community building", "Taking or supporting community-led initiatives",
"Taking lead or participating in advocacy", "Helping people in need with specific support/action",
"Facilitating access to services", "Facilitating complaints to service provider",
"Facilitating feedback", "Identifying concerns, challenges and solutions",
"Resolving inter-community disputes", "Empowering certain segment of community",
"Language support", "Technical/professional advices", "Preventative activities")
data <- reshape(df.services, 
                varying = c("Sharing information on availability of and accessibility to the services", "Awareness activities",
                            "Community dialogue", "Organizing or participating in community events ",
                            "Recreational activities", "Skill and knowledge development support",
                            "Identifying persons with specific needs", "Supporting for alternative dispute resolution",
                            "Community building", "Taking or supporting community-led initiatives",
                            "Taking lead or participating in advocacy", "Helping people in need with specific support/action",
                            "Facilitating access to services", "Facilitating complaints to service provider",
                            "Facilitating feedback", "Identifying concerns, challenges and solutions",
                            "Resolving inter-community disputes", "Empowering certain segment of community",
                            "Language support", "Technical/professional advices", "Preventative activities"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Sharing information on availability of and accessibility to the services", "Awareness activities",
                          "Community dialogue", "Organizing or participating in community events ",
                          "Recreational activities", "Skill and knowledge development support",
                          "Identifying persons with specific needs", "Supporting for alternative dispute resolution",
                          "Community building", "Taking or supporting community-led initiatives",
                          "Taking lead or participating in advocacy", "Helping people in need with specific support/action",
                          "Facilitating access to services", "Facilitating complaints to service provider",
                          "Facilitating feedback", "Identifying concerns, challenges and solutions",
                          "Resolving inter-community disputes", "Empowering certain segment of community",
                          "Language support", "Technical/professional advices", "Preventative activities"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.services)
#######################################
## PLOT community services in percent
p.services <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.services)

## Prioritized community services
services.prio <- data.frame(data.commwork[,23])
data <- data.frame(table(services.prio))
data <- data[!apply(data[1] == "", 1, all),]
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "service"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(services.prio)
#######################################
## PLOT prioritizes community services in percent
p.services.prio <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.services.prio)


## 4.3) Recognition of engaged individual (entity = individual) within the community
df.indrecognition <- data.commwork[,44:59]
colnames(df.indrecognition) <- c("as social activist", "as helper or supporter", "as solution finder",
                                 "as influential person", "as counsellor/advisor", "as religious leader",
                                 "as traditional leader", "as community innovator", "as community worker/volunteer",
                                 "as community intellectual", "as community mediator or negotiator", 
                                 "as women activist", "as youth activist", "as recreationist", "as role model",
                                 "as informant")
data <- reshape(df.indrecognition, 
                varying = c("as social activist", "as helper or supporter", "as solution finder",
                            "as influential person", "as counsellor/advisor", "as religious leader",
                            "as traditional leader", "as community innovator", "as community worker/volunteer",
                            "as community intellectual", "as community mediator or negotiator", 
                            "as women activist", "as youth activist", "as recreationist", "as role model",
                            "as informant"), 
                v.names = "freq",
                timevar = "var", 
                times = c("as social activist", "as helper or supporter", "as solution finder",
                          "as influential person", "as counsellor/advisor", "as religious leader",
                          "as traditional leader", "as community innovator", "as community worker/volunteer",
                          "as community intellectual", "as community mediator or negotiator", 
                          "as women activist", "as youth activist", "as recreationist", "as role model",
                          "as informant"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.indrecognition)
#######################################
## PLOT individual's social recognition within the community in percent
p.indrecognition <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.indrecognition)

##  4.4) Skills of enganged individual (entity = individual) ** cypercafe -> plumbing, vender -> Dental care
df.indskills <- data.commwork[,61:87]
colnames(df.indskills) <- c("Electrician", "Car servicing", "Entrepreneur/vender", "Computer support",
                            "Plumbing/fitting", "Handicraft", "Veterinary technology", "Dental care",
                            "Pharmacist", "Carpentry/Joinery", "Shelter/housing mediation/negotiation",
                            "Food processing/services", "Hair styling and cosmetology", "Public safety",
                            "Children services", "Tailoring/fashion design", "Health care and social assistance",
                            "Manufacturing", "Facilitating training", "Beautician/styling", "Appliance repair and maintenance",
                            "Trucking", "Construction", "Mechanical", "Career counseling/support with formal documents",
                            "Bookkeeping, accounting, auditing", "Sign language")
data <- reshape(df.indskills, 
                varying = c("Electrician", "Car servicing", "Entrepreneur/vender", "Computer support",
                            "Plumbing/fitting", "Handicraft", "Veterinary technology", "Dental care",
                            "Pharmacist", "Carpentry/Joinery", "Shelter/housing mediation/negotiation",
                            "Food processing/services", "Hair styling and cosmetology", "Public safety",
                            "Children services", "Tailoring/fashion design", "Health care and social assistance",
                            "Manufacturing", "Facilitating training", "Beautician/styling", "Appliance repair and maintenance",
                            "Trucking", "Construction", "Mechanical", "Career counseling/support with formal documents",
                            "Bookkeeping, accounting, auditing", "Sign language"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Electrician", "Car servicing", "Entrepreneur/vender", "Computer support",
                          "Plumbing/fitting", "Handicraft", "Veterinary technology", "Dental care",
                          "Pharmacist", "Carpentry/Joinery", "Shelter/housing mediation/negotiation",
                          "Food processing/services", "Hair styling and cosmetology", "Public safety",
                          "Children services", "Tailoring/fashion design", "Health care and social assistance",
                          "Manufacturing", "Facilitating training", "Beautician/styling", "Appliance repair and maintenance",
                          "Trucking", "Construction", "Mechanical", "Career counseling/support with formal documents",
                          "Bookkeeping, accounting, auditing", "Sign language"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.indskills)
#######################################
## PLOT individual's skills within the community in percent
p.indskills <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.indskills)

##  4.5) Talents of enganged individual (entity = individual)
df.indtalents <- data.commwork[,89:99]
colnames(df.indtalents) <- c("Storytelling", "Literature writing", "Dancing", "Singing", "Athlete/sport playing",
                             "Innovation", "Acting", "Artisan", "Music playing/composing", "Commedian",
                             "Drawing/painting")
data <- reshape(df.indtalents, 
                varying = c("Storytelling", "Literature writing", "Dancing", "Singing", "Athlete/sport playing",
                            "Innovation", "Acting", "Artisan", "Music playing/composing", "Commedian",
                            "Drawing/painting"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Storytelling", "Literature writing", "Dancing", "Singing", "Athlete/sport playing",
                          "Innovation", "Acting", "Artisan", "Music playing/composing", "Commedian",
                          "Drawing/painting"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.indtalents)
#######################################
## PLOT individual's talents within the community in percent
p.indtalents <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.indtalents)


rm(data.commwork)
############################################################################################
############################################################################################
## 5) Referrals
data.referral <- data.process[,c(21,22,163:247)]

## 5.1) Refered services
df.refservices1 <- data.referral[,c(3,7:27)]
df.refservices2 <- data.referral[,c(31,35:55)]
df.refservices3 <- data.referral[,c(59,63:83)]
colnames(df.refservices1) <- c("Referral to", "Preventative activities", "Resolving inter-community disputes", "Technical/professional advices", "Facilitating access to services",
                               "Facilitating complaints to service provider", "Taking lead or participating in advocacy",
                               "Community dialogue", "Organizing or participating in community events ", 
                               "Helping people in need with specific support/action", "Supporting for alternative dispute resolution",
                               "Community building", "Empowering certain segment of community", "Identifying concerns, challenges and solutions",
                               "Facilitating feedback", "Skill and knowledge development support", "Language support",
                               "Taking or supporting community-led initiatives", "Sharing information on availability of and accessibility to the services",
                               "Identifying persons with specific needs", "Recreational activities", "Awareness activities") 
colnames(df.refservices2) <- c("Referral to", "Preventative activities", "Resolving inter-community disputes", "Technical/professional advices", "Facilitating access to services",
                               "Facilitating complaints to service provider", "Taking lead or participating in advocacy",
                               "Community dialogue", "Organizing or participating in community events ", 
                               "Helping people in need with specific support/action", "Supporting for alternative dispute resolution",
                               "Community building", "Empowering certain segment of community", "Identifying concerns, challenges and solutions",
                               "Facilitating feedback", "Skill and knowledge development support", "Language support",
                               "Taking or supporting community-led initiatives", "Sharing information on availability of and accessibility to the services",
                               "Identifying persons with specific needs", "Recreational activities", "Awareness activities") 
colnames(df.refservices3) <- c("Referral to", "Preventative activities", "Resolving inter-community disputes", "Technical/professional advices", "Facilitating access to services",
                               "Facilitating complaints to service provider", "Taking lead or participating in advocacy",
                               "Community dialogue", "Organizing or participating in community events ", 
                               "Helping people in need with specific support/action", "Supporting for alternative dispute resolution",
                               "Community building", "Empowering certain segment of community", "Identifying concerns, challenges and solutions",
                               "Facilitating feedback", "Skill and knowledge development support", "Language support",
                               "Taking or supporting community-led initiatives", "Sharing information on availability of and accessibility to the services",
                               "Identifying persons with specific needs", "Recreational activities", "Awareness activities") 
df.refservices <- rbind(df.refservices1, df.refservices2, df.refservices3)
rm(df.refservices1, df.refservices2, df.refservices3)
   
                                
                               
                                
                                
                                
                                
                                
                

rm(data.referral)


############################################################################################
############################################################################################
## 6) Targeted groups
data.target <- data.process[,248:274]

## 6.1) Percent of targeted groups
df.poc <- data.target[,2:8]
colnames(df.poc) <- c("Refugees/Asylum seekers", "Anybody who approaches", "Internally displaced persons",
  "People from this area", "People in mixed movements", "Host community members", "Returnees")
data <- reshape(df.poc, 
                varying = c("Refugees/Asylum seekers", "Anybody who approaches", "Internally displaced persons",
                            "People from this area", "People in mixed movements", "Host community members", "Returnees"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Refugees/Asylum seekers", "Anybody who approaches", "Internally displaced persons",
                          "People from this area", "People in mixed movements", "Host community members", "Returnees"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.poc)
#######################################
## PLOT targeted persons in percent
p.poc <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.poc)

## 6.2) Specific groups of concern
df.specific <- data.target[,10:27]
colnames(df.specific) <- c("Survivor of gender-based violence", "Socio-economically vulnerable",
"Persons with disabilities", "LGBTI persons", "Adolescent (10-14)", "Orphans", "Unaccompanied/separated children", 
"Minority community members", "People living with mental health and psychosocial problems", "Youth (15-24)",
"Women and girls", "No focus on a specific group", "Older persons (60 +)", "Marginalized members in the community",
"Single and house-bound individuals", "Children (< 18)","Unaccompanied older persons", "Female headed households")
data <- reshape(df.specific, 
                varying = c("Survivor of gender-based violence", "Socio-economically vulnerable",
                            "Persons with disabilities", "LGBTI persons", "Adolescent (10-14)", "Orphans", "Unaccompanied/separated children", 
                            "Minority community members", "People living with mental health and psychosocial problems", "Youth (15-24)",
                            "Women and girls", "No focus on a specific group", "Older persons (60 +)", "Marginalized members in the community",
                            "Single and house-bound individuals", "Children (< 18)","Unaccompanied older persons", "Female headed households"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Survivor of gender-based violence", "Socio-economically vulnerable",
                          "Persons with disabilities", "LGBTI persons", "Adolescent (10-14)", "Orphans", "Unaccompanied/separated children", 
                          "Minority community members", "People living with mental health and psychosocial problems", "Youth (15-24)",
                          "Women and girls", "No focus on a specific group", "Older persons (60 +)", "Marginalized members in the community",
                          "Single and house-bound individuals", "Children (< 18)","Unaccompanied older persons", "Female headed households"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.specific)
#######################################
## PLOT specific groups in percent
p.specific <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.specific)


rm(data.target)
############################################################################################
############################################################################################
## 7) Active community members
data.activememb <- data.process[,275:285]

## 7.1) Type of active community members
df.actmembers <- data.activememb[,2:10]
colnames(df.actmembers) <- c("Professionals", "Network members", "Recognized and knowledgeable community members",
                           "Skilled group of people from outside the community", "Collaborating agency or authority",
                           "Dedicated staff", "Influential community leaders and members", 
                           "Community volunteers", "Skilled community members")
data <- reshape(df.actmembers, 
                varying = c("Professionals", "Network members", "Recognized and knowledgeable community members",
                            "Skilled group of people from outside the community", "Collaborating agency or authority",
                            "Dedicated staff", "Influential community leaders and members", 
                            "Community volunteers", "Skilled community members"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Professionals", "Network members", "Recognized and knowledgeable community members",
                          "Skilled group of people from outside the community", "Collaborating agency or authority",
                          "Dedicated staff", "Influential community leaders and members", 
                          "Community volunteers", "Skilled community members"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.actmembers)
#######################################
## PLOT Type of active community members in percent
p.actmembers <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.actmembers)


## 7.2) Duration of community work
df.duration <- data.frame(data.activememb[,11])
data <- data.frame(table(df.duration))
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data <- data[which(data[,3] == "duration_services"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.duration)
#######################################
## PLOT conduciveness of meeting space in percent
p.duration <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.duration)




rm(data.activememb)
############################################################################################
############################################################################################
## 8) Communication channels
data.channels <- data.frame(data.process[,c(21,286:310)])

## 8.1) Type of used communication shannels
df.communication <- data.channels[,3:19]
colnames(df.communication) <- c("Through local social network", "Social/Community network",
  "WhatsApp/SMS/Viber", "Webpage", "e-newspaper", "Home visit", "Organizing events during the special days",
  "Blog", "Phone call", "Facebook", "Local Radio/FM", "Meeting in-person", "Neighborhood/community meeting",
  "Leaflets/broachers/posters", "Drama and community-based event", "Group discussion/meeting",
  "Email")
data <- reshape(df.communication, 
                varying = c("Through local social network", "Social/Community network",
                            "WhatsApp/SMS/Viber", "Webpage", "e-newspaper", "Home visit", "Organizing events during the special days",
                            "Blog", "Phone call", "Facebook", "Local Radio/FM", "Meeting in-person", "Neighborhood/community meeting",
                            "Leaflets/broachers/posters", "Drama and community-based event", "Group discussion/meeting",
                            "Email"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Through local social network", "Social/Community network",
                          "WhatsApp/SMS/Viber", "Webpage", "e-newspaper", "Home visit", "Organizing events during the special days",
                          "Blog", "Phone call", "Facebook", "Local Radio/FM", "Meeting in-person", "Neighborhood/community meeting",
                          "Leaflets/broachers/posters", "Drama and community-based event", "Group discussion/meeting",
                          "Email"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.communication)
#######################################
## PLOT specific groups in percent
p.channels <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.channels)
  

## 8.2) List of manual entries of specific communication channels  
df.addresses <- data.channels[,c(1,20:26)]   
colnames(df.addresses) <- c("Community name", "Webpage", "Facebook", "Blog", "Email", "Whatsapp/SMS/Vibor", "Local Radio/FM", "E-Newspaper")
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.8)),
  colhead = list(fg_params=list(cex = 0.9)),
  rowhead = list(fg_params=list(cex = 0.8)))
p.addresses <- tableGrob(df.addresses, theme=mytheme)
plot(p.addresses)
rm(df.addresses)  
  

rm(data.channels)
############################################################################################
############################################################################################
## 9) Meeting space
data.space <- data.process[,311:332]

## 9.1) Type of meeting space
df.meetingspace <- data.space[,2:13]
colnames(df.meetingspace) <- c("School or educational institution", "No specific place", "Office of the local authority/municipality",
             "Youth Club", "Park or open space", "Public Place/Building", "Community Event Space", "Sport Club",
             "Community Center", "Office of a private entity", "Office of the Community", "Religious Place")
data <- reshape(df.meetingspace, 
                varying = c("School or educational institution", "No specific place", "Office of the local authority/municipality",
                            "Youth Club", "Park or open space", "Public Place/Building", "Community Event Space", "Sport Club",
                            "Community Center", "Office of a private entity", "Office of the Community", "Religious Place"), 
                v.names = "freq",
                timevar = "var", 
                times = c("School or educational institution", "No specific place", "Office of the local authority/municipality",
                          "Youth Club", "Park or open space", "Public Place/Building", "Community Event Space", "Sport Club",
                          "Community Center", "Office of a private entity", "Office of the Community", "Religious Place"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.meetingspace)
#######################################
## PLOT type of meeting space in percent
p.meetingspace <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.meetingspace)

## 9.2) Type of access
df.access <- data.space[,15:21]
colnames(df.access) <- c("On ad-hoc basis", "Own space", "Rental basis", "Freely available",
                         "Randomly used", "Public space", "On pre-agreement basis")
data <- reshape(df.access, 
                varying = c("On ad-hoc basis", "Own space", "Rental basis", "Freely available",
                            "Randomly used", "Public space", "On pre-agreement basis"), 
                v.names = "freq",
                timevar = "var", 
                times = c("On ad-hoc basis", "Own space", "Rental basis", "Freely available",
                          "Randomly used", "Public space", "On pre-agreement basis"), 
                new.row.names = 1:2000,
                direction = "long")
data <- data[!is.na(data$freq),]
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.access)
#######################################
## PLOT type access in percent
p.access <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.access)

### 9.2) Conduciveness
df.conduciveness <- data.space[,22]
data <- data.frame(table(df.conduciveness))
colnames(data) <- c("name", "freq")
data <- merge(data, form.or, by = "name")
data$freq <- as.numeric(data$freq)
data <- data[which(data[,3] == "conduciveness"),]
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.conduciveness)
#######################################
## PLOT conduciveness of meeting space in percent
p.conduciveness <- ggplot(data=data, aes(x=label, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.conduciveness)


rm(data.space)
############################################################################################
############################################################################################
## 10) Community challenges
data.challenges <- data.process[,333:339]

df.challenges <- data.challenges[,2:6]
colnames(df.challenges) <- c("Having more specific skills to provide more services", "Having better communication channels", 
  "Having a better network with other communities", "Having more actively engaged community members", 
  "Having better or more community space")
data <- reshape(df.challenges, 
                varying = c("Having more specific skills to provide more services", "Having better communication channels", 
                            "Having a better network with other communities", "Having more actively engaged community members", 
                            "Having better or more community space"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Having more specific skills to provide more services", "Having better communication channels", 
                          "Having a better network with other communities", "Having more actively engaged community members", 
                          "Having better or more community space"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.challenges)
#######################################
## PLOT community challenges in percent
p.challenges <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.challenges)  
  

rm(data.challenges)
############################################################################################
############################################################################################
## 11) Community collaboration
data.collab <- data.process[,340:348]

## 11.1) Reward to motivate community work
df.reward <- data.collab[,2:8]
colnames(df.reward) <- c("Getting access to a forum to share experiences and ideas", 
  "Appreciation through certificate", "Specific support linked to the work", "Providing social recognition",
  "Keeping in the communication loop", "Knowing reasons for volunteering", "Promotion of the good work")
data <- reshape(df.reward, 
                varying = c("Getting access to a forum to share experiences and ideas", 
                           "Appreciation through certificate", "Specific support linked to the work", "Providing social recognition",
                           "Keeping in the communication loop", "Knowing reasons for volunteering", "Promotion of the good work"), 
                v.names = "freq",
                timevar = "var", 
                times = c("Getting access to a forum to share experiences and ideas", 
                         "Appreciation through certificate", "Specific support linked to the work", "Providing social recognition",
                         "Keeping in the communication loop", "Knowing reasons for volunteering", "Promotion of the good work"), 
                new.row.names = 1:2000,
                direction = "long")
data$freq <- as.numeric(data$freq/sum(data$freq))
rm(df.reward)
#######################################
## PLOT community challenges in percent
p.reward <- ggplot(data=data, aes(x=var, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.challenges) 



## 11.2) Collaboration with UNHCR
df.collabunhcr <- data.frame(data.collab[,9])
data <- data.frame(table(df.collabunhcr))
colnames(data) <- c("name", "freq")
data$freq <- as.numeric(data$freq/sum(data$freq))

rm(df.collabunhcr)
rm(data.collab)
#######################################
## PLOT conduciveness of meeting space in percent
p.collabunhcr <- ggplot(data=data, aes(x=name, y=freq)) + 
  geom_bar(stat="identity", fill="#0072bc", colour="#3f3f3f",  alpha=2/3) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_plot() +
  theme(panel.grid.major.x = element_line(color = "gray10", size = 0.2),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.text.x = element_text(size=22, face="bold", color="gray30",angle = 0, hjust = 0),
        axis.ticks.x = element_line(colour = "#a8a8a8", size = 0.2),
        axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2)) +
  #scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  guides(fill=FALSE) +
  xlab("") + ylab("") +
  ggtitle(label="")+
  coord_flip()
plot(p.collabunhcr)



############################################################################################
############################################################################################
## 12) Community contact details
df.contact <- data.process[,c(21,22,350,351,349,358)]
colnames(df.contact) <- c("Community name", "Community type", "City", "Neighborhood", "Phone Number", "Email")
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.8)),
  colhead = list(fg_params=list(cex = 0.9)),
  rowhead = list(fg_params=list(cex = 0.8)))
p.contact <- tableGrob(df.contact, theme=mytheme)
plot(p.contact)
rm(df.contact)  

rm(data.contact)

# ############################################################################################
# ############################################################################################
# ## 13) Creation of overview list
# df.communityoverview <- data.process[,c(25,22,8,9,26,48,49,50,51,85)]


