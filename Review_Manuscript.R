######### Murphy et al: Systematic Review ##########

setwd("/Users/kaitlynmurphy/Desktop")
getwd()

#install these packages
install.packages('ggplot2')
install.packages('tidyverse')
install.packages("extrafontdb")
install.packages("extrafont")
install.packages("reshape2")

#call on these libraries
library(ggplot2)
library(tidyverse)
library(extrafontdb)
library(extrafont)
library(reshape2)

#Call on the entire dataset- 'Articles.csv'
datum_full=read.csv(file.choose())
head(datum_full)

#Call on the subset dataset used for analyses - 'DATA.csv'
datum=read.csv(file.choose())
head(datum)

#Call on the dataset for abundance plot - 'Abundance.csv'
abund=read.csv(file.choose())
head(abund)

####Literature Search####

#####Plot of Timeline#####

#Figure out how many papers there are by year and make them into a data frame

print(nrow(datum_full[datum_full$YEAR == "2010",]))
print(nrow(datum_full[datum_full$YEAR == "2011",]))
print(nrow(datum_full[datum_full$YEAR == "2012",]))
print(nrow(datum_full[datum_full$YEAR == "2013",]))
print(nrow(datum_full[datum_full$YEAR == "2014",]))
print(nrow(datum_full[datum_full$YEAR == "2015",]))
print(nrow(datum_full[datum_full$YEAR == "2016",]))
print(nrow(datum_full[datum_full$YEAR == "2017",]))
print(nrow(datum_full[datum_full$YEAR == "2018",]))
print(nrow(datum_full[datum_full$YEAR == "2019",]))
print(nrow(datum_full[datum_full$YEAR == "2020",]))
print(nrow(datum_full[datum_full$YEAR == "2021",]))
print(nrow(datum_full[datum_full$YEAR == "2022",]))

YEAR <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
NUMBER <- c(24,25,26,26,44,45,44,68,75,93,100,107,112)

years <- data.frame(YEAR,NUMBER)
print(years)

#Make the plot using the main datafram (Articles.csv)

tiff("Figure1A.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(data=datum_full, aes(x=YEAR, fill=Y.N)) +
  geom_bar(aes(color=Y.N, fill=Y.N), width = 0.6) +
  #stat_bin(bins=30) + 
  scale_fill_manual(values = c("#145784", "#CCDDEA")) +
  scale_color_manual(values = c("#174463", "#7EA9CA")) +             
  scale_x_continuous(breaks=seq(2010,2022,1)) +
  scale_y_continuous(breaks=seq(0,120,10), lim = c(0,120)) +
  labs(x = "Years",
       y = "Number of papers") +
  theme_classic() + 
  theme(legend.position="top") +
  theme(
    axis.title.x  = element_text(family = "Microsoft Sans Serif", color = "black"),
    axis.title.y  = element_text(family = "Microsoft Sans Serif", color = "black"),
    axis.text.x   = element_text(family = "Microsoft Sans Serif", color = "black"),
    axis.text.y   = element_text(family = "Microsoft Sans Serif", color = "black"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"))

dev.off()

#####Plot of Exclusion Criteria#####

help(extrafont)
font_import()
fonttable()

#Bar chart of exclusion criteria (not included in paper)

palette <- c("#94A088", "#E48312", "#CCDDEA", "#DAD7B3")
lines <-c("#637052", "#BD582C","#7EA9CA", "#A39B4E")

tiff("barchart.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(data=datum_full, aes(x=REASON)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill=palette, color=lines) +
  #data=subset(datum_full,REASON==1),fill="#94A088", color="#637052", bins=7) + 
  #geom_bar(aes(y = (..count..)/sum(..count..)),data=subset(datum_full,REASON==2),fill="#E48312", color="#BD582C", bins=7) +
  #geom_bar(aes(y = (..count..)/sum(..count..)),data=subset(datum_full,REASON==3),fill="#CCDDEA", color="#40749B", bins=7) +
  #geom_bar(aes(y = (..count..)/sum(..count..)),data=subset(datum_full,REASON==4),fill="#C2BC80", color="#9B8357", bins=7) +
  #scale_y_continuous(breaks=seq(0,120,10), lim = c(0,120)) +
  scale_y_continuous(labels=percent) +
  labs(x = "Reason for Exclusion",
       y = "Percentage of papers") +
  theme_bw()

dev.off()

#Pie chart of exclusion criteria 

#Remove rows if empty in 'REASON' column
nafile <- datum_full[-which(is.na(datum_full$REASON)), ]
                 
print(nrow(nafile[nafile$REASON == "1",]))
print(nrow(nafile[nafile$REASON == "2",]))
print(nrow(nafile[nafile$REASON == "3",]))
print(nrow(nafile[nafile$REASON == "4",]))

#Categorical data
REASON <- c("Did not sample from invertebrate/vertebrate", "Did not collect whole microbiome sample", "Did not use culture independent methods", "Did not sample from both mother and offspring")
NUMBER <- c(131,317,35,142)

#Store the variable as a data frame
reason <- data.frame(REASON,NUMBER)
print(reason)

#Make the pie chart
cols <- c("#CCDDEA", "#DAD7B3", "#637052", "#BD582C")

tiff("Figure1B.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(reason, aes(x = "", y = NUMBER, fill = REASON)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#BD582C", "#CCDDEA", "#DAD7B3", "#637052")) +
  geom_text(aes(x= 1.25, label = percent(NUMBER / sum(NUMBER))),
            position = position_stack(vjust = 0.5), size = 3, fontface = "bold") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    axis.text.y   = element_text(family = "Microsoft Sans Serif"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="right") 

dev.off()

####Met Inclusion Criteria####

#####Plot for Taxonomy#####

print(nrow(datum[datum$TAXA == "Fish",]))
print(nrow(datum[datum$TAXA == "Mammal",]))
print(nrow(datum[datum$TAXA == "Bird",]))
print(nrow(datum[datum$TAXA == "Reptile",]))
print(nrow(datum[datum$TAXA == "Invertebrate",]))

#Categorical data
TAXA <- c("Fish", "Mammal", "Bird", "Reptile", "Invertebrate")
NUMBER <- c(3,66,6,2,29)

#Store the variable as a data frame
taxa <- data.frame(TAXA,NUMBER)
print(taxa)

#Make the pie chart (not included in paper)
cols <- c("#CCDDEA", "#DAD7B3", "#637052", "#BD582C","#7EA9CA")

tiff("PieChart1.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(taxa, aes(x = "", y = NUMBER, fill = TAXA)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#BD582C", "#CCDDEA", "#DAD7B3", "#637052","#7EA9CA")) +
  geom_text(aes(x= 1.25, label = percent(NUMBER / sum(NUMBER))),
            position = position_stack(vjust = 0.5), size = 3, fontface = "bold") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    axis.text.y   = element_text(family = "Microsoft Sans Serif"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="bottom") 

dev.off()

#####Plot for Reproductive Mode#####

print(nrow(datum[datum$REPRO == "Oviparous",]))
print(nrow(datum[datum$REPRO == "Viviparous",]))
print(nrow(datum[datum$REPRO == "Other",]))

#Categorical data
REPRO <- c("Oviparous", "Viviparous", "Other")
NUMBER <- c(25,68,13)

#Store the variable as a data frame
repro <- data.frame(REPRO,NUMBER)
print(repro)

#Make the pie chart (not included in paper)

tiff("PieChart2.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(repro, aes(x = "", y = NUMBER, fill = REPRO)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#637052","#7EA9CA")) +
  geom_text(aes(x= 1.25, label = percent(NUMBER / sum(NUMBER))),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    axis.text.y   = element_text(family = "Microsoft Sans Serif"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="bottom") 

dev.off()

#####Plot for Study Design#####

print(nrow(datum[datum$DESIGN == "Experimental",]))
print(nrow(datum[datum$DESIGN == "Observational",]))

#Categorical data
DESIGN <- c("Experimental", "Observational")
NUMBER <- c(70,36)

#Store the variable as a data frame
design <- data.frame(DESIGN,NUMBER)
print(design)

#Make the pie chart (not included in paper)

tiff("PieChart3.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(design, aes(x = "", y = NUMBER, fill = DESIGN )) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#637052","#7EA9CA")) +
  #geom_text(aes(x= 1.25, label = NUMBER),
  #          position = position_stack(vjust = 0.5), size = 5, fontface = "bold"))
  geom_text(aes(x= 1, label = percent(NUMBER / sum(NUMBER))),
    position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    axis.text.y   = element_text(family = "Microsoft Sans Serif"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="bottom") 

dev.off()

#####Plot for Functionality#####

print(nrow(datum[datum$FUNCTION == "Y",]))
print(nrow(datum[datum$FUNCTION == "N",]))

#Categorical data
FUNCTION <- c("Measured microbial function", "Did not measure microbial function")
NUMBER <- c(43,63)

#Store the variable as a data frame
functions <- data.frame(FUNCTION,NUMBER)
print(functions)

#Make the pie chart (not included in paper)

tiff("PieChart4.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(functions, aes(x = "", y = NUMBER, fill = FUNCTION )) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#BD582C", "#DAD7B3")) +
  geom_text(aes(x= 1, label = percent(NUMBER / sum(NUMBER))),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    axis.text.y   = element_text(family = "Microsoft Sans Serif"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="bottom") 

dev.off()

#####Plot for Offspring Phenotype#####

print(nrow(datum[datum$PHEN == "Y",]))
print(nrow(datum[datum$PHEN == "N",]))

#Categorical data
PHEN <- c("Measured phenotypes", "Did not measure phenotypes")
NUMBER <- c(49,57)

#Store the variable as a data frame
phenotype <- data.frame(PHEN,NUMBER)
print(phenotype)

#Make the pie chart (not included in paper)

tiff("PieChart5.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(phenotype, aes(x = "", y = NUMBER, fill = PHEN )) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#174463", "#CCDDEA")) +
  geom_text(aes(x= 1, label = percent(NUMBER / sum(NUMBER))),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    axis.text.y   = element_text(family = "Microsoft Sans Serif"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="bottom") 

dev.off()

#####Plot for Sampling Regions#####

# Simple Bar Plot
mom <- table(datum$MOM_REGION)
print(mom)
barplot(mom,xlab="Sampling Region")

# Simple Bar Plot
offspring <- table(datum$O_REGION)
print(offspring)
summary(offspring)

tiff("offspring.tiff", width = 10, height = 4, units = 'in', res = 300)

barplot(offspring,xlab="Sampling Region")

dev.off()

#####Sequencing type#####

# Simple Bar Plot
seq <- table(datum$SEQUENCING)
print(seq)
barplot(seq,xlab="Sequencing type")

#####Abundance Plot#####

#Make this color palette
fillcols<- c( "#CCDDEA", "#145784","#94A088", "#E48312","#DAD7B3", "white", "#7EA9CA", "#174463", "#637052", "#BD582C","#A39B4E", "lightgray", "brown","lightpink") 

#Remove NA rows
abund1 <- abund[rowSums(is.na(abund)) == 0, ] 

#Format data for bar chart
abund2 <- melt(abund1, id.vars = "Info", variable.name = "types")

tiff("Figure2.tiff", width = 8, height = 4, units = 'in', res = 300)

ggplot(abund2, aes(x = Info, y = value, fill = types)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.9, color = "black") +
  scale_fill_manual(values = fillcols) +
  labs(x = "Article Factors",
       y = "Abundance Percentage") +
  theme_classic() +
  theme(
    axis.title.x  = element_text(family = "Microsoft Sans Serif", color = "black", vjust = -1.5),
    axis.title.y  = element_text(family = "Microsoft Sans Serif", color = "black"),
    axis.text.x   = element_text(family = "Microsoft Sans Serif", angle = 25, vjust = 0.5, color = "black"),
    axis.text.y   = element_text(family = "Microsoft Sans Serif", color = "black"),
    legend.text = element_text(family = "Microsoft Sans Serif"),
    legend.title = element_text(family = "Microsoft Sans Serif"),
    legend.position="right")

dev.off()

####Bioinformatics Pipeline Plots####

#####Alpha Diversity#####

#ASV vs. OTU Plot

print(nrow(datum[datum$OTU == "Y",]))
print(nrow(datum[datum$OTU == "N",]))

#Categorical data
OTU <- c("Used OTUs", "Used ASVs")
NUMBER <- c(65,41)

#Store the variable as a data frame
otu <- data.frame(OTU,NUMBER)
print(otu)

#Shannon's Plot

print(nrow(datum[datum$SHANNON == "Y",]))
print(nrow(datum[datum$SHANNON == "N",]))

#Categorical data
SHANNON <- c("Used Shannon's Index", "Did not use Shannon's Index")
NUMBER <- c(73,33)

#Store the variable as a data frame
shannon <- data.frame(SHANNON,NUMBER)
print(shannon)

#####Beta Diversity#####

#Bray-Curtis Plot

print(nrow(datum[datum$BC == "Y",]))
print(nrow(datum[datum$BC == "N",]))

#Categorical data
BC <- c("Used Bray-Curtis Metric", "Did not use Bray-Curtis Metric")
NUMBER <- c(51,55)

#Store the variable as a data frame
bc <- data.frame(BC,NUMBER)
print(bc)

#Weighted Unifrac

print(nrow(datum[datum$UNIFRAC == "Y",]))
print(nrow(datum[datum$UNIFRAC == "N",]))

#Categorical data
UNIFRAC <- c("Used Weighted Unifrac distances", "Did not use Weighted Unifrac distances")
NUMBER <- c(40,66)

#Store the variable as a data frame
unifrac <- data.frame(UNIFRAC,NUMBER)
print(unifrac)

#PCOA Plot

print(nrow(datum[datum$PCOA == "Y",]))
print(nrow(datum[datum$PCOA == "N",]))

#Categorical data
PCOA <- c("Used a PCoA", "Did not use a PCoA")
NUMBER <- c(60,46)

#Store the variable as a data frame
pcoa <- data.frame(PCOA,NUMBER)
print(pcoa)

#PERMANOVA

print(nrow(datum[datum$PERMANOVA == "Y",]))
print(nrow(datum[datum$PERMANOVA == "N",]))

#Categorical data
PERMANOVA <- c("Used a PERMANOVA", "Did not use a PERMANOVA")
NUMBER <- c(52,54)

#Store the variable as a data frame
permanova <- data.frame(PERMANOVA,NUMBER)
print(permanova)

#####Overlap#####

print(nrow(datum[datum$OVERLAP == "Y",]))
print(nrow(datum[datum$OVERLAP == "N",]))

#Categorical data
OVERLAP <- c("Reported overlap", "Reported overlap")
NUMBER <- c(91,15)

#Store the variable as a data frame
overlap <- data.frame(OVERLAP,NUMBER)
print(overlap)