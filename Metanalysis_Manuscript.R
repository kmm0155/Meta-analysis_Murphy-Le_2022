######### Murphhy and Le: Metanalysis ##########

setwd("/Users/kaitlynmurphy/Desktop")
getwd()

#install these packages
install.packages('lme4')
install.packages('nlme')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('metafor')
install.packages('metaviz')
install.packages('glue')
install.packages("patchwork")
install.packages("R.rsp")
install.packages("pacman")

#call on these libraries
library(lme4)
library(nlme)
library(ggplot2)
library(tidyverse)
library(metafor)
library(metaviz)
library(glue)
library(orchaRd)
library(patchwork)

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)

#Call on your dataset
datum=read.csv(file.choose())
head(datum)

#### Shannon's Diversity ####

resultsh=lme(Shannons_Mothers~Shannons_Offspring,random=~1|Title,data=datum,na.action=na.omit)
summary(resultsh)
plot(Shannons_Mothers~Shannons_Offspring,col=(c("blue")),data=datum)

resultsh=lme(Shannons_Mothers~Shannons_Offspring+Reproduction,random=~1|Title,data=datum,na.action=na.omit)
summary(resultsh)

tiff("Figure1.tiff", width = 4, height = 4, units = 'in', res = 300)

plot(Shannons_Mothers~Shannons_Offspring, data=datum,
        col=(c("blue")),
        xlab="Offspring Shannon's Diversity", ylab="Mother's Shannon's Diversity")
abline(lm(Shannons_Mothers~Shannons_Offspring,data=datum),col='black')

dev.off()

#### OTU: Hedge's G ####

#use escalc function to get Hedge's G
datum2=escalc(measure="SMDH", n1i=Total_Offspring, m1i=OTU_Offspring, sd1i=OTU_Offspring_SD, n2i=Total_Mothers, m2i=OTU_Mothers, sd2i=OTU_Mothers_SD, data=datum)

##results from escalc below
datum2

##save .csv file with new data
write.csv(datum2,'datum2.csv')

#### WEIGHTED random effects ####
results=rma(Shannons_Mothers, Shannons_Offspring, data=datum, method="REML") ##You can change heterogeneity estimator with method
results
forest(results)
funnel(results)

#### UNWEIGHTED meta-analysis ####
results1=rma(yi, vi, data=datum2, weighted=FALSE)  
results1
forest(results1)  ##note that boxes for effect size weights are same showing equal weights across effect sizes (thus unweighted)
funnel(results1)

#### Random effects meta-analysis ####
#run meta-analysis - random effects - INCLUDE RANDOM FACTOR FOR STUDY 
#(USEFUL IF MULTIPLE EFFECTS FROM SAME PAPER)  - NOTE RMA.MV CHANGE
results2=rma.mv(yi, vi, data=datum2, method="REML", random =~ 1 | AUTHOR)
results2
forest(results2)
funnel(results2)

### Adding Moderators ####
#To add moderators with intercept (compares intercept (one treatment) with null and other treatments to intercept treatment)
results3=rma.mv(yi, vi, mods=~CLASS_SIZE , data=datum2,method="REML", random =~ 1 | AUTHOR)
results3

## you can change reference treatment with relevel function
results3a=rma.mv(yi, vi, mods=~ relevel(factor(CLASS_SIZE), ref="Small"), data=datum2, method="REML")
results3a

## Add moderators without intercept ##
#To add moderators without intercept (compares all treatments to null; useful for getting data to create figures):  
results4=rma.mv(Shannons_Mothers, Shannons_Offspring, mods=~ Reproduction, data=datum, method="REML", random =~ 1 | Title)
results4
forest(results4)
funnel(results4)

# To add CONTINOUS moderator
#Not possible with this dataset
#(METAREGRESSION) with intercept (compares intercept (one treatment) with null and other treatments to intercept treatment)
results5=rma.mv(yi, vi, mods=~ percent, data=datum2,method="REML", random =~ 1 | study)
results5
forest(results5)
funnel(results5)
regplot(results5)  ##nice bubble plot

#### Publication Bias Tests ####
## leave-one-out analysis - https://wviechtb.github.io/metafor/reference/leave1out.html
leave1out(results)

## influence - https://wviechtb.github.io/metafor/reference/influence.rma.uni.html
influence(results)

#Fail safe N analysis
#conduct fail safe N analysis - https://rdrr.io/cran/metafor/man/fsn.html
fsn(Shannons_Mothers, Shannons_Offspring, data=datum)
fsn(yi, data=datum2, type="Orwin", target=log(0.95)) # target corresponds to a 5% risk reduction
fsn(yi, vi, data=datum2, type="Orwin", weighted=TRUE, target=log(0.95))
fsn(Shannons_Mothers, Shannons_Offspring, data=datum, type="Rosenberg")

#Trim and fill analysis
# conduct trim and fill analysis - https://rdrr.io/cran/metafor/man/trimfill.html
res.tf <- trimfill(results)
res.tf
funnel(res.tf, legend=TRUE, cex=1.2)

#Egger test regression analysis
##conduct classical Egger test regression analysis - https://wviechtb.github.io/metafor/reference/regtest.html
regtest(results, model="lm")

##conduct mixed-effects meta-regression version of the Egger test - https://wviechtb.github.io/metafor/reference/regtest.html
regtest(results)

#Rank Correlation test
### carry out the rank correlation test - https://wviechtb.github.io/metafor/reference/ranktest.html
ranktest(results)

#### Make a forest plot ####
viz_forest(x = datum[, c("Shannons_Mothers", "Shannons_Offspring")], study_labels = datum[, c("Title")],  summary_label = "Summary effect", xlab = "Shannon's Diversity")
viz_forest(x = datum2[, c("yi", "vi")], 
           group = datum[, "AUTHOR"], 
           study_labels = datum[, "AUTHOR"], 
           xlab = "Cohen's d",
           col = "Greys",
           variant = "rain")

##### Make an orchard plot ####
p1 <- orchard_plot(results, xlab = "Correlation Coefficient")
p2 <- orchard_plot(res13, mod = "Pig_type", xlab = "Correlation Coefficient")
p3 <- orchard_plot(res14, mod = "Migration", xlab = "Correlation Coefficient") 

p1
p2/p3
