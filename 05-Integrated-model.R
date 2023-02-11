
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS :
#
# One single integrated model  
#
####################################################################################################

library(data.table) 
library(stats)
library(lme4)
library(nlme)
library(ggridges)
library(ggplot2)
library(tidyverse)
#library(DHARMa)
library(ggeffects)

# -- set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1/data")
#setwd("C:/Users/kiran/Dropbox/PAPER/Clara")

# -- delete all objects
rm(list=ls())

# -- load data 
AllBioDataStandardized  <- as.data.frame(fread("AllBioData_StandardizedLongFormat310123.csv"))
AllFunctionsDataStandardized <- as.data.frame(fread("AllFunctionsDataStandardized_LongFormat250123.csv"))

# - set diversity level of control plots to 0 (no planted trees)

ind = which(AllBioDataStandardized$Treatment=="Control")
AllBioDataStandardized[ind,]$DivLev = 0
ind = which(AllFunctionsDataStandardized$Treatment=="Control")
AllFunctionsDataStandardized[ind,]$DivLev = 0

# -- log-transform the data 
AllBioDataStandardized$SizeLog          <- log(AllBioDataStandardized$Size)
AllBioDataStandardized$DivLevLog        <- log(AllBioDataStandardized$DivLev + 1)
AllFunctionsDataStandardized$SizeLog    <- log(AllFunctionsDataStandardized$Size)
AllFunctionsDataStandardized$DivLevLog  <- log(AllFunctionsDataStandardized$DivLev + 1)

# -- convert as factor 
AllBioDataStandardized<-AllBioDataStandardized %>%
  mutate_at(vars(Treatment, PlotID, Composition, Hill, Taxa), factor)

AllFunctionsDataStandardized<-AllFunctionsDataStandardized %>%
  mutate_at(vars(Treatment, PlotID, Composition, Function), factor)

################################################################
# -- run the linear models  & linear mixed effect models

# -- for biodiversity 

# species richness 

AllBioDataStandardized$Treatment <- relevel(AllBioDataStandardized$Treatment, ref = "Control")
 
m1 <- lme(Diversity ~ Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog
           + Taxa +
             Taxa:(Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog),
           random = ~1| PlotID, 
           data = AllBioDataStandardized[which(AllBioDataStandardized$Hill=="Species richness"),])

anova(m1)
x11(); plot(m1)
x11(); qqnorm(m1)
summary(m1)
AIC(m1)

capture.output(anova(m1),file="lme_SpeciesRichness_Standardized.doc")
capture.output(summary(m1),file="lme_SpeciesRichness_Standardized_summary.doc")

# Shannon 
m2 <- lme(Diversity ~ Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog
           + Taxa +
             Taxa:(Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog),
           random = ~1| PlotID, 
           data = AllBioDataStandardized[which(AllBioDataStandardized$Hill=="Shannon diversity"),])

anova(m2)
x11(); plot(m2)
x11(); qqnorm(m2)
summary(m3b)
AIC(m2)

capture.output(anova(m2),file="lme_ShannonDiversity_Standardized.doc")
capture.output(summary(m2),file="lme_ShannonDiversity_Standardized_summary.doc")

# simpson diversity 
m3 <- lme(Diversity ~ Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog
           + Taxa +
             Taxa:(Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog),
           random = ~1| PlotID, 
           data = AllBioDataStandardized[which(AllBioDataStandardized$Hill=="Simpson diversity"),])

anova(m3)
x11(); plot(m3)
x11(); qqnorm(m3)
summary(m3)
AIC(m3)

capture.output(anova(m3),file="lme_SimpsonDiversity_Standardized.doc")
capture.output(summary(m3),file="lme_SimpsonDiversity_Standardized_summary.doc")

# -- for ecosystem functioning 

m4 <- lme(value ~ Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog
           + Function +
             Function:(Treatment + SizeLog + DivLevLog + SizeLog:DivLevLog),
           random = ~1| PlotID, 
           data = AllFunctionsDataStandardized)

anova(m4)
x11(); plot(m4)
x11(); qqnorm(m4)
summary(m4)
AIC(m4)

capture.output(anova(m4),file="lme_Functions_Standardized.doc")
capture.output(summary(m4),file="lme_Functions_Standardized_summary.doc")

# evaluate the effects 
ggpredict(m1, term="Treatment")
ggpredict(m2, term="Treatment")
ggpredict(m3, term="Treatment")
ggpredict(m4, term="Treatment")

ggpredict(m1, term="SizeLog")
ggpredict(m2, term="SizeLog")
ggpredict(m3, term="SizeLog")
ggpredict(m4, term="SizeLog")

ggpredict(m1, term=c("Treatment","Taxa"))
ggpredict(m1, term=c("SizeLog","Taxa"))
ggpredict(m1, term=c("DivLevLog","Taxa"))

ggpredict(m4, term=c("DivLevLog","Function"))
ggpredict(m3, term=c("DivLevLog","Taxa"))


#################################################################
# Plotting 

library(sjlabelled)

# using the linear mixed effect models for the biodiversity

dat <- ggeffect(m3, terms = c("Treatment", "Taxa"))
x11(); x11(); plot(dat, facet = TRUE, colors = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61",
                                                 "#fee08b", "#e6f598", "#abdda4", "#66c2a5",
                                                 "#3288bd", "#5e4fa2"))

dat1 <- ggeffect(m2, terms = c("SizeLog", "Taxa"))
x11(); plot(dat1, facet = TRUE, colors = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61",
                                           "#fee08b", "#e6f598", "#abdda4", "#66c2a5",
                                           "#3288bd", "#5e4fa2"))

dat1 <- ggeffect(m3, terms = c("DivLevLog", "Taxa"))
x11(); plot(dat1, facet = TRUE, colors = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61",
                                           "#fee08b", "#e6f598", "#abdda4", "#66c2a5",
                                           "#3288bd", "#5e4fa2"))


# using the linear mixed effect model for the ecosystem functioning

library(sjlabelled)
dat2 <- ggeffect(m4, terms = c("Treatment","Function"))
x11(); plot(dat2, facet = T) + scale_colour_hue() 

dat3 <- ggeffect(m4, terms = c("SizeLog","Function"))
x11(); plot(dat3, facet = T)+ scale_colour_hue() 

dat3 <- ggeffect(m4, terms = c("DivLevLog","Function"))
x11(); plot(dat3, facet = T)+ scale_colour_hue() 

