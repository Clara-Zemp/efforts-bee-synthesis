
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS :
#
# Calculation of Multi-functonality and multi-diversity based on (1) averaging and (2) thresholds.
#
####################################################################################################

#library(devtools)
#install_github("jebyrnes/multifunc")

library(multifunc) 
library(ggplot2)
library(gridExtra)
library(data.table)
library(vegan)


# delete all objects
rm(list=ls())

# set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1")

# load data 
biodiv  <- as.data.frame(fread("2022-11-10-Biodiv.H0.csv"))
biodiv  <- as.data.frame(fread("2022-11-10-Biodiv.H1.csv"))
biodiv  <- as.data.frame(fread("2022-11-10-Biodiv.H2.csv"))
ef        <- as.data.frame(fread("2022-11-10-EF.csv"))


# -- select the variables to be assessed 

## For ecosystem functions 
EFnames = c(#"Tot_BA_inc",
          "Y.NetChange",
          "total_AGB",
          "decomp.all.t1",
          "Kfs","ETmean","microclimate_buffer",
          "soil_decomp_metabo", "soil_herbi_metabo","soil_predator_metabo",
          "insect_predator_N", "vert_insecti_activity",
          "soilNC","soilP","soil_decompact",
          "litter_weight",
          "PollRate",
          "insect_pollinator_N","invasion_resist","seed_N_native")

# get the corresponding indices 
indEF = which(names(ef) %in% EFnames )
#df <- ef[,..indEF] # get the data (optional)

# fill missing data
ef$decomp.all.t1[is.na(ef$decomp.all.t1)] = median(ef$decomp.all.t1[53:56],na.rm = TRUE) # plot 56 is missing: fill in with median values from other control plot

## For biodiversity 

#Bionames = c("bat_H0","bird_H0","insect_H0","fungi_H0","bact_H0","spont_tree_H0_rare","herbs_H0","soil_fauna_H0")
#Bionames = c("bat_H1","bird_H1","insect_H1","fungi_H1","bact_H1","spont_tree_H1_rare","herbs_H1","soil_fauna_H1")
#Bionames = c("bat_H2","bird_H2","insect_H2","fungi_H2","bact_H2","spont_tree_H2_rare","herbs_H2","soil_fauna_H2")

Bionames = names(biodiv) # -- select all 

indBioDiv = which(names(biodiv) %in% Bionames )
# df <- biodiv[,indBioDiv] # get the data (optional)

# fill missing data
biodiv$pollen_H0[is.na(biodiv$pollen_H0)] = median(biodiv$pollen_H0[1:52],na.rm = TRUE)
biodiv$pollen_H1[is.na(biodiv$pollen_H1)] = median(biodiv$pollen_H1[1:52],na.rm = TRUE)
biodiv$pollen_H2[is.na(biodiv$pollen_H2)] = median(biodiv$pollen_H2[1:52],na.rm = TRUE)


###################################################################################################
# -- Option 1: get mean of standardized functions 

# for ecosystem functions
#EF_st = getStdAndMeanFunctions(ef,EFnames,standardizeFunction = standardizeZScore) 
EF_st = getStdAndMeanFunctions(ef,EFnames,standardizeFunction = standardizeUnitScale) 

# for biodiversity 
#biodiv_st = getStdAndMeanFunctions(biodiv,Bionames,standardizeFunction = standardizeZScore) 
biodiv_st = getStdAndMeanFunctions(biodiv,Bionames,standardizeFunction = standardizeUnitScale) 

# rename 
names(EF_st)[length(EF_st)] <- "EF_meanFunction"
names(biodiv_st)[length(biodiv_st)] <- "BioDiv_meanFunction"

# add plot name 
EF_st$PlotID <- seq(1,56) 
biodiv_st$PlotID <- seq(1,56) 

# save the files
write.csv(biodiv_st, file = paste0(Sys.Date(),"_Multi-Biodiv.H2.Mean.csv"),row.names=F)
write.csv(EF_st, file = paste0(Sys.Date(), "_Multi-EF.Mean.csv"),row.names=F)

###################################################################################################
# -- option 2: get number of functions greater than or equal to a threshold across a wide range of thresholds.

# Standardize the data 

ef <-  decostand(ef,"range",na.rm=TRUE) # standardize using the range: variances are kept constant 
#ef <- decostand(ef,"standardize",na.rm=TRUE) 

biodiv <-  decostand(biodiv,"range",na.rm=TRUE) # standardize using the range: variances are kept constant 
#biodiv <- decostand(biodiv,"standardize",na.rm=TRUE) 

# add plot name (optional)
biodiv$PlotID <- seq(1,56) 
ef$PlotID <- seq(1,56) 


# Ecosystem function 

EF_Thresh <-getFuncsMaxed(ef, EFnames, threshmin=0.01, threshmax=0.99, threshstep = 0.01, maxN=3, prepend=c("PlotID"))
EF_Thresh.simple <-getFuncsMaxed(ef, EFnames, threshmin=0.1, threshmax=0.9, threshstep = 0.1, maxN=3, prepend=c("PlotID"))

# Biodiversity 

Biodiv_Thresh <- getFuncsMaxed(biodiv, Bionames, threshmin=0.01, threshmax=0.99, maxN=3, prepend=c("PlotID"))
Biodiv_Thresh.simple <- getFuncsMaxed(biodiv, Bionames, threshmin=0.1, threshmax=0.9, threshstep = 0.1, maxN=3, prepend=c("PlotID"))


# save the files

write.csv(EF_Thresh, file = paste0(Sys.Date(), "_Multi-EF.Thresh.csv"),row.names=F)
write.csv(Biodiv_Thresh.simple, file = paste0(Sys.Date(), "_Multi-Biodiv.H2.Thresh.csv"),row.names=F)

#######################################
# plotting 

theme_set(theme_bw())

# Treatment category 
Biodiv_Thresh$treat <- "Treat"
Biodiv_Thresh$treat[which(Biodiv_Thresh$PlotID > 52)]        <- "MAU"

EF_Thresh$treat <- "Treat"
EF_Thresh$treat[which(EF_Thresh$PlotID > 52)]        <- "MAU"



#x11(3,3)
#(d <- ggplot(Biodiv_Thresh, aes(x=thresholds, y=funcMaxed))
#  + geom_point(aes(color=treat)) + ylab("Number of taxa")) 


x11(6,4)
(d <- ggplot(Biodiv_Thresh, aes(x=thresholds, y=funcMaxed))
  # + ylim(-0.5, 8.5)
  + geom_jitter(position=position_jitter(width=0, height=.3),alpha=0.1,size=2,
                aes(color=treat),show.legend = FALSE) 
+   stat_summary(fun = median,
                 geom = "line",
                 aes(color = treat)) 
+ stat_summary(
  mapping = aes(color = treat),
  fun = function(z) { quantile(z,0.25) },geom = "line")
+ stat_summary(
  mapping = aes(color = treat),
  fun = function(z) { quantile(z,0.75) },geom = "line"))

x11(6,4)
(d <- ggplot(EF_Thresh, aes(x=thresholds, y=funcMaxed))
  + geom_jitter(position=position_jitter(width=0, height=.3),alpha=0.1,size=2,
                aes(color=treat),show.legend = FALSE) 
  +   stat_summary(fun = median,
                   geom = "line",
                   aes(color = treat)) 
  + stat_summary(
    mapping = aes(color = treat),
    fun = function(z) { quantile(z,0.25) },geom = "line")
  + stat_summary(
    mapping = aes(color = treat),
    fun = function(z) { quantile(z,0.75) },geom = "line"))


x11(3,3)
(d <- ggplot(EF_Thresh, aes(x=thresholds, y=funcMaxed))
  + geom_jitter(position=position_jitter(width=0, height=.3),
                aes(color=treat),show.legend = FALSE) )



