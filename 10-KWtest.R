
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS :
#
# Nonparameteric Kruskal-Wallis test  
#
####################################################################################################


library(data.table) 
library(multcomp) 
library(sandwich) 
library(stats)
library(lme4)
library(nlme)
library(dplyr)
library(ggpubr)
library(LambertW)
library(goft)
library(FSA) 
library(car) 
library(pscl) 
library(AER) 
library(DTK)
library(car)
library(plyr)
library(userfriendlyscience)
library(boot)
library(bestNormalize)

# -- set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1/data")

# -- delete all objects
rm(list=ls())

# -- load data 

plotinfo  <- as.data.frame(fread("plotinfo_simple.csv"))

###################################
# Individual indicators 

BiodivH0  <- as.data.frame(fread("2022-11-10-Biodiv.H0.csv"))
BiodivH1  <- as.data.frame(fread("2022-11-10-Biodiv.H1.csv"))
BiodivH2  <- as.data.frame(fread("2022-11-10-Biodiv.H2.csv"))
EF        <- as.data.frame(fread("2022-11-10-ef.csv"))

# -- chose here the data to analyse 
output <- EF

# fill the gaps (optional)
#biodiv_nt$pollen_H1[is.na(biodiv_nt$pollen_H1)] = median(biodiv_nt$pollen_H1[1:52],na.rm = TRUE)
#ef_nt$pollen_conc[is.na(ef_nt$pollen_conc)] = median(ef_nt$pollen_conc[1:52],na.rm = TRUE)
#ef$pollen_conc[is.na(ef$pollen_conc)] = median(ef$pollen_conc[1:52],na.rm = TRUE)

###################################
# multidiversity and multifunctionality 

# load data
ThreshEF             <- as.data.frame(fread("2022-11-20_Multi-EF.Thresh.csv"))
ThreshBiodivH0        <- as.data.frame(fread("2022-11-20_Multi-Biodiv.H0.Thresh.csv"))
ThreshBiodivH1        <- as.data.frame(fread("2022-11-20_Multi-Biodiv.H1.Thresh.csv"))
ThreshBiodivH2        <- as.data.frame(fread("2022-11-20_Multi-Biodiv.H2.Thresh.csv"))
MeanEF        <- as.data.frame(fread("2022-11-20_Multi-EF.Mean.csv"))
MeanBiodivH0  <- as.data.frame(fread("2022-11-20_Multi-Biodiv.H0.Mean.csv"))
MeanBiodivH1  <- as.data.frame(fread("2022-11-20_Multi-Biodiv.H1.Mean.csv"))
MeanBiodivH2  <- as.data.frame(fread("2022-11-20_Multi-Biodiv.H2.Mean.csv"))

# extract threshold values 20, 50 and 80 % 
Thresh20EF <- ThreshEF[(56+1):(56*2),]$funcMaxed
Thresh20BiodivH0 <- ThreshBiodivH0[(56+1):(56*2),]$funcMaxed
Thresh20BiodivH1 <- ThreshBiodivH1[(56+1):(56*2),]$funcMaxed
Thresh20BiodivH2 <- ThreshBiodivH2[(56+1):(56*2),]$funcMaxed

Thresh50EF <- ThreshEF[(56*4+1):(56*5),]$funcMaxed
Thresh50BiodivH0 <- ThreshBiodivH0[(56*4+1):(56*5),]$funcMaxed
Thresh50BiodivH1 <- ThreshBiodivH1[(56*4+1):(56*5),]$funcMaxed
Thresh50BiodivH2 <- ThreshBiodivH2[(56*4+1):(56*5),]$funcMaxed

Thresh80EF <- ThreshEF[(56*7+1):(56*8),]$funcMaxed
Thresh80BiodivH0 <- ThreshBiodivH0[(56*7+1):(56*8),]$funcMaxed
Thresh80BiodivH1 <- ThreshBiodivH1[(56*7+1):(56*8),]$funcMaxed
Thresh80BiodivH2 <- ThreshBiodivH2[(56*7+1):(56*8),]$funcMaxed

#Thresh10 <- Thresh[1:56,]$funcMaxed
#Thresh30 <- Thresh[(56*2+1):(56*3),]$funcMaxed
#Thresh40 <- Thresh[(56*3+1):(56*4),]$funcMaxed
#Thresh60 <- Thresh[(56*5+1):(56*6),]$funcMaxed
#Thresh70 <- Thresh[(56*6+1):(56*7),]$funcMaxed
#Thresh90 <- Thresh[(56*8+1):(56*9),]$funcMaxed

Multi <- cbind(Thresh20EF,Thresh50EF,Thresh80EF,MeanEF$EF_meanFunction,
               Thresh20BiodivH0,Thresh50BiodivH0,Thresh80BiodivH0,MeanBiodivH0$BioDiv_meanFunction,
               Thresh20BiodivH1,Thresh50BiodivH1,Thresh80BiodivH1,MeanBiodivH1$BioDiv_meanFunction,
               Thresh20BiodivH2,Thresh50BiodivH2,Thresh80BiodivH2,MeanBiodivH2$BioDiv_meanFunction)

output <- as.data.frame(Multi)
names(output) <- cbind("Thresh20EF","Thresh50EF", "Thresh80EF","AvgEF",
                       "Thresh20BiodivH0","Thresh50BiodivH0","Thresh80BiodivH0","AvgBiodivH0",
                       "Thresh20BiodivH1","Thresh50BiodivH1","Thresh80BiodivH1","AvgBiodivH1",
                       "Thresh20BiodivH2","Thresh50BiodivH2","Thresh80BiodivH2","AvgBiodivH2")

######################################################
# prepare data 

plotinfo       <- plotinfo[,2:dim(plotinfo)[2]] 

# variable names 
var_name <- names(output)

# number of variables
n_var   <- dim(output)[2]
n_plotinfo <- dim(plotinfo)[2]

# sort the levels so that the MAU is the 1st category (will be used as reference in the ANOVA) 
plotinfo$treat           <- factor(plotinfo$treat, levels=c("MAU","Treat")) # re-order the levels
#plotinfo$planted_tree_H1 <- factor(round(plotinfo$planted_tree_H1),levels=c("MAU","0","1","2","3","4"))
#plotinfo$plotsize        <- factor(plotinfo$plotsize,levels=c("MAU","5","10","20","40"))

# Subsituting 0 in planted trees and NA-values in plot size with MAU
#plotinfo$planted_tree_H1[which(plotinfo$treat == "MAU")] <- "MAU"
#plotinfo$plotsize[which(plotinfo$treat == "MAU")]        <- "MAU"

fulldata <- cbind(plotinfo,output)

# Grouping has to be conducted manually depending on preferences
#grouping_factor <- factor(plotinfo$planted_tree_H1, levels = c("MAU", "div0", "div1")) # Example: MAU | planted_trees = 0 | planted_trees > 0 
#grouping_factor[which(plotinfo$planted_tree_H1 == 0)]             <- "div0"
#grouping_factor[which(plotinfo$planted_tree_H1 %in% c(1,2,3,4))]  <- "div1"

# Original grouping factor with two levels MAU and treat
grouping_factor <- plotinfo$treat
  
# mydata is now the data frame containing customized grouping
mydata <- cbind(plotinfo$PlotID, grouping_factor, output)

# Adjustment in the case of ties; needed for Dunn Test
ties_sum <- function(ties){
  dif <- array(, dim = length(which(ties > 1)))
  counter <- 1
  for(i in as.numeric(which(ties > 1))){
    dif[counter] <- ties[i]^3-ties[i]
    counter <- counter + 1
  }
  return(sum(dif))
}

apply_KWtest <- function(component, grouping_factor, mydata){ # Function to perform Kruskal-Wallis test and dunnTest for pairwise comparison
  KW_test   <- kruskal.test(component ~ grouping_factor, data = mydata)
  if(length(levels(grouping_factor)) > 2){
    dunn_test <- dunnTest(component ~ grouping_factor, data = mydata, method = "holm")$res # post-hoc analysis for unequal numbers of oberservations
  }
  else{
    ranks <- rank(component)
    ties <- table(ranks)
    mean_1 <- mean(ranks[which(grouping_factor == levels(grouping_factor)[1])])
    mean_2 <- mean(ranks[which(grouping_factor == levels(grouping_factor)[2])])
    n_1 <- length(which(grouping_factor == grouping_factor[1]))
    n_2 <- length(which(grouping_factor == grouping_factor[2]))
    n <- n_1 + n_2
    sigma_i <- sqrt((n * (n + 1)/12 - ties_sum(ties)/(12 * (n - 1))) * (1/n_1 + 1/n_2))
    
    Z <- (mean_2 - mean_1)/sigma_i
    pvalue <- pvalue_adj <- KW_test$p.value
    Comparison <- paste(levels(mydata$grouping_factor)[2], "-", levels(mydata$grouping_factor)[1])
    
    dunn_test <- data.frame(cbind(Comparison, Z, pvalue, pvalue_adj))
    colnames(dunn_test) <- c("Comparison", "Z", "P.unadj", "P.adj")
  }
  return(list(KW_test, dunn_test))
}


# Either evaluate each component manually (1) or evaluate all components automatically (2):
# (1) Individual analysis, e.g. for bird_H1
#apply_KWtest(component = mydata$bird_H1, grouping_factor = mydata$grouping_factor, mydata = mydata) 

# (2) Full analysis of normal and non-normal components
results_KW <- list()
for(i in 1:n_var){ 
  results_KW[[i]] <- apply_KWtest(component = mydata[,which(colnames(mydata) == var_name[i])], grouping_factor = mydata$grouping_factor, mydata = mydata)
}

names(results_KW) <- var_name

results_KW


# Table: Kruskal-Wallis Test
table_KW <- array(, dim = c(n_var, 3))

for(i in 1:n_var){
  table_KW[i,1] <- results_KW[[i]][[1]]$parameter
  table_KW[i,2] <- results_KW[[i]][[1]]$statistic
  table_KW[i,3] <- results_KW[[i]][[1]]$p.value
}

table_KW_df <- data.frame(round(table_KW, 4), row.names = var_name)
colnames(table_KW_df) <- c("df", "chi-squared", "p-value")

write.csv(table_KW_df, paste0(Sys.Date(),"_KruskalWallis_Multi_grouping_factor.csv"))

# Table: Dunn-Test results - only includes components with signficiant KW-results
n_sign <- length(which(table_KW_df[,3] < 0.05)) 
n_comp <- choose(length(levels(mydata$grouping_factor)), 2)
table_dt <- array(, dim = c(n_sign*n_comp, 4))

counter <- 1
for(i in 1:n_sign){
  for(p in 1:n_comp){
    table_dt[counter,1] <- var_name[which(table_KW[,3] < 0.05)][i]
    table_dt[counter,2] <- levels(results_KW[[1]][[2]][,1])[p]
    table_dt[counter,3] <- as.numeric(as.character(results_KW[[which(table_KW[,3] < 0.05)[i]]][[2]][p,2]))
    table_dt[counter,4] <- as.numeric(as.character(results_KW[[which(table_KW[,3] < 0.05)[i]]][[2]][p,4]))
    counter             <- counter + 1
  }
}

table_dt_df <- data.frame(table_dt)
table_dt_df[,3] <- round(as.numeric(as.character(table_dt_df[,3])), 4)
table_dt_df[,4] <- round(as.numeric(as.character(table_dt_df[,4])), 4)
# table_dt_df[which(table_dt_df[,4] < 0.05),4] <- "significant"
# table_dt_df[which(table_dt_df[,4] >= 0.05 & table_dt_df[,4] != "significant"),4] <- "not significant"
colnames(table_dt_df) <- c("Variable", "Comparison", "Z", "p-value")

write.csv(table_dt_df, paste0(Sys.Date(),"_DunnTest_Multi_grouping_factor.csv"))

