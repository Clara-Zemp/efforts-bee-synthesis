
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS:
#
# Structural equation model 
#
####################################################################################################

# Install development branch from github
#library(devtools)
#install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE)

# Load library
library(data.table) 
library(Hmisc) 
library(multcomp) 
library(sandwich) 
library(stats)
library(lme4)
library(nlme)
library(dplyr)
library(ggpubr)
library(bestNormalize) 
library(LambertW)
library(goft)
library(FSA) 
library(car) 
library(pscl) 
library(AER) 
library(piecewiseSEM)
library(dotwhisker)
library(vegan)

# delete all objects
rm(list=ls())

# set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1/Data")

# load data
str_PCA <- as.data.frame(fread("str_PCA.csv"))
plotinfo <- as.data.frame(fread("B11plot_info.csv")) # plot design information 

str_PCA        <- str_PCA[,2:dim(str_PCA)[2]] 
#plotinfo       <- plotinfo[,2:dim(plotinfo)[2]] 

# log-transform the experimental treatment variables 
plotinfo$divlevelLog <- log(plotinfo$divlev + 1)
plotinfo$plotsizeLog <- log(plotinfo$plotsize) 

#################################################
# Biodiversity and ecosystem functioning data 
#################################################


biodiv0  <- as.data.frame(fread("2022-11-10-Biodiv.H0.csv"))
biodiv1  <- as.data.frame(fread("2022-11-10-Biodiv.H1.csv"))
biodiv2  <- as.data.frame(fread("2022-11-10-Biodiv.H2.csv"))
ef        <- as.data.frame(fread("2022-11-10-EF.csv"))

# --  select variables of interest 
ef <- ef[,c("Y.NetChange", # oil palm yield 
            "microclimate_buffer","Kfs","ETmean", # water / microclimate regulation 
            "litter_weight", "decomp.all.t1","soil_decomp_metabo", # nutrient cycling 
            "soil_herbi_metabo", # herbivory 
            "soil_predator_metabo","insect_predator_N","vert_insecti_activity", # predation 
            "soilNC","soil_decompact","soilP", # soil protection 
            "PollRate","insect_pollinator_N", # pollination 
            "invasion_resist","seed_N_native", # regeneration 
            "total_AGB" # productivity 
)] 


# fill missing data
biodiv0$pollen_H0[is.na(biodiv0$pollen_H0)] = median(biodiv0$pollen_H0[1:52],na.rm = TRUE)
biodiv1$pollen_H1[is.na(biodiv1$pollen_H1)] = median(biodiv1$pollen_H1[1:52],na.rm = TRUE)
biodiv2$pollen_H2[is.na(biodiv2$pollen_H2)] = median(biodiv2$pollen_H2[1:52],na.rm = TRUE)
ef$decomp.all.t1[is.na(ef$decomp.all.t1)] = median(ef$decomp.all.t1[53:56],na.rm = TRUE) # plot 56 is missing: fill in with median values from other control plot

# select the output and exclude control plots 
#output <- ef[1:52,]  
#output <- biodiv0[1:52,]  
#output <- biodiv1[1:52,]  
output <- biodiv2[1:52,]  

#################################################
# multidiversity and multifunctionality data 
#################################################

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

Multi <- cbind(Thresh20EF[1:52],Thresh50EF[1:52],Thresh80EF[1:52],MeanEF$EF_meanFunction[1:52],
               Thresh20BiodivH0[1:52],Thresh50BiodivH0[1:52],Thresh80BiodivH0[1:52],MeanBiodivH0$BioDiv_meanFunction[1:52],
               Thresh20BiodivH1[1:52],Thresh50BiodivH1[1:52],Thresh80BiodivH1[1:52],MeanBiodivH1$BioDiv_meanFunction[1:52],
               Thresh20BiodivH2[1:52],Thresh50BiodivH2[1:52],Thresh80BiodivH2[1:52],MeanBiodivH2$BioDiv_meanFunction[1:52])

output <- as.data.frame(Multi)
names(output) <- cbind("Thresh20EF","Thresh50EF", "Thresh80EF","AvgEF",
                       "Thresh20BiodivH0","Thresh50BiodivH0","Thresh80BiodivH0","AvgBiodivH0",
                       "Thresh20BiodivH1","Thresh50BiodivH1","Thresh80BiodivH1","AvgBiodivH1",
                       "Thresh20BiodivH2","Thresh50BiodivH2","Thresh80BiodivH2","AvgBiodivH2")


#----------------------------------------------#
# Apply the linear models                      #
#----------------------------------------------#

# create dataframe 
df <- as.data.frame(cbind(str_PCA,plotinfo[1:52,],output))

# variable names 
output_name <- names(output)

# number of variables
n_output <- dim(output)[2]
n_plotinfo <- dim(str_PCA)[2]+dim(plotinfo)[2]

# linear models with the paths that are significant (and that makes sense)
m1 <- lm(str1 ~ divlevelLog, data = df)
m2 <- lm(str2 ~ plotsizeLog, data = df)

#x11(); plot(m1)
#x11(); plot(m2)

# normality test 
#shapiro.test(residuals(m1)) # normal 
#shapiro.test(residuals(m2)) # normal 

#------------------------------------------------#
# Normality Test
#------------------------------------------------#

sw_test <- array(, dim = n_output)
for(i in 1:n_output){ # Shapiro-Wilk Test for overall normal residuals at alpha = 5%
  #  model <- aov(df[,i+n_plotinfo] ~ str1 + str2 + plotsizeLog + divlevelLog, data = df)
  model <- aov(df[,i+n_plotinfo] ~ plotsizeLog + divlevelLog, data = df)
  
  if(shapiro.test(residuals(model))$p.value > 0.05){ 
    sw_test[i] <- "normal"
  }
  else{
    sw_test[i] <- "not normal"
  }
}

ind_normal    <- which(sw_test == "normal") # index of normal components
ind_nonnormal <- which(sw_test == "not normal") # index of non-normal components 

output_name[ind_nonnormal]
output_name[ind_normal]
length(ind_nonnormal)
length(ind_normal)

#----------------------------------------------#
# Data transformation of non-normal components
# and standardization 
#----------------------------------------------#

mydata_transformed <- df
output_transformed <- output

sw_test_trans  <- array(, dim = length(ind_nonnormal))
transfo_method <- list()
model          <- list() # continue to use the existing model list 
counter <- 1
for(i in ind_nonnormal){ # Normalization according to the bestNormalize package
  transformation            <- bestNormalize(output[,i], allow_orderNorm = FALSE, out_of_sample = FALSE, standardize = FALSE)
  output_transformed[,i]    <- transformation$x.t
  transfo_method[[counter]] <- transformation$chosen_transform
  counter <- counter +1
}

# standardize the transformed output (optional)
#output_transformed <- decostand(output_transformed,"standardize",na.rm=TRUE) 

df_transformed <- cbind(str_PCA,plotinfo,output_transformed)

names(transfo_method) <- output_name[ind_nonnormal]
transfo_method

#----------------------------------------------#
# Apply the linear models                      #
#----------------------------------------------#

# Model 0: with only vegetation structure as explanatory variables 
lm_0 <- list()
lm_1 <- list()
for(i in 1:n_output){ # Shapiro-Wilk Test for overall normal residuals at alpha = 5%
  lm_0[[i]] <- lm(df_transformed[,i+n_plotinfo] ~ str1 + str2, data = df_transformed)
  lm_1[[i]] <- lm(df_transformed[,i+n_plotinfo] ~ plotsizeLog + divlevelLog, data = df_transformed)
}

names(lm_0) <- output_name
names(lm_1) <- output_name

# vizualize model output 
theme_set(theme_bw())

#x11(); 
#d <- dwplot(lm_1,dodge_size = 0.9) #+ xlim(-1.5, 1.5)
#d + scale_colour_hue(labels = output_name) 
# reverse the order so that the 1st model appears on the top 
#d <- dwplot(rev(lm_1),dodge_size = 0.9) #+ xlim(-1.5, 1.5)
#d

#x11(); dwplot(lm_1,dodge_size = 0.9)# + xlim(-1.5, 1.5)


# full model (optional)
lm_output <- list()
for(i in 1:n_output){ # Shapiro-Wilk Test for overall normal residuals at alpha = 5%
  lm_output[[i]] <- lm(df_transformed[,i+n_plotinfo] ~ str1 + str2 + plotsizeLog + divlevelLog, data = df_transformed)
}

names(lm_output) <- output_name
# to do: find a way to use the list in the SEM 

#----------------------------------------------#
# Apply the structural equation model          #
#----------------------------------------------#

m1 <- lm(str1 ~ divlevelLog, data = df_transformed)
m2 <- lm(str2 ~ plotsizeLog, data = df_transformed)

#######################
# Biodiversity 
#######################

## -- H0 (species richness)

m3 <- lm(bat_H0 ~  plotsizeLog + divlevelLog,  data = df_transformed)
m4 <- lm(bird_H0 ~  plotsizeLog + divlevelLog,  data = df_transformed)
m5 <- lm(insect_H0 ~  plotsizeLog + divlevelLog , data = df_transformed)
m6 <- lm(herbs_H0 ~ plotsizeLog + divlevelLog , data = df_transformed)
m7 <- lm(pollen_H0 ~  plotsizeLog + divlevelLog , data = df_transformed)
m8 <- lm(seed_H0 ~  plotsizeLog + divlevelLog , data = df_transformed)
m9 <- lm(spont_tree_H0rare ~  plotsizeLog + divlevelLog , data = df_transformed)
m10 <- lm(soil_fauna_H0 ~  plotsizeLog + divlevelLog ,  data = df_transformed)
m11 <- lm(bact_H0 ~ plotsizeLog + divlevelLog , data = df_transformed)
m12 <- lm(fungi_H0 ~  plotsizeLog + divlevelLog , data = df_transformed)

# apply SEM (see below) and update models for which teste of directed separation indicates a missing link 
m5 <- lm(insect_H0 ~  plotsizeLog + divlevelLog + str1 + str2, data = df_transformed)
m8 <- lm(seed_H0 ~  plotsizeLog + divlevelLog + str1 + str2, data = df_transformed)

## -- H1 (Shannon diversity)

m3 <- lm(bat_H1 ~  plotsizeLog + divlevelLog,  data = df_transformed)
m4 <- lm(bird_H1 ~  plotsizeLog + divlevelLog,  data = df_transformed)
m5 <- lm(insect_H1 ~  plotsizeLog + divlevelLog , data = df_transformed)
m6 <- lm(herbs_H1 ~ plotsizeLog + divlevelLog , data = df_transformed)
m7 <- lm(pollen_H1 ~  plotsizeLog + divlevelLog , data = df_transformed)
m8 <- lm(seed_H1 ~  plotsizeLog + divlevelLog , data = df_transformed)
m9 <- lm(spont_tree_H1rare ~  plotsizeLog + divlevelLog , data = df_transformed)
m10 <- lm(soil_fauna_H1 ~  plotsizeLog + divlevelLog ,  data = df_transformed)
m11 <- lm(bact_H1 ~ plotsizeLog + divlevelLog , data = df_transformed)
m12 <- lm(fungi_H1 ~  plotsizeLog + divlevelLog , data = df_transformed)

# apply SEM (see below) and update models for which teste of directed separation indicates a missing link 
m3 <- lm(bat_H1 ~  plotsizeLog + divlevelLog + str1,  data = df_transformed)
m6 <- lm(herbs_H1 ~ plotsizeLog + divlevelLog + str1 , data = df_transformed)

## -- H2 (Simpson diversity)

m3 <- lm(bat_H2 ~  plotsizeLog + divlevelLog,  data = df_transformed)
m4 <- lm(bird_H2 ~  plotsizeLog + divlevelLog,  data = df_transformed)
m5 <- lm(insect_H2 ~  plotsizeLog + divlevelLog , data = df_transformed)
m6 <- lm(herbs_H2 ~ plotsizeLog + divlevelLog , data = df_transformed)
m7 <- lm(pollen_H2 ~  plotsizeLog + divlevelLog , data = df_transformed)
m8 <- lm(seed_H2 ~  plotsizeLog + divlevelLog , data = df_transformed)
m9 <- lm(spont_tree_H2rare ~  plotsizeLog + divlevelLog , data = df_transformed)
m10 <- lm(soil_fauna_H2 ~  plotsizeLog + divlevelLog ,  data = df_transformed)
m11 <- lm(bact_H2 ~ plotsizeLog + divlevelLog , data = df_transformed)
m12 <- lm(fungi_H2 ~  plotsizeLog + divlevelLog , data = df_transformed)

# apply SEM (see below) and update models for which teste of directed separation indicates a missing link 
m3 <- lm(bat_H2 ~  plotsizeLog + divlevelLog + str1,  data = df_transformed)
m6 <- lm(herbs_H2 ~  plotsizeLog + divlevelLog + str1,  data = df_transformed)
m9 <- lm(spont_tree_H2rare ~  plotsizeLog + divlevelLog + str2, data = df_transformed)

## run the SEM 

# for one taxa 
modelList <- psem(m1,m2,m3,data = df_transformed) # --> change here with m4, m5 etc. 
# get summary statistics
S = summary(modelList)
S$dTable # Tests of directed separation
S$Cstat
# get coef 
Coefs <- coefs(modelList)

# for all the output 
lm_biodiv_list <- list(m3,m4,m5,m6,m7,m8,m9,m10,m11,m12) # Store individual models in a list 
SEMStat <- cbind(output_name[1],S$Cstat,S$IC,S$R2[3,5]) # initialize 
names(SEMStat)[10] <- "R2"
names(SEMStat)[1] <- "var_name"
Coefs.matrix <- Coefs[,-9] # initialize (do not use the last column that shows significance level)
levels(Coefs.matrix$Predictor) <- c("(Intercept)","divlevelLog","plotsizeLog","str1","str2")
counter <- 3 # start the coeff table at lign 3 (the first 2 rows are used for str1 and str2)

for(i in 1:n_output){ 
  # run the SEM 
  modelList <- psem(m1,m2,lm_biodiv_list[[i]],data = df_transformed)
  # get summary
  S = summary(modelList)
  print(AIC(modelList)) 
  # get coef 
  Coefs <- coefs(modelList)
  nvar <- dim(Coefs)[1]-2 # number of variables included to predict the output 
  Coefs.matrix[counter:(counter+nvar-1),] <- Coefs[3:(3+nvar-1),1:8]
  counter <- counter + nvar 
  SEMStat[i,2:4] <- S$Cstat # Fischer's C 
  SEMStat[i,5:9]   <- S$IC # AIC, BIC 
  SEMStat[i,10]   <- S$R2[3,5] # R2
}

SEMStat[,1] <- output_name # variable name  

# save results 
write.csv(Coefs.matrix, file = paste0(Sys.Date(),"-Coefs.matrix.biodiv.H2.csv"))
write.csv(SEMStat, file = paste0(Sys.Date(),"-SEMStat.biodiv.H2.csv"))


#######################################
# Ecosystem function data 
######################################

m3 <- lm(Y.NetChange ~  plotsizeLog + divlevelLog , data = df_transformed)
m4 <- lm(total_AGB ~   plotsizeLog + divlevelLog, data = df_transformed)
m5 <- lm(seed_N_native ~  plotsizeLog + divlevelLog , data = df_transformed)
m6 <- lm(invasion_resist ~  plotsizeLog + divlevelLog , data = df_transformed)
m7 <- lm(insect_pollinator_N ~  plotsizeLog + divlevelLog , data = df_transformed) 
m8 <- lm(PollRate ~  plotsizeLog + divlevelLog , data = df_transformed)
m9 <- lm(soilP ~   plotsizeLog + divlevelLog , data = df_transformed) 
m10 <- lm(soil_decompact ~  plotsizeLog + divlevelLog , data = df_transformed)
m11 <- lm(soilNC ~  plotsizeLog + divlevelLog , data = df_transformed)
m12 <- lm(vert_insecti_activity ~  plotsizeLog + divlevelLog , data = df_transformed)
m13 <- lm(insect_predator_N ~  plotsizeLog + divlevelLog , data = df_transformed) 
m14 <- lm(soil_predator_metabo ~   plotsizeLog + divlevelLog , data = df_transformed)
m15 <- lm(soil_herbi_metabo ~   plotsizeLog + divlevelLog , data = df_transformed)
m16 <- lm(soil_decomp_metabo ~   plotsizeLog + divlevelLog , data = df_transformed)
m17 <- lm(decomp.all.t1 ~  plotsizeLog + divlevelLog , data = df_transformed) 
m18 <- lm(litter_weight ~ plotsizeLog + divlevelLog, data = df_transformed)
m19 <- lm(ETmean ~ plotsizeLog + divlevelLog , data = df_transformed)
m20 <- lm(Kfs ~   plotsizeLog + divlevelLog , data = df_transformed)
m21 <- lm(microclimate_buffer ~  plotsizeLog + divlevelLog , data = df_transformed)

# apply SEM (see below) and update models for which teste of directed separation indicates a missing link 

m3 <- lm(Y.NetChange ~ plotsizeLog + divlevelLog + str1,  data = df_transformed)
m4 <- lm(total_AGB ~   plotsizeLog + divlevelLog + str1 , data = df_transformed)
m5 <- lm(seed_N_native ~  plotsizeLog + divlevelLog + str1 , data = df_transformed)
m7 <- lm(insect_pollinator_N ~  plotsizeLog + divlevelLog + str1, data = df_transformed) 
m13 <- lm(insect_predator_N ~  plotsizeLog + divlevelLog + str1, data = df_transformed) 
m15 <- lm(soil_herbi_metabo ~   plotsizeLog + divlevelLog + str1 , data = df_transformed)
m18 <- lm(litter_weight ~ plotsizeLog + divlevelLog + str1 + str2, data = df_transformed)
m21 <- lm(microclimate_buffer ~  plotsizeLog + divlevelLog + str1, data = df_transformed)


# run one SEM (ex: Tot_BA_inc)
modelList <- psem(m1,m2,m3,data = df_transformed)
# get summary statistics
S = summary(modelList)
S$dTable
S$Cstat
# get coef 
Coefs <- coefs(modelList)

# run all the SEM (for all functions)
lm_ef_list <- list(m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21)

SEMStat <- cbind(output_name[1],S$Cstat,S$IC,S$R2[3,5]) # initialize 
names(SEMStat)[10] <- "R2"
names(SEMStat)[1] <- "var_name"
Coefs.matrix <- Coefs[,-9] # initialize (do not use the last column that shows significance level)
levels(Coefs.matrix$Predictor) <- c("(Intercept)","divlevelLog","plotsizeLog","str1","str2")
counter <- 3 # start the coeff table at lign 3 (the first 2 rows are used for str1 and str2)

for(i in 1:n_output){ 
  # run the SEM 
  modelList <- psem(m1,m2,lm_ef_list[[i]],data = df_transformed)
  # get summary
  S = summary(modelList)
  print(AIC(modelList)) 
  # get coef 
  Coefs <- coefs(modelList)
  nvar <- dim(Coefs)[1]-2 # number of variables included to predict the output 
  Coefs.matrix[counter:(counter+nvar-1),] <- Coefs[3:(3+nvar-1),1:8]
  counter <- counter + nvar 
  SEMStat[i,2:4] <- S$Cstat # Fischer's C 
  SEMStat[i,5:9]   <- S$IC # AIC, BIC 
  SEMStat[i,10]   <- S$R2[3,5] # R2
}

SEMStat[,1] <- output_name # variable name  

write.csv(Coefs.matrix, file = paste0(Sys.Date(),"-Coefs.matrix.EF.csv"))
write.csv(SEMStat, file = paste0(Sys.Date(),"-SEMStat.EF.csv"))


############################
# Multidiversity and multifunctionality 
############################

m3 <- lm(Thresh20EF ~ plotsizeLog + divlevelLog,  data = df_transformed)
m4 <- lm(Thresh50EF ~ plotsizeLog + divlevelLog,  data = df_transformed)
m5 <- lm(Thresh80EF ~ plotsizeLog + divlevelLog,  data = df_transformed)
m6 <- lm(AvgEF ~ plotsizeLog + divlevelLog,  data = df_transformed)
m7 <- lm(Thresh20BiodivH0 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m8 <- lm(Thresh50BiodivH0 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m9 <- lm(Thresh80BiodivH0 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m10 <- lm(AvgBiodivH0 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m11 <- lm(Thresh20BiodivH1 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m12 <- lm(Thresh50BiodivH1 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m13 <- lm(Thresh80BiodivH1 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m14 <- lm(AvgBiodivH1 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m15 <- lm(Thresh20BiodivH2 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m16 <- lm(Thresh50BiodivH2 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m17 <- lm(Thresh80BiodivH2 ~ plotsizeLog + divlevelLog,  data = df_transformed)
m18 <- lm(AvgBiodivH2 ~ plotsizeLog + divlevelLog,  data = df_transformed)

# update models for which SEM indicated a missing link 
m3 <- lm(Thresh20EF ~ plotsizeLog + divlevelLog + str1 ,  data = df_transformed)
m6 <- lm(AvgEF ~ plotsizeLog + divlevelLog + str1 ,  data = df_transformed)
m8 <- lm(Thresh50BiodivH0 ~ plotsizeLog + divlevelLog + str2,  data = df_transformed)
m10 <- lm(AvgBiodivH0 ~ plotsizeLog + divlevelLog + str2,  data = df_transformed)
m16 <- lm(Thresh50BiodivH2 ~ plotsizeLog + divlevelLog  + str2,  data = df_transformed)

# run one SEM 
modelList <- psem(m1,m2,m3,data = df_transformed)
# get summary statistics
S = summary(modelList)
S$dTable
S$Cstat
# get coef 
Coefs <- coefs(modelList)


# run all the SEM (for all functions)
lm_multi_list <- list(m3,m4,m5,m6,m7,m8,m9,m10,m11, m12, m13, m14, m15, m16, m17, m18)

SEMStat <- cbind(output_name[1],S$Cstat,S$IC,S$R2[3,5]) # initialize 
names(SEMStat)[10] <- "R2"
names(SEMStat)[1] <- "var_name"
Coefs.matrix <- Coefs[,-9] # initialize (do not use the last column that shows significance level)
levels(Coefs.matrix$Predictor) <- c("(Intercept)","divlevelLog","plotsizeLog","str1","str2")
counter <- 3 # start the coeff table at lign 3 (the first 2 rows are used for str1 and str2)

for(i in 1:n_output){ 
  # run the SEM 
  modelList <- psem(m1,m2,lm_multi_list[[i]],data = df_transformed)
  # get summary
  S = summary(modelList)
  print(AIC(modelList)) 
  # get coef 
  Coefs <- coefs(modelList)
  nvar <- dim(Coefs)[1]-2 # number of variables included to predict the output 
  Coefs.matrix[counter:(counter+nvar-1),] <- Coefs[3:(3+nvar-1),1:8]
  counter <- counter + nvar 
  SEMStat[i,2:4] <- S$Cstat # Fischer's C 
  SEMStat[i,5:9]   <- S$IC # AIC, BIC 
  SEMStat[i,10]   <- S$R2[3,5] # R2
}

SEMStat[,1] <- output_name # variable name  

# save results 
write.csv(Coefs.matrix, file = paste0(Sys.Date(),"-Coefs.matrix.Multi.csv"))
write.csv(SEMStat, file = paste0(Sys.Date(),"-SEMStat.Multi.csv"))
