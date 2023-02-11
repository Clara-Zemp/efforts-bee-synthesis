
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS :
#
# Read all data 
#
####################################################################################################


library(data.table) 
library(tidyverse)
library(huge)
library(MVN)

####################################################################################################

# -- set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1/Data")

# -- delete all objects
rm(list=ls())

# -- load data 
list.files()
file.list <- list.files(pattern='*.plot.csv')
data_list <- lapply(file.list, read.csv, header=TRUE)
mydata = data_list %>% reduce(inner_join, by = "PlotID")  

mydata <- as.data.frame(mydata)

# -- do some variables processing 

mydata <- transform(mydata, vert_insecti_activity = bird_insecti_activity + bat_activity) # total activity of insectivorous vertebrates
mydata <- transform(mydata, soilNC = 1/soilCN)
mydata <- transform(mydata, soil_decompact = 1/soil_dens)
mydata <- transform(mydata, invasion_resist = 100-Clidemia_cover)
mydata <- transform(mydata, microclimate_buffer = 1/Median_amplitude_AT)

# -- select variables of interest 

#  biodiversity H0
biodivH0 <- as.data.frame(mydata[,c(#"biodivMaxed_tresh50",
  "bat_H0","bird_H0", "insect_H0",
#  "insect_predator_H0","insect_pollinator_H0",
  "herbs_H0","pollen_H0","seed_H0",
  "spont_tree_H0rare","soil_fauna_H0","bact_H0", "fungi_H0"
 # "fungi_patho_H0","fungi_sapo_H0","fungi_symbio_H0"
)])

#  biodiversity H1
biodivH1 <- as.data.frame(mydata[,c(#"biodivMaxed_tresh50",
  "bat_H1","bird_H1", "insect_H1",
#  "insect_predator_H1","insect_pollinator_H1",
  "herbs_H1","pollen_H1","seed_H1",
  "spont_tree_H1rare","soil_fauna_H1","bact_H1", "fungi_H1"
#  "fungi_patho_H1","fungi_sapo_H1","fungi_symbio_H1"
)])

#  biodiversity H2
biodivH2 <- as.data.frame(mydata[,c(#"biodivMaxed_tresh50",
  "bat_H2","bird_H2", "insect_H2",
#  "insect_predator_H2","insect_pollinator_H2",
  "herbs_H2","pollen_H2","seed_H2",
  "spont_tree_H2rare","soil_fauna_H2","bact_H2", "fungi_H2"
#  "fungi_patho_H2","fungi_sapo_H2","fungi_symbio_H2"
)])

# ecosystem functions 
ef <- as.data.frame(mydata[,c("Y.NetChange", # oil palm yield 
                                 "microclimate_buffer","Kfs","ETmean", # water / microclimate regulation 
                                 "litter_weight", "decomp.all.t1","soil_decomp_metabo", # nutrient cycling 
                              "soil_herbi_metabo", # herbivory 
                               "soil_predator_metabo","insect_predator_N","vert_insecti_activity", # predation 
                               "soilNC","soil_decompact","soilP", # soil protection 
                               "PollRate","insect_pollinator_N", # pollination 
                                "invasion_resist","seed_N_native", # regeneration 
                                "total_AGB","Tot_BA_inc" # productivity 
)]) 

# -- Save the data 

#write.csv(mydata_transformed2, file = paste0(Sys.Date(),"-Biodiv.H1.transformed.csv"),row.names=F)
#write.csv(mydata, file = paste0(Sys.Date(),"-Biodiv.H1.csv"),row.names=F)

write.csv(ef, file = paste0(Sys.Date(),"-EF.csv"),row.names=F)
write.csv(biodivH0, file = paste0(Sys.Date(),"-Biodiv.H0.csv"),row.names=F)
write.csv(biodivH1, file = paste0(Sys.Date(),"-Biodiv.H1.csv"),row.names=F)
write.csv(biodivH2, file = paste0(Sys.Date(),"-Biodiv.H2.csv"),row.names=F)
