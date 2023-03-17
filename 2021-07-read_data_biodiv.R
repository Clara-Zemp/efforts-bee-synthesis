
####################################################################################################
#
# EFForTS-BEE Data Proprocessing 
# Read data related to associated biodiversity 
# Last updates: July 2021 by Clara Zemp 
#
####################################################################################################

library(data.table)
library(iNEXT)
library(vegan)
library(codyn)
library(stringi)

# delete all objects
rm(list=ls())

# read plot info 
plotinfo   <- fread("C:/CRC990-B11/Experiment/B11plot_with_controlplots_201705.csv")
plotlist <- plotinfo[,1] # get list of plot IDs

#plotinfo[is.na(DivLev)]$DivLev=-1

####################################################################################################
# -- birds -------------------------------------------------------------------------------

# -- load the data 

# Load the raw data from sound record 
#birds_B11.data = fread("C:/CRC990-B11/data/bird/bird calls.csv") 
# birds_B11 = birds_B11[call_distance_m<= 28] # max distance (m) for bird detection

# Load the data processed by Kevin D
birds_B11.data = fread("C:/CRC990-B11/data/bird/Birds 2017 less than 28 m for Clara.csv")

# Load the info about survey
birds_B11.survey = fread("C:/CRC990-B11/data/bird/surveys.csv") 

# load info about the species
birds_sp = fread("C:/CRC990-B11/data/bird/Species-level bird traits+areas.csv") 

# merge data 
birds_B11 = merge(birds_B11.data,birds_sp,by.x="Species",by.y="Species_Birdlife")
names(birds_B11)[2] <- "PlotID" # rename 

# merge data and survey 
#birds_B11 <- merge(birds_B11.survey,birds_B11.data,by="SoundName", all.x = TRUE, all.y = TRUE)

# get incidence data per unit recording 
birds_B11 <- transform(birds_B11, bird_incidence = abs(1-as.numeric(is.na(birds_B11$Time_recording))))
birds_B11 <- transform(birds_B11, bird_incidence = abs(1-as.numeric(is.na(birds_B11$Time_recording))))

# get the number of unit per plot 
# for (k in seq(1,56)){
#  print(length(unique(birds_B11[Plot==k]$SoundName))) # should be 4 (4 recordings per plot)
# }  
#Nunitplot = 4

# -- aggregate the data per recording 
bird.sp.sound = birds_B11.data[,.(bird_activity = sum(call_duration_s*number_of_individuals) # total vocal activity 
                                   ,bird_N = max(number_of_individuals) # abundance 
                                  ,bird_detection = sum(number_of_individuals)) # nb of detection 
                               ,.(PlotID,Species,SoundName)]


# # -- aggregate the data per species and per plot 

# # option 1: based on the data aggregated per recording -> over-estimated abundance
bird.sp.plot1 = bird.sp.sound[,.(bird_freq1 = length(unique(SoundName)) # frequency of occurence 
                                   ,bird_activity1 = sum(bird_activity) # total vocal activity 
                                   ,bird_N1 = sum(bird_N)) # abundance as the sum of the max number of individuals for all recordings 
                             #      ,bird_detection = sum(bird_detection)) # nb of detection 
                             ,.(PlotID,Species)]

# # option 2 : based on the pooled raw data --> under-estimated abundance
bird.sp.plot = birds_B11[,.(bird_freq = length(unique(SoundName)) # frequency of occurence 
                             ,bird_activity = sum(call_duration_s*number_of_individuals) # total vocal activity 
                             ,bird_N = max(number_of_individuals)) # abundance as the max number of individuals per recording 
                          #   ,bird_detection = sum(number_of_individuals)) # nb of detection (should not be used)
                          ,.(PlotID,Species)]

# median_A0 <- median(bird.sp.plot1$bird_N, na.rm=TRUE) # median number is 1! 

bird_insecti.sp.plot = birds_B11[Diet=="insectivore",.(bird_insecti_freq = length(unique(SoundName)) # frequency of occurence
                            ,bird_insecti_activity = sum(call_duration_s*number_of_individuals) # total vocal activity
                            ,bird_insecti_N = max(number_of_individuals)) # abundance (under-estimated)
                         ,.(PlotID,Species)]

bird_grani.sp.plot = birds_B11[Diet=="granivore",.(bird_grani_freq = length(unique(SoundName)) # frequency of occurence
                            ,bird_grani_activity = sum(call_duration_s*number_of_individuals) # total vocal activity
                            ,bird_grani_N = max(number_of_individuals)) # abundance (under-estimated)
                         ,.(PlotID,Species)]

bird_omni.sp.plot = birds_B11[Diet=="omnivore",.(bird_omni_freq = length(unique(SoundName)) # frequency of occurence
                            ,bird_omni_activity = sum(call_duration_s*number_of_individuals) # total vocal activity
                            ,bird_omni_N = max(number_of_individuals)) # abundance (under-estimated)
                         ,.(PlotID,Species)]

# # -- aggregate the activity data per plot 

bird_activity0.plot = birds_B11[,.(bird_activity = sum(call_duration_s*number_of_individuals)) # total vocal activity
                              ,.(PlotID)]

bird_insecti.plot = birds_B11[Diet=="insectivore",.(bird_insecti_activity = sum(call_duration_s*number_of_individuals)) # total vocal activity
                                 ,.(PlotID)]

bird_grani.plot = birds_B11[Diet=="granivore",.(bird_grani_activity = sum(call_duration_s*number_of_individuals)) # total vocal activity
                               ,.(PlotID)]

bird_omni.plot = birds_B11[Diet=="omnivore",.(bird_omni_activity = sum(call_duration_s*number_of_individuals)) # total vocal activity
                              ,.(PlotID)]

bird_activity1.plot = merge(bird_insecti.plot,bird_grani.plot,by="PlotID",all=TRUE)
bird_activity2.plot = merge(bird_activity1.plot,bird_omni.plot,by="PlotID",all=TRUE)
bird_activity3.plot = merge(bird_activity2.plot,bird_activity0.plot,by="PlotID",all=TRUE)
bird_activity.plot = merge(bird_activity3.plot,plotinfo[,1],by="PlotID",all=TRUE)


# -- find NA values and set these to 0 (no counts)

ind = which(is.na(bird_activity.plot$bird_insecti_activity))
bird_activity.plot[ind]$bird_insecti_activity <- 0
ind = which(is.na(bird_activity.plot$bird_omni_activity))
bird_activity.plot [ind]$bird_omni_activity <- 0
ind = which(is.na(bird_activity.plot$bird_grani_activity))
bird_activity.plot[ind]$bird_grani_activity <- 0
ind = which(is.na(bird_activity.plot$bird_activity))
bird_activity.plot[ind]$bird_activity <- 0

# save the file 
write.csv(bird_activity.plot, file = paste0(Sys.Date(),"_bird_activity.plot.csv"),row.names=F)

# -- get count matrix

# at plot level 
x <- list()
Plots = sort(unique(bird.sp.plot$PlotID))
k = 1
for (plotid in Plots){
  x[[k]] <- as.numeric(bird.sp.plot[PlotID==plotid]$bird_N)
  #x[[k]] <- as.numeric(bird_grani.sp.plot[Plot_ID==plotid]$bird_grani_N)
  #x[[k]] <- as.numeric(bird_insecti.sp.plot[Plot_ID==plotid]$bird_insecti_N)
  #x[[k]] <- as.numeric(bird_omni.sp.plot[Plot_ID==plotid]$bird_omni_N)
    # x[[k]] <- c(Nunitplot, as.numeric(bird.sp.plot[Plot_ID==plotid]$bird_freq)) # the first nb is the number of sampling unit (4 recordings)
   k <- k+1
 }
names(x) <- Plots


# -- get diversity indices

# get diversity indices using Vegan package
H0 = {}; H1 = {}; H2 = {}; N = {};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]]) # abundance
}

# get rarefied / extrapolated hill numbers using the median number of individuals as level
# H0rare = {}; H1rare = {}; H2rare = {};
# for (k in seq(length(x))){
#Hrare = estimateD(x[k], datatype = "abundance", base = "size", conf=NULL, level = median(N))
#   H0rare[k] <- Hrare$`q = 0`
#H1rare[k] <- Hrare$`q = 1`
#   H2rare[k] <- Hrare$`q = 2`
#}

birds.plot = data.frame(matrix(,ncol=5,nrow=length(x))) # create an empty data frame
names(birds.plot) <- c("PlotID","bird_H0","bird_H1","bird_H2","bird_N")
birds.plot$PlotID <- as.numeric(names(x))
birds.plot$bird_H0 <- H0
birds.plot$bird_H1 <- H1
birds.plot$bird_H2 <- H2
birds.plot$bird_N <- N

# -- find missing plots and set these to 0 (no counts)
birds.plot = merge(birds.plot,plotinfo[,1],by="PlotID",all=TRUE)
birds.plot[which(is.na(birds.plot$bird_H0)),]$bird_H0 <- 0
birds.plot[which(is.na(birds.plot$bird_H1)),]$bird_H1 <- 0
birds.plot[which(is.na(birds.plot$bird_H2)),]$bird_H2 <- 0
birds.plot[which(is.na(birds.plot$bird_N)),]$bird_N <- 0


# save the file 
write.csv(birds.plot, file = paste0(Sys.Date(),"_birds.plot.csv"),row.names=F)

####################################################################################################
# -- bats -------------------------------------------------------------------------------

#-- import data from sound recordings 

bats_B11   <- fread("C:/CRC990-B11/Data/bats/insectivorous bat activities - automated recordings.csv")
                 
# convert format PlotID (from P01 to 1)
mytext <-sub(".", "", bats_B11$Plot)
bats_B11$PlotID <- as.numeric(mytext)

# -- get count matrix

x <- list()
Plots = sort(unique(bats_B11$PlotID))
k = 1
for (plotid in Plots){
  x[[k]] <- as.numeric(bats_B11[PlotID==plotid]$number_of_individuals)
  k <- k+1
}
names(x) <- Plots


# -- get diversity indices

# get diversity indices using Vegan package
H0 = {}; H1 = {}; H2 = {}; N = {};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]]) # abundance
}

# get rarefied / extrapolated hill numbers using the median number of individuals as level (only 3 !)
# H0rare = {}; H1rare = {}; H2rare = {};
# for (k in seq(length(x))){
#    Hrare = estimateD(x[k], datatype = "abundance", base = "size", conf=NULL, level = median(N))
#    H0rare[k] <- Hrare$`q = 0`
#    H1rare[k] <- Hrare$`q = 1`
#    H2rare[k] <- Hrare$`q = 2`
# }

bats.plot = data.frame(matrix(,ncol=5,nrow=length(x))) # create an empty data frame
names(bats.plot) <- c("PlotID","bat_H0","bat_H1","bat_H2","bat_N")
bats.plot$PlotID <- as.numeric(names(x))
bats.plot$bat_H0 <- H0
bats.plot$bat_H1 <- H1
bats.plot$bat_H2 <- H2
bats.plot$bat_N <- N

# get total activity estimates and merge the data 
bats.plot1 <- bats_B11[,.(bat_activity = sum(activity_s*number_of_individuals)),.(PlotID)]
bats.plot <- merge(bats.plot,bats.plot1,by="PlotID",all.x="TRUE")

# save the file 
write.csv(bats.plot, file = paste0(Sys.Date(),"_bats.plot.csv"),row.names=F)

####################################################################################################
# -- insects -------------------------------------------------------------------------------

#-- import data 

insects_B11.data <- fread("C:/CRC990-B11/data/insects/B09_data_2018_pan_traps.csv")
# remove space in the name
names(insects_B11.data) <- gsub(" ", "_", names(insects_B11.data))

insects_B11.taxa <- fread("C:/CRC990-B11/data/insects/B09_data_2018_morphospecies.csv")

insects_B11 <- merge(insects_B11.taxa,insects_B11.data,by="Morphospecies")

# include spiders (only with assigned Family) as predators 
insects_B11[which(insects_B11$Order=="Araneae" & !is.na(insects_B11$Family))]$Functional_group <- "predator"

# -- get standardized number of individuals 
 
# get total number of individuals per sample  
insects_N.sample = insects_B11[,.(N=sum(Abundance))
                              ,.(PlotID,Sampling_round)]

# get average number of individuals per plot 
insects_N.plot = insects_N.sample [,.(insects_Nstd=mean(N, na.rm=TRUE))
                               ,.(PlotID)]

# -- get number of individual per species and per plot  
insects.sp.plot = insects_B11[,.(counts=sum(Abundance))
                             ,.(PlotID,Morphospecies,Functional_group)]

# -- find NA values and set these to 0 (no counts)

ind = which(is.na(insects.sp.plot$counts))
insects.sp.plot[ind]$counts <- 0


# -- create empty data frame 

insects.plot = data.frame(matrix(ncol=13,nrow=56)) # create an empty data frame
names(insects.plot) <- c("PlotID","insect_H0","insect_H1","insect_H2","insect_N",
                         "insect_pollinator_H0","insect_pollinator_H1","insect_pollinator_H2","insect_pollinator_N",
                      "insect_predator_H0","insect_predator_H1","insect_predator_H2","insect_predator_N")

# -- get count matrix 

Plots = sort(unique(insects.sp.plot$PlotID))
k = 1
x <- list()

for (plotid in Plots){
#  x[[k]] <- as.numeric(insects.sp.plot[PlotID==plotid]$counts)
#  x[[k]] <- as.numeric(insects.sp.plot[PlotID==plotid & Functional_group == "pollinator"]$counts)
 x[[k]] <- as.numeric(insects.sp.plot[PlotID==plotid & (Functional_group == "predator" | Functional_group =="parasitica")]$counts)
  k <- k+1
}
names(x) <- Plots

# -- vizualize accumulation curves 

#I = iNEXT(x,q=2,datatype = "abundance")
#x11(); ggiNEXT(I, type = 2)
 
# -- get diversity indices using Hill numbers ("Effective number of species")

#E = estimateD(x, datatype = "abundance", base = "size")
#Ec = estimateD(x, datatype = "abundance", base = "coverage")
 
H0 = {}; H1 = {}; H2 = {}; N = {};
 for (k in seq(length(x))){
   H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
   H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
   H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
   N[k] <- sum(x[[k]])
 }

# -- fill in the dataframe 

# overall insects 
#insects.plot$insect_H0rare <- E[which(E$order==0),]$qD
#insects.plot$insect_H1rare <- E[which(E$order==1),]$qD
#insects.plot$insect_H2rare <- E[which(E$order==2),]$qD
insects.plot$insect_H0 <- H0
insects.plot$insect_H1 <- H1
insects.plot$insect_H2 <- H2
insects.plot$insect_N <- N

# pollinators 
#insects.plot$insect_pollinator_H0rare <- E[which(E$order==0),]$qD
#insects.plot$insect_pollinator_H1rare <- E[which(E$order==1),]$qD
#insects.plot$insect_pollinator_H2rare <- E[which(E$order==2),]$qD
insects.plot$insect_pollinator_H0 <- H0
insects.plot$insect_pollinator_H1 <- H1
insects.plot$insect_pollinator_H2 <- H2
insects.plot$insect_pollinator_N <- N

# predators 
#insects.plot$insect_predator_H0rare <- E[which(E$order==0),]$qD
#insects.plot$insect_predator_H1rare <- E[which(E$order==1),]$qD
#insects.plot$insect_predator_H2rare <- E[which(E$order==2),]$qD
insects.plot$insect_predator_H0 <- H0
insects.plot$insect_predator_H1 <- H1
insects.plot$insect_predator_H2 <- H2
insects.plot$insect_predator_N <- N


# plot ID 
insects.plot$PlotID <- Plots

# save the file 
write.csv(insects.plot, file = paste0(Sys.Date(),"_insects.plot.csv"),row.names=F)


# ####################################################################################################
# # Trees and shrubs -------------------------------------------------------------------------------

# -- import data of planted trees 
planted_trees_B11 <- fread("C:/CRC990-B11/Data/trees/B11.tree.plots.csv")

# -- include only individuals > 1.3 m & alive in 2018
planted_trees_surv <- planted_trees_B11[(round(Height.2018,1)>=1.3) & Survival.2018==1]

# -- get number of living individuals per species and per plot
planted_trees_surv.sp.plot = planted_trees_surv[,.(count=length(TreeID))
                                      ,.(PlotID,Species)]

# -- get diversity indices
x <- list()
Plots = sort(unique(planted_trees_surv.sp.plot$PlotID))
k=1
for (plotid in Plots){
  x[[k]] <- as.numeric(planted_trees_surv.sp.plot[PlotID==plotid]$count)
  k <- k+1
}
names(x) <- Plots

H0 = {}; H1 = {}; H2 = {}; N = {}; 
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]])
  
}

# get rarefied / extrapolated hill numbers using the median number of individuals as level 
H0rare = {}; H1rare = {}; H2rare = {};
for (k in seq(length(x))){
  Hrare = estimateD(x[k], datatype = "abundance", base = "size", conf=NULL, level = median(N))
  H0rare[k] <- Hrare$`q = 0`
  H1rare[k] <- Hrare$`q = 1`
  H2rare[k] <- Hrare$`q = 2`
}


planted_trees.plot = data.frame(matrix(0,ncol=8,nrow=56)) # create an empty data frame
names(planted_trees.plot) <- c("PlotID","planted_tree_H0","planted_tree_H1","planted_tree_H2","planted_tree_N","planted_tree_H0rare","planted_tree_H1rare","planted_tree_H2rare")
planted_trees.plot$PlotID <- seq(1,56)
planted_trees.plot$planted_tree_H0[Plots] <- H0
planted_trees.plot$planted_tree_H1[Plots] <- H1
planted_trees.plot$planted_tree_H2[Plots] <- H2
planted_trees.plot$planted_tree_N[Plots] <- N
planted_trees.plot$planted_tree_H0rare[Plots] <- H0rare
planted_trees.plot$planted_tree_H1rare[Plots] <- H1rare
planted_trees.plot$planted_tree_H2rare[Plots] <- H2rare

# replace NA by 1 (species diversity is 1 if there is only one individual)
ind <- is.na(planted_trees.plot)
planted_trees.plot[ind] <- 1

# save the file 
write.csv(planted_trees.plot, file = paste0(Sys.Date(),"_planted_trees.plot.csv"),row.names=F)


# -- sponteneous established trees and shrubs -------------------------------------------------------------------------------
# higher than 1.3 m (but different sampling areas -> scaling needed !)

#-- import data

spont_trees_B11 <- fread("C:/CRC990-B11/data/spontaneous_trees/b11-b06-spont-tree-inventory/data/processed/2021-07-05v_EFForTS-BEE_recruited_trees_2018.csv")

# -- include only individuals > 1.3 m (otherwise belong to understorey)
spont_trees_B11 <- spont_trees_B11[height>=1.3]

# -- get total nb of species (optional)
length(unique(spont_trees_B11$species))

# convert format PlotID (from P01 to 1)
mytext <-sub(".", "", spont_trees_B11$plot_ID)
spont_trees_B11$PlotID <- as.numeric(mytext)

# -- merge the datasets
#spont_trees_B11 <- merge(spont_trees_list,spont_trees_B11,by.x="Field name",by.y="Species")
spont_trees.plotinfo <- merge(plotinfo,spont_trees_B11,by="PlotID")

# -- get number of individual per species and per plot
spont_trees.sp.plot = spont_trees_B11[,.(count=length(tree_ID))
                                         ,.(PlotID,species)]


# -- get number of individual per species and diversity level
#spont_trees.sp.divlev = spont_trees.plotinfo[,.(count=length(TreeID))# frequency of occurence
#                                    ,.(DivLev,Species)]

# -- get count matrix

# at plot level
x <- list()
Plots = sort(unique(spont_trees.sp.plot$PlotID))
k=1
for (plotid in Plots){
  x[[k]] <- as.numeric(spont_trees.sp.plot[PlotID==plotid]$count)
  k <- k+1
}
names(x) <- Plots

# -- get diversity indices

H0 = {}; H1 = {}; H2 = {}; N={};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]])
}

# not working if we compute the confidence interval !
#I = iNEXT(x,q=0,datatype = "abundance", se=FALSE)
#x11(); ggiNEXT(I, type = 1, se=FALSE)
#ggiNEXT(I)

# get rarefied / extrapolated hill numbers using the median number of individuals as level 
H0rare = {}; H1rare = {}; H2rare = {};
for (k in seq(length(x))){
  Hrare = estimateD(x[k], datatype = "abundance", base = "size", conf=NULL, level = median(N))
  H0rare[k] <- Hrare$`q = 0`
  H1rare[k] <- Hrare$`q = 1`
  H2rare[k] <- Hrare$`q = 2`
}

spont_trees.plot = data.frame(matrix(0,ncol=8,nrow=56)) # create an empty data frame
names(spont_trees.plot) <- c("PlotID","spont_tree_H0","spont_tree_H1","spont_tree_H2","spont_tree_H0rare","spont_tree_H1rare","spont_tree_H2rare","spont_tree_N")
spont_trees.plot$PlotID <- seq(1,56)
spont_trees.plot$spont_tree_H0[Plots] <- H0
spont_trees.plot$spont_tree_H1[Plots]  <- H1
spont_trees.plot$spont_tree_H2[Plots]  <- H2
spont_trees.plot$spont_tree_N[Plots]  <- N
spont_trees.plot$spont_tree_H0rare[Plots] <- H0rare
spont_trees.plot$spont_tree_H1rare[Plots] <- H1rare
spont_trees.plot$spont_tree_H2rare[Plots] <- H2rare

# replace NA by 1 (species diversity is 1 if there is only one individual)
ind <- is.na(spont_trees.plot)
spont_trees.plot[ind] <- 1

# save the file 
write.csv(spont_trees.plot, file = paste0(Sys.Date(),"_spont_trees.plot.csv"),row.names=F)

####################################################################################################
# -- Understorey plants -------------------------------------------------------------------------------

#-- import data 

# all plants 
plants_B11 <- fread("C:/CRC990-B11/Data/understorey_veg/Data_Understorey_Vegetation_12.03.19_GrowthForm.csv")
# only herbeceous 
herbs_B11 <- fread("C:/CRC990-B11/Data/understorey_veg/Data_Understorey_Vegetation_29.10.18_herb.csv") # per species (only grass-like, fern and climbers)

# -- get count matrix 

#Plots = sort(unique(plants_B11$PlotID)) # should be same
Plots = sort(unique(herbs_B11$ID)) 

# get the cover data 
herbs_B11 <- herbs_B11[,2:ncol(herbs_B11)]
plants_B11 <- plants_B11[,22:ncol(plants_B11)]

# convert format 
df <- data.frame(apply(plants_B11, 2, function(x) as.numeric(as.character(x))))
#df <- data.frame(apply(herbs_B11, 2, function(x) as.numeric(as.character(x))))
df[is.na(df)]<- 0
df[df==0.1]<- 1 # convert 0.1 (= between 0 and 1% cover) to 1 % cover 

x <- list()
for (plotid in Plots){
  counts = df[plotid,]
  #x[[plotid]] <- as.numeric(counts) # get all values 
  x[[plotid]] <- counts[counts>0] # get only positive values 
}
names(x) <- Plots

# -- vizualize accumulation curves 

#I = iNEXT(x,q=2,datatype = "abundance")
#x11(); ggiNEXT(I, type = 1)

# -- get diversity indices

# estimated from rarefaction curve
#E = estimateD(x, datatype = "abundance", base = "size")
#Ec = estimateD(x, datatype = "abundance", base = "coverage")

# plants.plot = data.frame(matrix(,ncol=4,nrow=56)) # create an empty data frame
# names(plants.plot) <- c("PlotID","plant_H0","plant_H1","plant_H2")
# plants.plot$plant_H0 <- E[which(E$order==0),]$qD
# plants.plot$plant_H1 <- E[which(E$order==1),]$qD
# plants.plot$plant_H2 <- E[which(E$order==2),]$qD
# plants.plot$PlotID <- E[which(E$order==2),]$site


H0 = {}; H1 = {}; H2 = {}; N ={};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]])
}

herbs.plot = data.frame(matrix(,ncol=4,nrow=56)) # create an empty data frame
names(herbs.plot) <- c("PlotID","herbs_H0","herbs_H1","herbs_H2")
herbs.plot$herbs_H0 <- H0
herbs.plot$herbs_H1 <- H1
herbs.plot$herbs_H2 <- H2
herbs.plot$PlotID <- Plots

plants.plot = data.frame(matrix(,ncol=4,nrow=56)) # create an empty data frame
names(plants.plot) <- c("PlotID","plants_H0","plants_H1","plants_H2")
plants.plot$plants_H0 <- H0
plants.plot$plants_H1 <- H1
plants.plot$plants_H2 <- H2
plants.plot$PlotID <- Plots

# save the files 
write.csv(herbs.plot, file = paste0(Sys.Date(),"_herbs.plot.csv"),row.names=F)
write.csv(plants.plot, file = paste0(Sys.Date(),"_plants.plot.csv"),row.names=F)


####################################################################################################
# -- Soil Fungi -------------------------------------------------------------------------------

#-- import data 

#fungi_B11 <- fread("C:/CRC990-B11/Data/fungi/B07_fungi_2017_community_sample.csv") # count data per sample
fungi_B11 <- fread("C:/CRC990-B11/Data/fungi/B07_fungi_2017.csv") # count data (3 samples combined) and info about the OTU 
fungi_B11_div <- fread("C:/CRC990-B11/Data/fungi/B07_fungi_2017_diversity_sample.csv") # diversity data per sample
#fungi_B11_div <- fread("C:/CRC990-B11/Data/fungi/B07_fungi_2017_diversity.csv") # diversity data per sample

# --  extract info 
f2 = as.data.frame(fungi_B11)
Plots <- as.numeric(f2[1,2:57])
count_fungi <- f2[2:8284,2:57] # counts per OTU 
Ntot <- as.numeric(colSums(count_fungi)[1]) # should be 57000 # number used for rarefaction (check colSums(count_fungi))

Trophic.Mode <- as.factor(f2[2:8284,61])
Guild <- as.factor(f2[2:8284,62])
Confidence <- as.factor(f2[2:8284,65])

# -- get diversity 

fungi.plot = data.frame(matrix(ncol=16,nrow=56)) # create an empty data frame
names(fungi.plot) <- c("PlotID","fungi_H0","fungi_H1","fungi_H2",
                         "fungi_patho_H0","fungi_patho_H1","fungi_patho_H2","fungi_patho_N",
                         "fungi_sapo_H0","fungi_sapo_H1","fungi_sapo_H2","fungi_sapo_N",
                         "fungi_symbio_H0","fungi_symbio_H1","fungi_symbio_H2","fungi_symbio_N")

x <- list()
for (plotid in Plots){
  counts = count_fungi[,plotid]
#  counts = count_fungi[which(Trophic.Mode=="Pathotroph"),plotid]
#  counts = count_fungi[which(Trophic.Mode=="Saprotroph"),plotid]
#  counts = count_fungi[which(Trophic.Mode=="Symbiotroph"),plotid]
  #x[[plotid]] <- as.numeric(counts) # get all values 
  x[[plotid]] <- counts[counts>0] # get only positive values 
}
names(x) <- Plots

H0 = {}; H1 = {}; H2 = {}; N = {};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]])
}

fungi.plot$fungi_H0  <- H0
fungi.plot$fungi_H1  <- H1
fungi.plot$fungi_H2 <- H2

fungi.plot$fungi_patho_H0  <- H0
fungi.plot$fungi_patho_H1  <- H1
fungi.plot$fungi_patho_H2 <- H2
fungi.plot$fungi_patho_N <- N/ Ntot

fungi.plot$fungi_sapo_H0  <- H0
fungi.plot$fungi_sapo_H1  <- H1
fungi.plot$fungi_sapo_H2 <- H2
fungi.plot$fungi_sapo_N <- N / Ntot

fungi.plot$fungi_symbio_H0  <- H0
fungi.plot$fungi_symbio_H1  <- H1
fungi.plot$fungi_symbio_H2 <- H2
fungi.plot$fungi_symbio_N <- N/ Ntot

fungi.plot$PlotID <- Plots

# save the file 
write.csv(fungi.plot, file = paste0(Sys.Date(),"_fungi.plot.csv"),row.names=F)


####################################################################################################
# -- Soil bacteria -------------------------------------------------------------------------------

#-- import data 

# - get mean diversity per plot (provided by B02 team)
#bacteria_B11 <- fread("C:/CRC990-B11/Data/soil_bacteria/B02_diversity.v2.csv")
#bacteria.plot = bacteria_B11[DNAorRNA=="RNA",.(bact_H0 = mean(ObservedOTUs)
#                                               ,bact_H1 = mean(exp(Shannon)) # Hill number with q = 1
#                                               ,bact_H2 = mean(1/(1-Simpson))) # Hill number with q = 2
#                             ,.(PlotID)]

# - get raw count data 
bacteria_B11 <- fread("C:/CRC990-B11/Data/soil_bacteria/otu_table_cleaned_100_mod.csv")

# --  extract info 
c = dim(bacteria_B11)[1] # total number of different OTU 
count_DNA_bact <- bacteria_B11[3:c,2:169]    # get counts of OTU for each sample based on DNA (skip first 2 rows)
count_RNA_bact <- bacteria_B11[3:c,170:337]  # get counts of OTU for each sample based on RNA (skip first 2 rows)
taxon_bact <- bacteria_B11[,338] # get info about taxon for each OTU

count_bact <- as.matrix(count_DNA_bact)# convert to matrix
#count_bact <- as.matrix(count_RNA_bact)# convert to matrix


# -- get diversity 

bact.plot = data.frame(matrix(ncol=4,nrow=56)) # create an empty data frame
names(bact.plot) <- c("PlotID","bact_H0","bact_H1","bact_H2")


x <- list() 
for (plotid in seq(1,56)){
  M <- count_bact[,seq(plotid,plotid+2)] # extract the count data for the 3 samples 
  Mnum <- apply(M,2,as.numeric) # convert to numeric
  x[[plotid]] <- rowSums(Mnum) # sum the counts of the 3 samples for each OTU
  x[[plotid]] <- x[[plotid]][x[[plotid]]>0] # get only positive values 
}

names(x) <- seq(1,56)

H0 = {}; H1 = {}; H2 = {};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
}

bact.plot$bact_H0  <- H0
bact.plot$bact_H1  <- H1
bact.plot$bact_H2 <- H2
bact.plot$PlotID <- seq(1,56)

# save the file 
write.csv(bact.plot, file = paste0(Sys.Date(),"_bact.plot.csv"),row.names=F)


# ####################################################################################################
# #-- seed -------------------------------------------------------------------------------

seed.data <- fread("C:/CRC990-B11/Data/seed_traps/B11_data_seed.v2.csv")

# -- get onyl data for the plots 1-56
seed.data$PlotID <- as.numeric(seed.data$PlotID)# convert to numeric (all plots not within 1 - 56 will be converted to NA)
seed.data <- seed.data[-which(is.na(seed.data$PlotID))] 

# convert to integer (for unknown reason, in the plot 10 the counts data are numeric and not integer)
seed.data$Abundance <- as.integer(seed.data$Abundance)

# exclude certain species and families 
seed.data = seed.data[Species != "Clidemia hirta"]
#seed.data = seed.data[Family != "Poaceae"]

################################
# -- get diversity indices 

# get number of individual per species and per plot  (pool the samples and the traps)
seed.sp.plot = seed.data[,.(counts=sum(Abundance,na.rm=TRUE))
                              ,.(PlotID,Species)]


# find NA values and set these to 0 (no counts)
ind = which(is.na(seed.sp.plot$counts))
seed.sp.plot[ind]$counts <- 0

# create empty data frame 
seeds.plot = data.frame(matrix(ncol=5,nrow=56)) # create an empty data frame
names(seeds.plot) <- c("PlotID","seed_H0","seed_H1","seed_H2","seed_N")

# get count matrix 
Plots = sort(unique(seed.sp.plot$PlotID))
k = 1
x <- list()

for (plotid in Plots){
  x[[k]] <- as.numeric(seed.sp.plot[PlotID==plotid]$counts)
  k <- k+1
}
names(x) <- Plots

# -- vizualize accumulation curves (optional)

#I = iNEXT(x,q=2,datatype = "abundance")
#x11(); ggiNEXT(I, type = 2)

# get rarefied / extrapolated hill numbers using the median number of seeds as the reference level 
#E = estimateD(x, datatype = "abundance", base = "size", level = median_seed)

H0 = {}; H1 = {}; H2 = {}; N = {};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]])
}

# -- fill in the dataframe 

#seeds.plot$seed_H0rare <- E[which(E$order==0),]$qD
#seeds.plot$seed_H1rare <- E[which(E$order==1),]$qD
#seeds.plot$seed_H2rare <- E[which(E$order==2),]$qD
seeds.plot$seed_H0 <- H0
seeds.plot$seed_H1 <- H1
seeds.plot$seed_H2 <- H2
seeds.plot$seed_N <- N 

seeds.plot$PlotID <- Plots

# save the file 
write.csv(seeds.plot, file = paste0(Sys.Date(),"_seeds.plot.csv"),row.names=F)


# ####################################################################################################
# #-- pollen -------------------------------------------------------------------------------

# -- import data
pollen_B11_count <- fread("C:/CRC990-B11/Data/pollen_rain/B11_Final_pollen_count.csv")
pollen_B11_concent <- fread("C:/CRC990-B11/Data/pollen_rain/B11_Final_pollen_concentration.csv")

# -- get count matrix 
count_pollen <- as.matrix(pollen_B11_count) # convert to matrix
Nsp = ncol(pollen_B11_count)-1 # total nb of morphospecies in the dataset (exclude 1st column with plotid)

# find NA values and set these to 0 (no counts)
ind = which(is.na(count_pollen))
count_pollen[ind] <- 0

Plots = sort(unique(pollen_B11_count$PlotID)) # plots 28,34,41,47 are missing 
x <- list() 
k <- 1
for (plotid in Plots){
  ind <- which(count_pollen[,1]==plotid)  # extract the count data for specific plot 
  M <- count_pollen[ind,2:Nsp]
  Mnum <- as.numeric(M) # convert to numeric
  x[[k]] <- Mnum
  x[[k]] <- x[[k]][x[[k]]>0] # get only positive values --> remove that line to calculate rarefied diversity metric !!
  k <- k+1
}
names(x) <- Plots


# -- get diversity indices 

# rarefy to 100 pollen (when rounded to interger: leads to dame result then non-rarified )
# E = estimateD(as.data.frame(x), datatype = "abundance", base = "size", level = 100)

pollen.plot = data.frame(matrix(ncol=5,nrow=length(Plots))) # create an empty data frame
names(pollen.plot) <- c("PlotID","pollen_H0","pollen_H1","pollen_H2","pollen_conc")

H0 = {}; H1 = {}; H2 = {}; N= {};
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]]) 
}

#pollen.plot$pollen_H0rare <- E[which(E$order==0),]$qD
#pollen.plot$pollen_H1rare <- E[which(E$order==1),]$qD
#pollen.plot$pollen_H2rare <- E[which(E$order==2),]$qD
pollen.plot$pollen_H0  <- H0
pollen.plot$pollen_H1  <- H1
pollen.plot$pollen_H2 <- H2
#pollen.plot$pollen_N <- N # number of pollen grain has no meaning (was counted to approx. 100 only)

pollen.plot$PlotID <- Plots

# get pollen concentration 
merge0 <- merge(pollen.plot,pollen_B11_concent, by="PlotID")
pollen.plot$pollen_conc <- merge0$Concentration_grains_per_cm3

# fill missing data (Plots 28, 34, 41, 47) with median from all experimental plots 
pollen.plot <- merge(pollen.plot,plotinfo[,1],all=TRUE) 
#pollen.plot[which(is.na(pollen.plot$pollen_H0)),]$pollen_H0 <- median(pollen.plot[which(pollen.plot$PlotID<53),]$pollen_H0)
#is.na(pollen.plot$pollen_H1) <- median(pollen.plot[which(pollen.plot$PlotID<53),]$pollen_H1)
#is.na(pollen.plot$pollen_H2) <- median(pollen.plot[which(pollen.plot$PlotID<53),]$pollen_H2)

# save the file 
write.csv(pollen.plot, file = paste0(Sys.Date(),"_pollen.plot.csv"),row.names=F)


####################################################################################################
# -- soil fauna -------------------------------------------------------------------------------

#-- import data 
#soil_fauna_B11 <- fread("C:/CRC990-B11/data/soil_fauna/Data_B13_GroupRichness.csv")
soil_fauna_B11 <- fread("C:/CRC990-B11/data/soil_fauna/Data_B13_groups.v2.csv")
#soil_fauna_metabolism <- fread("C:/CRC990-B11/data/soil_fauna/Data_B13_groups_metabolism.csv")
soil_fauna_metabolism <- fread("C:/CRC990-B11/data/soil_fauna/Data_B13_groups_metabolism.v3.csv")

soil_fauna_B11[Group=="collembola"]$Group = "Collembola" # homogeneize 
soil_fauna_B11 <- soil_fauna_B11[Group!="Nematoda"] # exclude group 
soil_fauna_B11 <- soil_fauna_B11[Group!="Hirudinea"] # exclude group 

#-- get abundance & metabolism per group and per plot

soil_fauna0 <- merge(soil_fauna_B11,soil_fauna_metabolism,by = "Group")

soil_fauna1 <- soil_fauna0[,.(group_metabolism_JouleHour = Number * Metabolism_O) # Nb of individuals * metabolic rate in Joule per hour per individual 
                            ,.(PlotID,Group,Guild,Number)]

soil_fauna_metabo0.plot = soil_fauna1[,.(soil_fauna_metabo = sum(group_metabolism_JouleHour)) # total metabolism
                                     ,.(PlotID)]

# -- get metabolism per plot and per guild 

soil_fauna.guild = soil_fauna1[,.(soil_fauna_metabolism = sum(group_metabolism_JouleHour))
                     ,.(PlotID,Guild)]

soil_predator.plot = soil_fauna.guild[Guild=="predators"][,c(1,3)]
names(soil_predator.plot)[c(2)]<- c("soil_predator_metabo")

soil_herbivore.plot = soil_fauna.guild[Guild=="herbivores"][,c(1,3)]
names(soil_herbivore.plot)[c(2)]<- c("soil_herbi_metabo")

soil_decomposer.plot = soil_fauna.guild[Guild=="decomposers"][,c(1,3)]
names(soil_decomposer.plot)[c(2)]<- c("soil_decomp_metabo")

#merge the file 
soil_fauna_metabo2.plot = merge(soil_predator.plot,soil_herbivore.plot, by="PlotID", all=TRUE)
soil_fauna_metabo3.plot = merge(soil_fauna_metabo2.plot,soil_decomposer.plot, by="PlotID", all=TRUE)
soil_fauna_metabo.plot = merge(soil_fauna_metabo3.plot,soil_fauna_metabo0.plot, by="PlotID", all=TRUE)

# set NA values to 0 (missing data means no organisms were found)
soil_fauna_metabo.plot[is.na(soil_fauna_metabo.plot$soil_herbi_metabo)]$soil_herbi_metabo <- 0

# save the files 
write.csv(soil_fauna_metabo.plot, file = paste0(Sys.Date(),"_soil_fauna_metabo.plot.csv"))


# -- get diversity indices

soil_fauna.plot = data.frame(matrix(ncol=16,nrow=56)) # create an empty data frame
names(soil_fauna.plot) <- c("PlotID","soil_fauna_H0","soil_fauna_H1","soil_fauna_H2",
                       "soil_predator_H0","soil_predator_H1","soil_predator_H2","soil_predator_N",
                       "soil_decomp_H0","soil_decomp_H1","soil_decomp_H2","soil_decomp_N",
                       "soil_herbi_H0","soil_herbi_H1","soil_herbi_H2","soil_herbi_N")


Plots = sort(unique(soil_fauna1$PlotID))
k = 1
x <- list() 
for (plotid in Plots){
  x[[k]] <- as.numeric(soil_fauna1[PlotID==plotid]$Number)
#  x[[k]] <- as.numeric(soil_fauna1[Guild == "predators" & PlotID==plotid]$Number)
#  x[[k]] <- as.numeric(soil_fauna1[Guild == "decomposers" & PlotID==plotid]$Number)
 # x[[k]] <- as.numeric(soil_fauna1[Guild == "herbivores" & PlotID==plotid]$Number)
  k <- k+1
}
names(x) <- Plots


H0 = {}; H1 = {}; H2 = {}; N = {}; 
for (k in seq(length(x))){
  H0[k] <- length(x[[k]]) # Hill nb with q = 0 (species richness)
  H1[k] <- exp(diversity(x[[k]], index = "shannon")) # Hill nb with q = 1
  H2[k] <- 1/(1-diversity(x[[k]], index = "simpson")) # Hill nb with q = 2
  N[k] <- sum(x[[k]])
}

soil_fauna.plot$soil_fauna_H0  <- H0
soil_fauna.plot$soil_fauna_H1  <- H1
soil_fauna.plot$soil_fauna_H2 <- H2
soil_fauna.plot$soil_fauna_N <- N

soil_fauna.plot$soil_predator_H0  <- H0
soil_fauna.plot$soil_predator_H1  <- H1
soil_fauna.plot$soil_predator_H2 <- H2
soil_fauna.plot$soil_predator_N <- N

soil_fauna.plot$soil_decomp_H0  <- H0
soil_fauna.plot$soil_decomp_H1  <- H1
soil_fauna.plot$soil_decomp_H2 <- H2
soil_fauna.plot$soil_decomp_N <- N

soil_fauna.plot$soil_herbi_H0  <- H0
soil_fauna.plot$soil_herbi_H1  <- H1
soil_fauna.plot$soil_herbi_H2 <- H2
soil_fauna.plot$soil_herbi_N <- N

soil_fauna.plot$PlotID <- Plots

# replace INF by 1 (species diversity is 1 if there is only one individual)
ind <- is.infinite(soil_fauna.plot$soil_herbi_H2)
soil_fauna.plot$soil_herbi_H2[ind] <- 1

# save the files 
write.csv(soil_fauna.plot, file = paste0(Sys.Date(),"_soil_fauna.plot.csv"),row.names=F)

