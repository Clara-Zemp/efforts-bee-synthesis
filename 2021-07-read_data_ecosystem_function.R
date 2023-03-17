####################################################################################################
#
# EFForTS-BEE Data Proprocessing 
# Read data related to ecosystem functions 
# Last updates: July 2021 by Clara Zemp 
#
#
####################################################################################################

library(data.table)
library(stringi)

# delete all objects
rm(list=ls())

# set working directory 
#setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper/")

####################################################################################################
# -- litter decomposition -------------------------------------------------------------------------------

decomp_B11 <- fread("C:/CRC990-B11/Data/litter_decomposition/B08_LitterDecomposition.csv")
decomp.palm.T1 <- decomp_B11[Time =='6 months' & Treatment=="Palm"]
decomp.palm.T2 <- decomp_B11[Time =='12 month' & Treatment=="Palm"]
decomp.palm <- merge(decomp.palm.T1, decomp.palm.T2, by="PlotID")
decomp.palm <- decomp.palm[,c("PlotID","decomposed_percent.x","decomposed_percent.y",
                              "root_colonization.x","root_colonization.y")]

decomp.treat.T1 <- decomp_B11[Time =='6 months' & Treatment=="Treat"]
decomp.treat.T2 <- decomp_B11[Time =='12 month' & Treatment=="Treat"]
decomp.treat <- merge (decomp.treat.T1, decomp.treat.T2, by="PlotID")
decomp.treat <- decomp.treat[,c("PlotID","decomposed_percent.x","decomposed_percent.y",
                                "root_colonization.x","root_colonization.y")]

decomp.all.T1 <- decomp_B11[Time =='6 months' & Treatment=="Treat+Palm"]
decomp.all.T2 <- decomp_B11[Time =='12 month' & Treatment=="Treat+Palm"]
decomp.all <- merge(decomp.all.T1, decomp.all.T2, by="PlotID")
decomp.all <- decomp.all[,c("PlotID","decomposed_percent.x","decomposed_percent.y",
                                          "root_colonization.x","root_colonization.y")]

#- rename 
names(decomp.palm) <- c("PlotID","decomp.palm.t1","decomp.palm.t2","root_colon.palm.t1","root_colon.palm.t2")
names(decomp.treat) <- c("PlotID","decomp.treat.t1","decomp.treat.t2","root_colon.treat.t1","root_colon.treat.t2")
names(decomp.all) <- c("PlotID","decomp.all.t1","decomp.all.t2","root_colon.all.t1","root_colon.all.t2")

# - merge 
decomp0 = merge(decomp.palm,decomp.all,by="PlotID", all = TRUE)
decomp= merge(decomp0,decomp.treat,by="PlotID", all = TRUE)

# -- fill missing data 

# for control plots: get the mean of the other control plots
#decomp[PlotID==56]$decomp.palm.t1 <- mean(decomp[PlotID>53]$decomp.palm.t1,na.rm=TRUE)
#decomp[PlotID==56]$root_colon.palm.t1 <- mean(decomp[PlotID>53]$root_colon.palm.t1,na.rm=TRUE)

# for experimental plots: get the mean of the other experimental plots
#decomp[PlotID==49]$root_colon.treat.t2 <- mean(decomp$root_colon.treat.t2,na.rm=TRUE)

# for control plots: use the decomposition of oil palm litter as treatement 
decomp[PlotID>52]$decomp.all.t1 <- decomp[PlotID>52]$decomp.palm.t1
decomp[PlotID>52]$decomp.all.t2 <- decomp[PlotID>52]$decomp.palm.t2
decomp[PlotID>52]$decomp.treat.t1 <- decomp[PlotID>52]$decomp.palm.t1
decomp[PlotID>52]$decomp.treat.t2 <- decomp[PlotID>52]$decomp.palm.t2
decomp[PlotID>52]$decomp.all.t1 <- decomp[PlotID>52]$decomp.palm.t1
decomp[PlotID>52]$decomp.all.t2 <- decomp[PlotID>52]$decomp.palm.t2
decomp[PlotID>52]$root_colon.treat.t1 <- decomp[PlotID>52]$root_colon.palm.t1
decomp[PlotID>52]$root_colon.treat.t2 <- decomp[PlotID>52]$root_colon.palm.t1
decomp[PlotID>52]$root_colon.all.t1 <- decomp[PlotID>52]$root_colon.palm.t1
decomp[PlotID>52]$root_colon.all.t2 <- decomp[PlotID>52]$root_colon.palm.t2


# save the file 
write.csv(decomp, file = paste0(Sys.Date(),"_litter_decomp.plot.csv"))

####################################################################################################
# -- water infiltration -------------------------------------------------------------------------------
water_infiltr <- fread("C:/CRC990-B11/Data/soil/B11_A02_water_infiltration.v2.csv")
water_infiltr <- water_infiltr[,c("PlotID","Kfs_linear_merged")]
names(water_infiltr)[2] <- "Kfs"

# save the file 
write.csv(water_infiltr, file = paste0(Sys.Date(),"_water_infiltr.plot.csv"))

####################################################################################################
# tree growth -------------------------------------------------------------------------------

AGR.tree0 <- fread('C:/CRC990-B11/data/trees/AGR.tree.csv') # absolute growth rates for each tree invidual

# sum over all trees per plot  
# get for 2017-2018
AGR.plot1 <- AGR.tree0[,.(Tot_BA_inc = sum(BA_inc_17.18,na.rm=TRUE))
                        ,.(PlotID)]
# merge with plot info 
plotinfo   <- fread("C:/CRC990-B11/Experiment/B11plot_with_controlplots_201705.csv")
AGR.plot2 <- merge(AGR.plot1,plotinfo,by="PlotID",all.y=TRUE)

# rescale 
AGR.plot3 <- transform(AGR.plot2,Tot_BA_inc = Tot_BA_inc/(Size^2)) # in cm2 / m2 / year 

# set NA values to 0 (no trees means no stem biomass increment)
AGR.plot3$Tot_BA_inc[is.na(AGR.plot3$Tot_BA_inc)] <- 0

# get only the variables of interest 
AGR.plot <- AGR.plot3[,c("PlotID","Tot_BA_inc")]
         
# save the file 
write.csv(AGR.plot, file = paste0(Sys.Date(),"_AGR.plot.csv"))

####################################################################################################
# Above-gound biomass

# data from 2017 (when the oil palms and the trees were both measured) 
# in ton / hectar  
AGB.plot <- fread("C:/CRC990-B11/data/AGB/B11_2017.csv")
       
# save the file 
write.csv(AGB.plot, file = paste0(Sys.Date(),"_AGB.plot.csv"))

####################################################################################################
# leaf litter -------------------------------------------------------------------------------

# Litter production (in g / m2 / year)
leaf.plot0 <- fread("C:/CRC990-B11/data/LeafLitter/litter_B11_2017_2018.v6.csv", na="") # outliers excluded, scaled per m2

# litter content 
B11_6sp_C_and_N <- fread("C:/CRC990-B11/data/LeafLitter/B11_6sp_C_and_N.csv", na="")

leaf.plot0 <- transform(leaf.plot0, litterC = (W.Sp.A.Leaf * B11_6sp_C_and_N[ID=="A.leaf"]$Ctotal_mg_per_g
                             + W.Sp.A.Branch * (B11_6sp_C_and_N[ID=="A.branch"]$Ctotal_mg_per_g + B11_6sp_C_and_N[ID=="A.branch.thick"]$Ctotal_mg_per_g)/2
                             + W.Sp.B * B11_6sp_C_and_N[ID=="B"]$Ctotal_mg_per_g
                             + W.Sp.C * B11_6sp_C_and_N[ID=="C"]$Ctotal_mg_per_g
                             + W.Sp.D * B11_6sp_C_and_N[ID=="D"]$Ctotal_mg_per_g
                             + W.Sp.E * B11_6sp_C_and_N[ID=="E"]$Ctotal_mg_per_g
                             + W.Sp.F * B11_6sp_C_and_N[ID=="F"]$Ctotal_mg_per_g))
          
leaf.plot1 <- transform(leaf.plot0, litterN = (W.Sp.A.Leaf * B11_6sp_C_and_N[ID=="A.leaf"]$Ntotal_mg_per_g
                             + W.Sp.A.Branch * (B11_6sp_C_and_N[ID=="A.branch"]$Ntotal_mg_per_g + B11_6sp_C_and_N[ID=="A.branch.thick"]$Ntotal_mg_per_g)/2
                             + W.Sp.B * B11_6sp_C_and_N[ID=="B"]$Ntotal_mg_per_g
                             + W.Sp.C * B11_6sp_C_and_N[ID=="C"]$Ntotal_mg_per_g
                             + W.Sp.D * B11_6sp_C_and_N[ID=="D"]$Ntotal_mg_per_g
                             + W.Sp.E * B11_6sp_C_and_N[ID=="E"]$Ntotal_mg_per_g
                             + W.Sp.F * B11_6sp_C_and_N[ID=="F"]$Ntotal_mg_per_g))
        
# keep only variables of interest 
leaf.plot2 <- leaf.plot1[,c("PlotID","litterN","litterC","W.all")]     
# rename 
names(leaf.plot2)[4]<- "litter_weight"

# convert to g / m2 / year 
leaf.plot <- transform(leaf.plot2, litterN = litterN/1000, litterC = litterC/1000, litterCN = litterC/litterN)
  
# save the file 
write.csv(leaf.plot, file = paste0(Sys.Date(),"_leaf.plot.csv"))

###################################################################################################
# pollination -----------------------------------------------------------------------

# load data 
chili0 <- fread("C:/CRC990-B11/data/pollination/fruit-flower.csv")

# rename the plot IDs
chili0$Plot <- stri_replace_all_regex(chili0$Plot, "[P]", "")
names(chili0) <- c("PlotID","PollRate")

# save the file 
write.csv(chili0, file = paste0(Sys.Date(),"_pollination.plot.csv"))

####################################################################################################
#-- invasive plants -------------------------------------------------------------------------------

# -- understorey vegetation measurement 
plants_B11 <- fread("C:/CRC990-B11/Data/understorey_veg/Data_Understorey_Vegetation_12.03.19_summary.csv")

# get only the variables of interest
plants.plot <- plants_B11 [,c("alien_cover","native_cover","Clidemia_cover","Clidemia_max_length",
                                     "PlotID")] 

# save the file 
write.csv(plants.plot , file = paste0(Sys.Date(),"_invasion.plot.csv"))

####################################################################################################
#-- Evapotranspiration -------------------------------------------------------------------------------

ET_B11 <- fread("C:/CRC990-B11/Data/evapotranspiration/AllData_Humusindo_56Plots_stats.csv")

ET.plot <- data.frame(matrix(0,ncol=2,nrow=56)) # create an empty data frame
names(ET.plot) <- c("PlotID","ETmean")

ET.plot$PlotID <- ET_B11$`Plot_ ID`
ET.plot$ETmean <- ET_B11$Latent_heat_flux_Mean

# save the file 
write.csv(ET.plot , file = paste0(Sys.Date(),"_ET.plot.csv"))

####################################################################################################
#-- Soil properties -------------------------------------------------------------------------------

# load data 
soil_dens_B11 <- fread("C:/CRC990-B11/Data/soil/A04_Bulk_Density.csv")
soil_pH_B11 <- fread("C:/CRC990-B11/Data/soil/A04_pH.csv")
soil <- fread("C:/CRC990-B11/Data/soil/B07_soil.csv")

# divide the C, N, or P value for it molar mass
soil$Cmol <- soil$C_mg_per_g/12.0107
soil$Nmol <- soil$N_mg_per_g/14.0067
#soil$Pmol <- soil$P_mg_per_g/30.973762

# get ratios 
#soil$CP_ratio = soil$Cmol/soil$Pmol
#soil$NP_ratio = soil$Nmol/soil$Pmol
soil$CN_ratio = soil$Cmol/soil$Nmol # this will overwrite the previous estimate from B07 data

# get mean values per plot from the different samples  
soil_CN_B11 = soil[,.(soilC = mean(C_mg_per_g,na.rm=TRUE)
                      ,soilN = mean(N_mg_per_g,na.rm=TRUE)
                      ,soilP = mean(P_mg_per_g,na.rm=TRUE)
                      ,soilCN = mean(C_mg_per_g/N_mg_per_g,na.rm=TRUE))
                   ,.(PlotID)]


# get mean bulk density from the multiple samples taken in the sub-plot 
soil_dens_B11 = soil_dens_B11[Position=="SP",.(soil_dens = mean(bulk_dens)) 
                              ,.(PlotID)]

# merge the data 
soil0 = merge(soil_CN_B11,soil_dens_B11)
soil.plot = merge(soil0,soil_pH_B11)

# save the file 
write.csv(soil.plot, file = paste0(Sys.Date(),"_soil.plot.csv"))
