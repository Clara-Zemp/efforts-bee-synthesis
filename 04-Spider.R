####################################################################################################
#
# EFForTS-BEE Data Analaysis
# Spider diagram 
# 
####################################################################################################

library(data.table) 
library(fmsb) # spider diagram

# delete all objects
rm(list=ls())

# set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1")

# -- load data 
mydata <- fread("2022-11-10-Biodiv.H0.csv")
mydata <- fread("2022-11-10-Biodiv.H1.csv")
mydata <- fread("2022-11-10-Biodiv.H2.csv")
mydata <- fread("2022-11-10-EF.csv")

# --  select variables of interest and order 

mydata  <- mydata[,c("Y.NetChange", # oil palm yield 
                     "microclimate_buffer","Kfs","ETmean", # water / microclimate regulation 
                     "litter_weight", "decomp.all.t1","soil_decomp_metabo", # nutrient cycling 
                     "soil_herbi_metabo", # herbivory 
                     "soil_predator_metabo","insect_predator_N","vert_insecti_activity", # predation 
                     "soilNC","soil_decompact","soilP", # soil protection 
                     "PollRate","insect_pollinator_N", # pollination 
                     "invasion_resist","seed_N_native", # regeneration 
                     "total_AGB" # productivity 
                     )] 


# -- get variable names 

#v_name = c("bats","birds","arthropods","herbs","pollen","seeds","trees",
#           "soil fauna","bacteria","fungi")


#v_name = c("AGB","tree growth","yield","water infiltr.","evapotransp.","litter input","litter decomp.","decomp. (fauna)","soil herbiv.", "soil predation", "1/soil C:N","1/soil density",
#           "soil P","pollination","pollinators","predators (arthrop.)","predators (vert.)","resistance to invasion","seed rain","pollen rain")

v_name = names(mydata)


###################################################################################################
# spider diagrams 

# get min and max values using the 5 and 95 percentiles 
maxmin <- data.frame(rbind(apply(mydata,2,quantile,probs=0.95,na.rm=TRUE),
                           apply(mydata,2,quantile,probs=0.05,na.rm=TRUE))) # 5% and 95% percentile 


# // some trials to change the min max values 
#test <- setorder(as.data.table(mydata), cols = colnames(mydata))
#test <- order(mydata[,3], decreasing = TRUE)
#order(mydata$bat_H2, decreasing = TRUE)

# get median values per categories 
dat <- rbind(maxmin,
             apply(mydata[53:56,],2,median,na.rm=TRUE), # BAU 
             apply(mydata[1:52,],2,median,na.rm=TRUE)) # Treat


op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))
x11(5,5); radarchart(dat, axistype=0, seg=5, plty=1, vlabels=v_name ,
                  pcol=c(rgb(0.5,0.5,0.5), # black 
                         rgb(0,0,0), # gray 
                         "#c95970","#c48533","#749f58","#6c93c5","#a065c4")
                  ,pfcol=c(rgb(0.5,0.5,0.5,0.3),
                           rgb(0.5,0.5,0.5,0.3),
                           rep(rgb(1,1,1,1,1),5))) # transparent 
             #     ,title="Biodiversity H1")


