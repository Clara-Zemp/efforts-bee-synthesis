# Oil palm yield data analysis 
# Clara, last updated 11/2022


library(ggplot2)   
library(data.table)

theme_set(theme_bw())

#deleting all objects
rm(list=ls())

# load information about oil palms per plot 
B11plots_OP  <- fread("C:/CRC990-B11/Experiment/B11plots_OP.csv")

# load adjacent positions 
OP_adj_pos <- fread("C:/CRC990-B11/Experiment/B11_OilPalms_adjacent.csv")

# get the measured number of palms directly adjacent to the plots 
#OP_dist <- fread("C:/CRC990-B11/Experiment/B11_OilPalms_adjacent_all_measure.csv")
#OP_dist.plot <- OP_dist[,.(Nb_palm_adj = length(which(Distance_OilPalm_Fence_m<7.8))),.(PlotID)]
#B11plots_OP <- merge(OP_dist.plot,B11plots_OP,by="PlotID",all=TRUE)
#B11plots_OP[PlotID>52]$Nb_palm_adj <- 0 # no adjacent oil palms in the control plots 
#
#x11()
#boxplot(Nb_palm_adj~Size,B11plots_OP,ylab="Nb of oil palms",xlab="Plot size",
#        main="Measured number of adjacent palms per plot size")

# alternative: 
# use the extrapolated number of oil palms directly adjacent, based on Gérard et al. 2017
B11plots_OP$Nb_palm_adj <-  0
B11plots_OP[Size==5]$Nb_palm_adj <- 5.85
B11plots_OP[Size==10]$Nb_palm_adj <- 7.9
B11plots_OP[Size==20]$Nb_palm_adj <- 12.41
B11plots_OP[Size==40]$Nb_palm_adj <- 21.39


# import yield data
# set working directory 
#setwd("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/")

# -- adjacent OP 
#Y.adj.201612        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201611-12.csv")
#Y.adj.201701        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201701.csv")
#Y.adj.201702        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201702.csv")
#Y.adj.201703        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201703.csv")
#Y.adj.201704        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201704.csv")
#Y.adj.201705        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201705.csv")
#Y.adj.201706        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_AdjacentPlot_201706-07.csv")

Y.adj.201710        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201710.csv")
Y.adj.201711        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201711.csv")
Y.adj.201712        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201712.csv")
Y.adj.201801        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201801.csv")
Y.adj.201802        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201802.csv")
Y.adj.201803        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201803.csv")
Y.adj.201804        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201804.csv")
Y.adj.201805        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201805.csv")
Y.adj.201806        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201806.csv")
Y.adj.201807        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201807.csv")
Y.adj.201808        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201808.csv")
Y.adj.201809        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201809.csv")
Y.adj.201810        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_AdjacentPlot_201810.csv")

# -- outside OP
#Y.out.201612        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201611-12.csv")
#Y.out.201701        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201701.csv")
#Y.out.201702        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201702.csv")
#Y.out.201703        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201703.csv")
#Y.out.201704        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201704.csv")
#Y.out.201705        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201705.csv")
#Y.out.201706        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201706-07.csv")

Y.out.201710        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_OutsidePlot_201710.csv")
Y.out.201711        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201711.csv")
Y.out.201712        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201712.csv")
Y.out.201801        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201801.csv")
Y.out.201802        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201802.csv")
Y.out.201803        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201803.csv")
Y.out.201804        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201804.csv")
Y.out.201805        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201805.csv")
Y.out.201806        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201806.csv")
Y.out.201807        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201807.csv")
Y.out.201808        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201808.csv")
Y.out.201809        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201809.csv")
Y.out.201810        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_OutsidePlot_201810.csv")

# -- inside OP 
#Y.in.201612        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201611-12.csv")
#Y.in.201701        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201701.csv")
#Y.in.201702        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201702.csv")
#Y.in.201703        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201703.csv")
#Y.in.201704        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201704.csv")
#Y.in.201705        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201705.csv")
#Y.in.201706        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201706-07.csv")

Y.in.201710        <- fread("C:/CRC990-B11/Data/OilPalms/OPyield_InsidePlot_201710.csv")
Y.in.201711        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201711.csv")
Y.in.201712        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201712.csv")
Y.in.201801        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201801.csv")
Y.in.201802        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201802.csv")
Y.in.201803        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201803.csv")
Y.in.201804        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201804.csv")
Y.in.201805        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201805.csv")
Y.in.201806        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201806.csv")
Y.in.201807        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201807.csv")
Y.in.201808        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201808.csv")
Y.in.201809        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201809.csv")
Y.in.201810        <- fread("C:/CRC990-B11/Data/OilPalms/OilPalmYieldPerFruit/OPyield_InsidePlot_201810.csv")

# combine the data
Y.in = rbind(Y.in.201711,Y.in.201712,
             Y.in.201801,Y.in.201802,Y.in.201803,
             Y.in.201804,Y.in.201805,Y.in.201806,
             Y.in.201807,Y.in.201808,Y.in.201809,Y.in.201810)
Y.out = rbind(Y.out.201711,Y.out.201712,
              Y.out.201801,Y.out.201802,Y.out.201803,
              Y.out.201804,Y.out.201805,Y.out.201806,
              Y.out.201807,Y.out.201808,Y.out.201809,Y.out.201810)
Y.adj = rbind(Y.adj.201711,Y.adj.201712,
              Y.adj.201801,Y.adj.201802,Y.adj.201803,
              Y.adj.201804,Y.adj.201805,Y.adj.201806,
              Y.adj.201807,Y.adj.201808,Y.adj.201809,Y.adj.201810)

# rename 
names(Y.in)[4:11] <- c("w1","w2","w3","w4","w5","w6","w7","w8")
names(Y.out)[3:10] <- c("w1","w2","w3","w4","w5","w6","w7","w8")
names(Y.adj)[4:11] <- c("w1","w2","w3","w4","w5","w6","w7","w8")


#--------------------------------------------------#
# -- inside oil palms 
#--------------------------------------------------#

# sum the weight of all fruits for each palm and each harvesting date 
Y.in.palm.date = Y.in[,.(w = sum(w1,w2,w3,w4,w5,w6,w7,na.rm=TRUE)) 
                      ,.(PlotID,PalmID,Date)]

########################################################################
# Optional : merge with oil palm yield of earlier dates available at palm level 

Y.in.palm.date2 = rbind(Y.in.201612,Y.in.201701,Y.in.201702,Y.in.201703,
             Y.in.201704,Y.in.201705,Y.in.201706)

# convert to data frame 
df_in  <- as.data.frame(Y.in.palm.date)
df_in2 <- as.data.frame(Y.in.palm.date2)

# rename 
names(df_in2) <- c("Harvester","Date","PlotID","PalmID","w")

# get rid of the 1st column (harvester)
df_in2 <- df_in2[,(2:dim(df_in2)[2])]  

# merge all 
Y.in.palm.all = rbind(df_in,df_in2)

# convert to numeric 
Y.in.palm.all$w <- as.numeric(Y.in.palm.all$w)

# convert back to data table
Y.in.palm.date <- as.data.table(Y.in.palm.all)
########################################################################

# sum total weight over time 
Y.in.palm = Y.in.palm.date[,.(w.in.tot = sum(w,na.rm=TRUE)) 
                 ,.(PlotID,PalmID)]



# check distribution
#x11(); boxplot(Y.in.palm$w.in.tot)
#x11(); hist(Y.in.palm$w.in.tot)
#Y.in.palm[which(Y.in.palm$w.in.tot>500)] <- NA  # exclude outliers 

# get the mean of all oil palms inside the plot
Y.in.plot = Y.in.palm[,.(w.in.mean = mean(w.in.tot,na.rm=TRUE)) 
                           ,.(PlotID)]


#--------------------------------------------------#
# -- adjacent oil palms 
#--------------------------------------------------#

# sum the weight of all fruits for each palm and each harvesting date 
Y.adj.palm.date = Y.adj[,.(w = sum(w1,w2,w3,w4,w5,w6,w7,na.rm=TRUE)) 
                      ,.(PlotID,PalmID,Date)]


########################################################################
# Optional : merge with oil palm yield of earlier dates available at palm level 

Y.adj.palm.date2 = rbind(Y.adj.201612,Y.adj.201701,Y.adj.201702,Y.adj.201703,
                        Y.adj.201704,Y.adj.201705,Y.adj.201706)

# convert to data frame 
df_adj  <- as.data.frame(Y.adj.palm.date)
df_adj2 <- as.data.frame(Y.adj.palm.date2)

# rename 
names(df_adj2) <- c("Harvester","Date","PlotID","PalmID","w")

# get rid of the 1st column (harvester)
df_adj2 <- df_adj2[,(2:dim(df_adj2)[2])]  

# merge all 
Y.adj.palm.all = rbind(df_adj,df_adj2)

# convert to numeric 
Y.adj.palm.all$w <- as.numeric(Y.adj.palm.all$w)

# convert back to data table
Y.adj.palm.date <- as.data.table(Y.adj.palm.all)
########################################################################

# sum total weight over time 
Y.adj.palm = Y.adj.palm.date[,.(w.adj.tot = sum(w,na.rm=TRUE)) 
                           ,.(PlotID,PalmID)]

# get the data for each category individually for each plot 
Y.adj.palm <- merge(OP_adj_pos,Y.adj.palm ,by="PalmID",all.x=TRUE)
Y.adj1.plot <- Y.adj.palm[DistPos==1,c("PlotID.x","w.adj.tot")] 
Y.adj2.plot <- Y.adj.palm[DistPos==2,c("PlotID.x","w.adj.tot")] 
Y.adj3.plot <- Y.adj.palm[DistPos==3,c("PlotID.x","w.adj.tot")] 

# rename 
names(Y.adj1.plot)[1] <- "PlotID"
names(Y.adj2.plot)[1] <- "PlotID"
names(Y.adj3.plot)[1] <- "PlotID"
names(Y.adj.palm)[2] <- "PlotID"
Y.adj.palm <- Y.adj.palm[,c(1,2,3,5)] # remove duplicated column 


#--------------------------------------------------#
# -- control oil palms 
#--------------------------------------------------#

# sum the weight of all fruits for each palm and each harvesting date 
Y.out.palm.date = Y.out[,.(w = sum(w1,w2,w3,w4,w5,w6,w7,na.rm=TRUE)) 
                        ,.(PalmID,Date)]


########################################################################
# Optional : merge with oil palm yield of earlier dates available at palm level 

Y.out.palm.date2 = rbind(Y.out.201612,Y.out.201701,Y.out.201702,Y.out.201703,
                         Y.out.201704,Y.out.201705,Y.out.201706)

# convert to data frame 
df_out  <- as.data.frame(Y.out.palm.date)
df_out2 <- as.data.frame(Y.out.palm.date2)

# rename 
names(df_out2) <- c("Harvester","Date","PalmID","w")

# get rid of the 1st column (harvester)
df_out2 <- df_out2[,(2:dim(df_out2)[2])]  

# merge all 
Y.out.palm.all = rbind(df_out,df_out2)

# convert to numeric 
Y.out.palm.all$w <- as.numeric(Y.out.palm.all$w)

# convert back to data table
Y.out.palm.date <- as.data.table(Y.out.palm.all)

########################################################################

# sum total weight over time 
Y.out.palm = Y.out.palm.date[,.(w.out.tot = sum(w,na.rm=TRUE)) 
                             ,.(PalmID)]


# -- choose reference value 

Y.ref <- median(Y.out.palm$w.out.tot) # 220 kg/tree/year  
#Y.ref <- mean(Y.out.palm$w.out.tot) # 229 kg/tree/year  
# sd(Y.out.palm$w.out.tot) # 103 kg/tree/year  

#median(Y.adj2.plot$w.adj.tot) # 253 kg/tree/year 
#mean(Y.adj2.plot$w.adj.tot) # 239 kg/tree/year
#sd(Y.adj2.plot$w.adj.tot) # 88 kg/tree/year  

#median(Y.adj3.plot$w.adj.tot) # 242 kg/tree/year 
#mean(Y.adj3.plot$w.adj.tot) # 245 kg/tree/year
#sd(Y.adj3.plot$w.adj.tot) # 86 kg/tree/year  

#median(Y.in.plot[PlotID>52]$w.in.mean) # 173 kg/tree/year 
#mean(Y.in.plot[PlotID>52]$w.in.mean) # 178 kg/tree/year
#sd(Y.in.plot[PlotID>52]$w.in.mean) # 49 kg/tree/year  

#median(Y.adj.palm$w.adj.tot) # 253 kg/tree/year 
#mean(Y.adj.palm$w.adj.tot) # 252.5 kg/tree/year 
#sd(Y.adj.palm$w.adj.tot) # 92 kg/tree/year  



########################################################################
# -- calculate the yield changes - Method fron Anne Gérard et al. 2017

# -- merge the yield data at plot level (mean per plot): inside plot and directly adjacent (position 1)
Y.plot <- merge(Y.in.plot,Y.adj1.plot,by="PlotID",all=TRUE)
Y.plot <- merge(Y.plot,B11plots_OP,by="PlotID",all=TRUE)

# -- calculate the different components of plot-level yield 
Y.change.plot <- Y.plot[,.(Y.Foregone = OilPalm_Cut*Y.ref,  # equ. 1 in gerard et al. 2017
                               Y.RemainChange = OilPalm_Remain*(w.in.mean - Y.ref),# equ. 2 in gerard et al. 2017
                               Y.Spillover = Nb_palm_adj*(w.adj.tot-Y.ref))# equ. 3 in gerard et al. 2017
                            ,.(PlotID,Size)]


# -- set NA values to 0 (no changes monitored)
ind1 = which(is.na(Y.change.plot$Y.Spillover))
Y.change.plot[ind1]$Y.Spillover <- 0 # no spill-over effect in the control plots 
ind2 = which(is.na(Y.change.plot$Y.RemainChange))
Y.change.plot[ind2]$Y.RemainChange <- 0

# -- calculate the net yield change 
Y.net.plot <- Y.change.plot[,.(Y.NetChange = Y.Spillover+Y.RemainChange-Y.Foregone,
                               Y.NetChange.NoSpillOver = Y.RemainChange-Y.Foregone),.(PlotID,Size)]





########################################################################
# -- calculate the yield changes - Method 2 from hendrick Lorenz (see MSc thesis)

# -- load expansion factor (EF) 

#pd_mean_ef <- fread("C:/CRC990-B11/Data/OilPalms/pd_mean_ef.csv") # mean per plot (useless !)
pd_ef <- fread("C:/CRC990-B11/shared_doc_data/B11_stuff_from_Hendrik/ins_long_sum_and_ef.csv") # per individual oil palm 
pd_ef <- pd_ef[,c(2,3,5)] # select variables of interest 
names(pd_ef) <- c("PalmID","PlotID","EF_12m_cut") # rename 

# -- merge expansion factor with oil palm yield data 
#merge0 <- merge(pd_mean_ef,Y.in.plot,by="PlotID",all = TRUE) # at plot level (useless)
merge0 <- merge(pd_ef,Y.in.palm,by="PalmID",all = TRUE) # at palm level 
merge1 <- merge(merge0,Y.out.palm,by="PalmID",all = TRUE) # at palm level 

merge0 <- merge0[,c(1,3,4,5)] # remove duplicated column 
names(merge0)[3] <- "PlotID" # rename 

# 120 palms / ha in the control plots 
merge0[PlotID>52]$EF_12m_cut <- 120

# multiply the yield per palm with the expansion factor 
merge0 <- transform(merge0, w.in.tot.EF = w.in.tot *EF_12m_cut)

Y.ref.EF <- Y.ref*120

# get change per palm scaled-up to hectare 
Y.palm.2 <- merge0[,.(Y.thinning.palm = ((w.in.tot.EF-Y.ref.EF)/Y.ref.EF)),# relative yield change / ha  
                   ,.(PalmID)]

# get mean change per plot 
Y.plot.2 <- merge0[,.(Y.thinning.plot = mean(w.in.tot.EF)),# yield / ha  
                   ,.(PlotID)]

Y.change.plot.2 <- Y.plot.2[,.(Y.NetChange.NoSpillOver2 = Y.thinning.plot - Y.ref.EF) # net change compare to reference
                   ,.(PlotID)]

###############################################################################
# merge all and save the file

Y.net.plot <- merge(Y.net.plot, Y.change.plot.2, by="PlotID",all = TRUE)   
#Y.net.plot <- merge(Y.net.plot, Y.plot.2, by="PlotID",all = TRUE)   

Y.net.plot[B11plots_OP$OilPalm_Remain==0]$Y.NetChange.NoSpillOver2 = 0 # set plots with no remaining oil palm to 0 (because no change)

#write.csv(Y.plot, file = "Y.B11plot.csv")
#write.csv(Y.out.palm, file = "Y.out.palm.csv")

write.csv(Y.net.plot, file = paste0(Sys.Date(),"-Y.net.plot.csv"),row.names=F)

################################################################################# 
# Compared the methods with scatter plots

# -- oil palm yield 
#Y.net.plot <- fread("Y.net.plot.csv")
#Y.net.plot <- Y.net.plot[,c("PlotID","Y.NetChange","Y.NetChange.NoSpillOver","Y.NetChangeNoSpillover.2")] # get variable of interest 

x11(); 
(d <- ggplot(Y.net.plot, aes(x = Y.NetChange.NoSpillOver, y = Y.NetChange.NoSpillOver2))
 + geom_point(aes(color=Size))
 + ylab("oil palm yield change in ha / year (Method 2)") 
 + xlab("oil palm yield change in ha / year (Method Gérard et al.) excluding spillover"))

x11(); 
(d <- ggplot(Y.net.plot, aes(x = Y.NetChange, y = Y.NetChange.NoSpillOver2))
 + geom_point(aes(color=Size))
 + ylab("oil palm yield change in ha / year (Method 2)") 
 + xlab("oil palm yield change in ha / year (Method Gérard et al.) including spillover"))


