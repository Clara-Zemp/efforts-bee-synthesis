
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS :
#
# Statsistical analysis of oil palm yield data  (Based on models described in Gérard et al. 2017) 
#
####################################################################################################


library(ggplot2)   
library(data.table)
library(nlme)
library(sandwich) 

# -- Plot effect 
M1 <- Y.adj.palm[,c(1,2,4)]
M1 <- M1[,c(2,1,3)] # re-order the columns 
M2 <- Y.in.palm[PlotID<53] # include only experimental plots 
M3 <- Y.out.palm
M3$PlotID <- seq(57,(56+dim(Y.out.palm)[1])) # set arbitrary plot IDs 
M3 <- M3[,c(3,1,2)] # re-order the columns 
M4 <- Y.in.palm[PlotID>52] # control plots (53-56)

# rename 
names(M1)<- c("PlotID","PalmID","w.tot")
names(M2)<- c("PlotID","PalmID","w.tot")
names(M3)<- c("PlotID","PalmID","w.tot")
names(M4)<- c("PlotID","PalmID","w.tot")

# dummy code for tratement type 
M1$treat <- 1 # adjacent (position 1, 2 3)
M2$treat <- 2 # inside plot 
M3$treat <- 3 # outside (reference palm)
M4$treat <- 3 # outside (control plots)

M <- rbind(M1,M2,M3,M4)
M$treat <- as.factor(M$treat)

# get mean value and deviation (optional)
mean(M[treat==1]$w.tot,na.rm=TRUE)
sd(M[treat==1]$w.tot,na.rm=TRUE)

mean(M[treat==2]$w.tot,na.rm=TRUE)
sd(M[treat==2]$w.tot,na.rm=TRUE)

mean(M[treat==3]$w.tot,na.rm=TRUE)
sd(M[treat==3]$w.tot,na.rm=TRUE)

# -- plot effects 

lm1 <- lmer(w.tot ~ treat + (1|PlotID), data = M)
summary(lm1)
anova(lm1)
car::Anova(lm1) 

summary(glht(lm1, linfct = mcp(treat = "Tukey"))) # Tukey Test for pairwise comparison 
#ph_tukey_rse          <- summary(glht(lm1, linfct = mcp(treat = "Tukey"), vcov = sandwich)) # Tukey Test with robust SE

x11(3,3)
ggplot(M, aes(x = treat, y = w.tot)) +
  geom_boxplot(outlier.shape=NA, aes(group = treat)) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0),aes(),show.legend = FALSE)


# -- distance effects 
Y.adj.palm$DistPos <- as.factor(Y.adj.palm$DistPos)

lm2 <- lmer(w.adj.tot ~ DistPos + (1|PlotID), data = Y.adj.palm)
summary(lm2)
anova(lm2)
car::Anova(lm2) 

summary(glht(lm2, linfct = mcp(DistPos = "Tukey"))) # Tukey Test for pairwise comparison 


x11(3,3)
ggplot(Y.adj.palm, aes(x = DistPos, y = w.adj.tot)) +
  geom_boxplot(outlier.shape=NA, aes(group = DistPos)) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0),aes(),show.legend = FALSE) 


# -- tree planting effect 
Y.adj.palm = merge(Y.adj.palm, B11plots_OP, by="PlotID")
Y.adj.palm$TreePlanting <- 0
Y.adj.palm[DivLev==0]$TreePlanting <- 0
Y.adj.palm[DivLev>0]$TreePlanting <- 1
Y.adj.palm$TreePlanting <- as.factor(Y.adj.palm$TreePlanting)
lm3 <- lmer(w.adj.tot ~ TreePlanting + (1|PlotID), data = Y.adj.palm)
#lm3 <- lmer(w.adj.tot ~ TreePlanting*DistPos + (1|PlotID), data = Y.adj.palm) # with interactions 
summary(lm3)
anova(lm3)
car::Anova(lm3) 

summary(glht(lm3, linfct = mcp(TreePlanting = "Tukey"))) # Tukey Test for pairwise comparison 

x11(3,3)
ggplot(Y.adj.palm, aes(x = TreePlanting, y = w.adj.tot)) +
  geom_boxplot(outlier.shape=NA, aes(group = TreePlanting)) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0),aes(),show.legend = FALSE) 


# -- thinning effect 
Y.adj.palm$Thinning <- 0
Y.adj.palm[OilPalm_Cut==0]$Thinning <- 0
Y.adj.palm[OilPalm_Cut>0]$Thinning <- 1
Y.adj.palm$Thinning <- as.factor(Y.adj.palm$Thinning)
lm4 <- lmer(w.adj.tot ~ Thinning + (1|PlotID), data = Y.adj.palm) # 
#lm4 <- lm(w.adj.tot ~ Thinning*DistPos + (1|PlotID), data = Y.adj.palm) # with interaction term 
summary(lm4)
anova(lm4)
car::Anova(lm4) 

summary(glht(lm4, linfct = mcp(Thinning = "Tukey"))) # Tukey Test for pairwise comparison 

x11(3,3)
ggplot(Y.adj.palm, aes(x = Thinning, y = w.adj.tot)) +
  geom_boxplot(outlier.shape=NA, aes(group = Thinning)) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0),aes(),show.legend = FALSE) 

##################################################################################
# per-area yield with expansion factor 

# merge data with expansion factor 
M2 = merge(M,pd_ef,by="PalmID", all=TRUE)

# keep only the variables of interest 
M2 <- M2[,c(1,2,3,4,6)]
#M2 <- M2[,c(1,2,3,4,6,7)]

# set palm not affected by thinning to reference density 
M2[treat==1]$EF_12m_cut=120
M2[treat==3]$EF_12m_cut=120

# multiply the yield per palm with the expansion factor 
M2 <- transform(M2, w.tot.EF = w.tot *EF_12m_cut)

# boxplots
x11(3,3)
ggplot(M2, aes(x = treat, y = w.tot.EF)) +
  geom_boxplot(outlier.shape=NA, aes(group = treat)) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0),aes(),show.legend = FALSE) 

# linear models 
lm5 <- lmer(w.tot.EF ~ treat + (1|PlotID.x), data = M2)
summary(lm5)
anova(lm5)
car::Anova(lm5)

summary(glht(lm5, linfct = mcp(treat = "Tukey"))) # Tukey Test for pairwise comparison 

### save data 
write.csv(M2, file = paste0(Sys.Date(),"-Y.palm.csv"),row.names=F)

