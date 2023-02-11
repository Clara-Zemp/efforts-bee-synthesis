
####################################################################################################
#
# EFForTS-BEE Data ANALYSIS:
#
# Visualization of the SEM results 
#
####################################################################################################

library(tidyverse)

theme_set(theme_bw())

# delete all objects
rm(list=ls())

# set working directory 
setwd("C:/CRC990-B11/Scripts/efforts-bee-synthesis-paper-1/Data")

# load SEM results 

# H2 (Simpson diversity)
df_raw <- read.csv("2023-02-11-Coefs.matrix.biodiv.H2.csv")
SEMStat <- read.csv("2023-02-11-SEMStat.biodiv.H2.csv")

# H1 (Shannon diversity) 
df_raw <- read.csv("2023-02-11-Coefs.matrix.biodiv.H1.csv")
SEMStat <- read.csv("2023-02-11-SEMStat.biodiv.H1.csv")

# Ho (Species richness)
df_raw <- read.csv("2023-02-11-Coefs.matrix.biodiv.H0.csv")
SEMStat <- read.csv("2023-02-11-SEMStat.biodiv.H0.csv")

# ecosystem functioning 
df_raw <- read.csv("2023-02-11-Coefs.matrix.EF.csv")
SEMStat <- read.csv("2023-02-11-SEMStat.EF.csv")

# multifunctionality and multidiversity 
df_raw <- read.csv("2023-02-11-Coefs.matrix.Multi.csv")
SEMStat <- read.csv("2023-02-11-SEMStat.Multi.csv")

#########################################################################
# prepare for plotting 

#--select variable of interest
df <- df_raw[c("Response", "Predictor", "P.Value", "Std.Estimate", "Estimate")]

#--Remove str1 and str2 values since these are only used to calc indirect estimates
df <- df[-which(str_detect(df$Response,"str")),]
# df  <- df[-grep("str", df$Response),] # alternative function 


# -- include only models with high explanatory power 
#var_include <- SEMStat[which(SEMStat$R2>0.4),]$var_name
#df <- df[which(df$Response %in% var_include),]

#--How coefs are calculated
#plotsite.direct = c | plotsite.indirect = a*b
#treediv.direct = f | treediv.indirect = c*d

# The predictors remain the same b/w datasets so we filter and subset based on those where possible
# relationship when str is response is also constant between datasets

#--assign a and c as columns
#c and a are where str1 and str2 are the reponse variable
df$a <- df_raw$Std.Estimate[df_raw$Response == "str2" & df_raw$Predictor == "plotsizeLog"]
df$c <- df_raw$Std.Estimate[df_raw$Response == "str1" & df_raw$Predictor == "divlevelLog"]

#--calc direct and indirect coefs
#b and d are where str1 and str2 are the predictors
coefs <- df %>%
  mutate(coefs = case_when(Predictor == "str1" ~ Std.Estimate*c,
                           Predictor == "str2" ~ Std.Estimate*a,
                           TRUE ~ Std.Estimate)) %>%
  mutate(Group = case_when(Predictor == "str1" ~ "Indirect Tree Diversity",
                           Predictor == "str2" ~ "Indirect Plot Size",
                           Predictor == "plotsizeLog" ~ "Direct Plot Size",
                           Predictor == "divlevelLog" ~ "Direct Tree Diversity")) %>%
  separate(Group, c("Type", "Predictor2"), sep = " ", remove = F, extra = "merge") %>%
  mutate(sig = as.factor(case_when(P.Value < 0.05 ~ "< 0.05",
                                   P.Value < 0.1 ~ "< 0.1",
                                   TRUE ~ "> 0.1")))


# -- alternative: use absolute estimate 

# df$a <- df_raw$Estimate[df_raw$Response == "str2" & df_raw$Predictor == "plotsize"] 
# df$c <- df_raw$Estimate[df_raw$Response == "str1" & df_raw$Predictor == "divlevel"]
#  
#  coefs <- df %>% 
#    mutate(coefs = case_when(Predictor == "str1" ~ Estimate*c,
#                             Predictor == "str2" ~ Estimate*a,
#                             TRUE ~ Estimate)) %>%
#    mutate(Group = case_when(Predictor == "str1" ~ "Indirect Tree Diversity",
#                             Predictor == "str2" ~ "Indirect Plot Size",
#                             Predictor == "plotsize" ~ "Direct Plot Size",
#                             Predictor == "divlevel" ~ "Direct Tree Diversity")) %>%
#    separate(Group, c("Type", "Predictor2"), sep = " ", remove = F, extra = "merge") %>%
#    mutate(sig = as.factor(case_when(P.Value < 0.05 ~ "< 0.05", 
#                                     P.Value < 0.1 ~ "< 0.1",
#                                     TRUE ~ "> 0.1"))) 

#########################################################################
# reordering 

#reorder factor levels to set order of bars = PlotSize , Tree Diversity
coefs$Group <- as.factor(coefs$Group)
coefs$Group <- factor(coefs$Group, levels = c("Direct Plot Size","Indirect Plot Size",
                                  "Direct Tree Diversity", "Indirect Tree Diversity"))

coefs$sig <- factor(coefs$sig, levels = c("> 0.1","< 0.1","< 0.05"))


# variable names 
var.names <- unique(as.character(df$Response))

# reorder the response variables 

coefs$Response <- factor(coefs$Response, 
                         levels = var.names)

SEMStat$var_name <- factor(SEMStat$var_name, 
                         levels = var.names)

#########################################################################
## -- barplots 

# -- Visualize coefficients 

# select only the significant coefs 
#ind <- which(coefs$sig == "< 0.05")
#coefs.sig <- coefs[ind,]

# or set to 0 value the non significant coefs 
coefs.sig <- coefs
coefs.sig[which(coefs$sig == "> 0.1"),]$coefs <- 0
coefs.sig[which(coefs$sig == "< 0.1"),]$coefs <- 0
coefs.sig[which(coefs$sig == "< 0.05"),]$coefs <- coefs[which(coefs$sig == "< 0.05"),]$coefs

x11(width=5, height=5)
#ggplot(SEMStat, aes(y=R2, x=var_name)) + ylim(0,1) + geom_col()
  
  
ggplot(coefs.sig, aes(fill = Predictor2, x=Response, y=coefs, alpha=Type)) + ylim(-0.5,0.5) +
  geom_col(position="stack", stat="identity",show.legend = FALSE, aes(alpha = Type, fill = Predictor2),
           linetype = "solid") + # Dodge, stack, (nudge), fill
#  scale_fill_manual(values = c(rgb(152/255,130/255,60/255), rgb(154/255,94/255,161/255))) +
  scale_fill_manual(values = c("black", "Darkgreen"))+
  scale_alpha_manual(values = c(1, 0.5))+
 labs(y= "Standard Estimate", x = NULL)
#  labs(y= "Absolute Estimate", x = NULL)

# -- Visualize R2

#SEMstat.sig <- SEMStat[coefs[ind,]$Response,]

