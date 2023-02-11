###################################
# Cluster Analysis for calculation of multifunctionality 
#
# script based on 
# Manning, P., Plas, F. Van Der, Soliveres, S., Allan, E., & Maestre, F. T. (2018). 
# Redefining ecosystem multifunctionality. Nature Ecology & Evolution, (February). http://doi.org/10.1038/s41559-017-0461-7
#
# modifified by Clara Zemp, 2021
#
####################################################################################################
library(data.table)
library(factoextra)
library(vegan)

# delete all objects
rm(list=ls())

# load data 
ef        <- as.data.frame(fread("2021-12-31-EF.transformed.csv"))
ef        <- as.data.frame(fread("2021-07-19-EF.csv"))

# standardize
df <- decostand(ef, "standardize")

# -- identify cluster of related functions 

funct_mat <- t(as.matrix(df))
d <- dist(funct_mat , method = "euclidean") 
dendrogram <- hclust(d, method = "complete" ) 
x11(); plot(dendrogram, cex = 0.6, hang = -1)

# -- find optimal number of clusters 

### Elbow method (look at the knee)
# Elbow method for kmeans
fviz_nbclust(funct_mat, kmeans, method = "wss") #+
 # geom_vline(xintercept = 3, linetype = 2)

# Average silhouette for kmeans
fviz_nbclust(funct_mat, kmeans, method = "silhouette")



### Gap statistic
library(cluster)
set.seed(123)
# Compute gap statistic for kmeans
# we used B = 10 for demo. Recommended value is ~500
gap_stat <- clusGap(funct_mat, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 100)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# Gap statistic for hierarchical clustering
gap_stat <- clusGap(funct_mat, FUN = hcut, K.max = 10, B = 10)
fviz_gap_stat(gap_stat)

# -- plot results for 9 clusters 

x11(); plot(dendrogram, cex = 0.6, hang = -1) 
rect.hclust(dendrogram, k = 7, border = 2:5)

# attribute the weight to each function, so that each cluster has a total weight = 1
loading_values <- c(1/3,1/3,1,1/3,1/4,1/4,1,1/3,1/2,1/2,1/2,1/3,1/2,1/3,1/2,1/2,1,1/4,1/4) 

#-- recode the function values so that those which exceed the threshold 
# are assigned a value of 1, and those below are assigned a value of 0.

mf_data_scaled <- matrix(nrow=nrow(df),ncol=Nfunc) 
threshold <- 0.90 
for(i in 1:Nfunc){
  maximum <- mean(sort(df[,i], decreasing=T)[c(1:5)]) # get mean of the highest 5 values 
  mf_data_scaled[,i] <- rep(0,length(mf_data_scaled[,i])) 
  high_values <- which(df[,i] >= threshold * maximum) 
  mf_data_scaled[high_values,i] <- loading_values[i]
}

Index.MF$MF_thres90 <- rowSums(mf_data_scaled) / 9

# save the file
#write.csv(df, file = "Index.MF.9clusters.tresh.csv")




####################################################################################################

library(ClustOfVar)

df <- EF[,2:29]

tree <- hclustvar(df, init=NULL)
x11(); plot(tree)      

stab <- stability(tree, B=50) # "B=50" refers to the number of bootstrap samples to use in the estimation.
plot(stab)


