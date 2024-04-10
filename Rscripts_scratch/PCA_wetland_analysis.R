###################################################################################
#Name: Initial Analysis 
#Coder: C. Nathan Jones
#Edited: Morgan Schulte
#Date: 1/10/17
#Purpose: Morgan's Great Dismal Swamp Dataset 
##################################################################################

##################################################################################
#Setup Workspace and Organize Data------------------------------------------------
##################################################################################
#Clear Memory
rm(list=ls(all=TRUE)) 

#Download Relevant Libraries (to download library: 
install.packages("vegan")
install.packages("clustsig")
library(vegan)
library(clustsig)
library(here)

#Set working directory (You'll need to change this based on your computer)
#setwd("/Volumes/SCHULTE/R_Ordination_Nate")

#Download Data and defining df and omitting zeros
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-03-27.csv"))
df<-na.omit(df)

##################################################################################
#Ordination Analysis--------------------------------------------------------------
##################################################################################
#Conduct Ordination~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define Ordination dataframe
x<-df[,-c(1,2,3,6,8,17)] #Remove related variables 

#Normalize values (How is this normalizing values?) 
x<-scale(x)

#Conduct NMDS Analysis (You may want to explore other "distance" measures.  Bray
#                       is commonly used for population "0" heavy data.)
set.seed(7) #Set random seed for consistent outputs
n<-1000 #Set number of iterations
mds.1 <-metaMDS(x,distance = "euclidean", k=1, trymax = n)
mds.2 <-metaMDS(x,distance = "euclidean", k=2, trymax = n)
mds.3 <-metaMDS(x,distance = "euclidean", k=3, trymax = n)
mds.4 <-metaMDS(x,distance = "euclidean", k=4, trymax = n)
mds.5 <-metaMDS(x,distance = "euclidean", k=5, trymax = n)
mds.6 <-metaMDS(x,distance = "euclidean", k=6, trymax = n)

#Select Axes and examine stress~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Scree Plot (To optimze number of axes)
stress<-c(mds.1$stress, mds.2$stress, mds.3$stress, mds.4$stress, mds.5$stress, mds.6$stress)
plot(stress,
     ylim = c(0,0.3),
     xlab="Number of Axes",
     type = "o",
     ylab="Stress",
     cex.lab=1.6,
     lty = 2,
     lwd = 3,
     pch=21,
     col= "black",
     bg= "red",
     cex = 3,
     bty="l")
box(lwd=2, 
    bty = "l")

#Investigate "stress" of deformation
par(mfrow=c(1,2))
stressplot(mds.3,
           main = "Shepard Plot")
gof = goodness(mds.3)
plot(mds.3, 
     type = "p",
     main = "Goodness of Fit",
     cex = gof*200)
par(mfrow=c(1,1))

#NMDS Plots (The $$ Makers!)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot Ordination
plot(mds.2$points[,1],mds.2$points[,2],
     pch=21,
     col= "black",
     bg= "gray",
     bty = "L",
     lwd = 2,
     cex=2.5,
     xlab = "MDS1",
     ylab = "MDS2")

#Fit and plot environmental vectors
ef2<-envfit(mds.2, x, permu=n)
plot(ef2)

#Add NMDS coorinates to master dataframe
df$MDS1<-mds.2$points[,1]
df$MDS2<-mds.2$points[,2]

##################################################################################
#Cluster Analysis-----------------------------------------------------------------
##################################################################################
#Subest data for cluster analysis
x<-df[,c("MDS1","MDS2")]

#Cluster Analysis
d <- vegdist(x, method = "euclidean") # distance matrix
fit <- hclust(d, method = "ward") #Heirarchal Cluster
clusters2 <- cutree(fit, k=2) # cut tree into 2 clusters
clusters3 <- cutree(fit, k=3) # cut tree into 3 clusters

#Plot Dendrogram (Both 2 and 3 Groups)
plot(fit, 
     sub="Sampling Site", 
     hang=-0.5, 
     main = NULL,
     labels = FALSE,
     ylab="% Information Remaining") 
title("Cluster Analysis: Ward's Mimium Variance (Euclidean Distance)", line = 3, cex =2)
title("2 Groups & 3 Groups", line = 2)
rect.hclust(fit, k=3, border= c("red","blue", "green"))
rect.hclust(fit, k=2, border= c("red","purple"))

#Add to master dataframe
df$cluster2<-clusters2
df$cluster3<-clusters3

#Plot NMDS with groups
color<-ifelse(df$cluster2==1,"red","blue")
plot(df$MDS1,df$MDS2,
     xlim=c(-4,4),ylim=c(-4,4),
     pch=21,
     col= "black",
     bg= color,
     bty = "L",
     lwd = 2,
     cex=2.5,
     xlab = "MDS1",
     ylab = "MDS2")
plot(ef2, col="black")

##################################################################################
#Multiple responce Permutation Procedure------------------------------------------
##################################################################################
#Create dataframe for MRPP test (Note, I'm ranking data)
mrpp.data<-data.frame(rank(df$Diversity),
                      rank(df$Richness),
                      rank(df$Tree_Map_BA),
                      rank(df$Tree_Map_Den),
                      rank(df$Tree_BA),
                      rank(df$Tree_Den),
                      rank(df$Map_Dom),
                      rank(df$Shrub_BA),
                      rank(df$Shrub_Den),
                      rank(df$Seedling_Den),
                      rank(df$Seedling_Map_Den),
                      rank(df$Peat_Depth_cm),
                      rank(df$Whole_Plot_Mean_WTH_m),
                      rank(df$SD_WTH_m),
                      rank(df$X10_WTH),
                      rank(df$X90_WTH),
                      rank(df$Microtopo_SD))

#Compute MRPP
mrpp.2group<-mrpp(mrpp.data, clusters2, permutations = n, distance = "euclidean")
mrpp.3group<-mrpp(mrpp.data, clusters3, permutations = n, distance = "euclidean")

#display the p-values and A of both tests
mrpp.2group$Pvalue
mrpp.3group$Pvalue

mrpp.2group$A
mrpp.3group$A


