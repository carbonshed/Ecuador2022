###################################################################################
#Name: Principle Componenet Analysis 
#Coder: C. Nathan Jones
#code editor: Kriddie Whitmore
#Date: 5/15/2024
#Purpose: Kriddie's GHG in paramo ponds
##################################################################################

##################################################################################
#Setup Workspace and Organize Data------------------------------------------------
##################################################################################
#Clear Memory
rm(list=ls(all=TRUE)) 

#Download Relevant Libraries (to download library: 
library(vegan)
library(here)

#Download Data and defining df and omitting zeros
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-04-14.csv"))
df$New.Name <- df$Site
df$site.date <- paste(df$Site,df$Date)

#make column with new name to reflect elevation. S stands for "site"
df <- df %>%
  mutate(New.Name = recode(New.Name, 'Wetland01'= 'S1', 'Wetland02' = 'S2','Wetland03' = 'S3',
                           'Wetland04' = 'S4','Wetland05' = 'S8','Wetland06' = 'S5', 
                           'Wetland07' = 'S7','Wetland08' = 'S12','Wetland09' = 'S10',
                           'Wetland10' = 'S11', 'Wetland11' = 'S6','Wetland12' = 'S9',))
df1 <- df%>%filter(Site!="Wetland12")%>%select(site.date,CO2_ppm,CO2_umol.L,CH4_umol.L,CH4_sat,
                                               Watertemp_c,Elevation_m,
                  DOC_mg.L,TDN_mg.L,#Baro_kpa,BaroTemp_c_eleAdjust,AirTemp_c,
                  depth_ave_m,
                  Volumn_m3,SA_to_Vol_ratio,precip_mm_ave2,pond_size,WS_area_minus_pond)
df2 <- df1%>%select(!CO2_ppm)%>%
  select(!CO2_umol.L)%>%
  select(!CH4_umol.L)%>%
  select(!CH4_sat)%>%
  select(!DOC_mg.L)%>%select(!TDN_mg.L)
df2 <- df2[,-1]
rownames(df2) <- df1[,1]

PCA <- rda(df2, scale = TRUE)
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 51%
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")
# You can extract the species and site scores on the new PC for further analyses:
sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

##vegan totrial2####

library(vegan)
library(ape)
library(dplyr)

data(varespec)
# With this command, you`ll perform a NMDS and plot the results
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

PCA <- rda(varespec, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")
# You can extract the species and site scores on the new PC for further analyses:
sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

# Check out the help file how to pimp your biplot further:
?biplot.rda

# You can even go beyond that, and use the ggbiplot package.
# You can install this package by running:
library(devtools)
library(ggbiplot)

# First step is to calculate a distance matrix. 
# Here we use Bray-Curtis distance metric
dist <- vegdist(varespec,  method = "bray")

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, varespec)

# Extract the plot scores from first two PCoA axes (if you need them):
PCOAaxes <- PCOA$vectors[,c(1,2)]

# Compare this result with the PCA plot
par(mfrow = c(1, 2)) 
biplot.pcoa(PCOA)
plot(PCA)

# reset plot window
par(mfrow = c(1, 1)) 




######vegan tutorial
data(dune)
ord <- decorana(dune)
ord
plot(ord)
plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(ord, display = "species", cex=0.7, col="blue")

plot(ord, type = "n") |>
  points("sites", cex = 0.8, pch=21, col="red", bg="yellow") |>
  text("species", cex=0.7, col="blue")

data(dune.env)
attach(dune.env)
plot(ord, disp="sites", type="n")
ordihull(ord, Management, col=1:4, lwd=3)
ordiellipse(ord, Management, col=1:4, kind = "ehull", lwd=3)
ordiellipse(ord, Management, col=1:4, draw="polygon")
ordispider(ord, Management, col=1:4, label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)

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


