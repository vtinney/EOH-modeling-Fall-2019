# Script to map the difference in rate inputs distribution

library(raster)
library(rgdal)
library(dplyr)
library(geosphere)
library(spdep)
library(ape)
library(spatialreg)
library(spatialEco)
library(spatstat)

# Set up the inputs ====================================================================

setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/diff analysis/Presentation_7.2.2019/")

# Results

# Inputs

setwd("C:/Users/Veronica Tinney/Google Drive/EOH Modeling/Final Project/")
list.files()

conc <- raster('dc.conc.tif')
weight <- raster('weight.tif')
smoke <- raster('smoke.tif')
hist <- raster('hist.tif')
dis <- raster('dis.tif')

#=======================================================================
# Model inputs

conc[conc == 0] <- NA
hist[hist == 0] <- NA
smoke[smoke == 0] <- NA
weight[weight == 0] <- NA
dis[dis == 0] <- NA

lm.p1 <- rasterToPoints(conc)
lm.p2 <- rasterToPoints(weight)
lm.p3 <- rasterToPoints(hist)
lm.p4 <- rasterToPoints(smoke)
lm.p5 <- rasterToPoints(dis)

df1 <- data.frame(lm.p1)
df2 <- data.frame(lm.p2)
df3 <- data.frame(lm.p3)
df4 <- data.frame(lm.p4)
df5 <- data.frame(lm.p5)

colnames(df1) <- c('lon','lat','conc')
colnames(df2) <- c('lon','lat','weight')
colnames(df3) <- c('lon','lat','hist')
colnames(df4) <- c('lon','lat','smoke')
colnames(df5) <- c('lon','lat','dis')

data1 <- cbind(df2, df3, df4, df5)
data2 <- data1[,c(1,2,3,6,9,12)]

mat <- distm(data2[,c('lon','lat')], df1[,c('lon','lat')], fun=distVincentyEllipsoid)
data2$conc <- df1$conc[apply(mat, 1, which.min)]

df1 <- data2

lm1 <- lm(conc[] ~ hist[], data=df1)
lm2 <- lm(conc[] ~ weight[], data=df1)
lm3 <- lm(conc[] ~ smoke[], data=df1)
lm4 <- lm(conc[] ~ dis[], data=df1)

### Auto-logistic model using 'autocov_dist' in 'spdep' package
lm1 <- logistic.regression(df1, y='dis', x=c('hist'),
                              autologistic=TRUE, coords=coordinates(df1), bw=5000) 

### Auto-logistic model using 'autocov_dist' in 'spdep' package
lm2 <- logistic.regression(df1, y='dis', x=c('weight'),
                              autologistic=TRUE, coords=coordinates(df1), bw=5000) 

### Auto-logistic model using 'autocov_dist' in 'spdep' package
lm3 <- logistic.regression(df1, y='dis', x=c('smoke'),
                              autologistic=TRUE, coords=coordinates(df1), bw=5000) 

### Auto-logistic model using 'autocov_dist' in 'spdep' package
lm4 <- logistic.regression(df1, y='dis', x=c('conc'),
                              autologistic=TRUE, coords=coordinates(df1), bw=5000) 

#==========================================================================================
# Assess spatial auto-correlation
# Moran's I for residuals

library(ape)

  # Create the inverse distance matrix
  df1.dists <- dist(cbind(df1$lon, df1$lat))
  
  df1.dists.inv = 1/df1.dists
  df1.dists.inv <- as.matrix(df1.dists.inv)
  diag(df1.dists.inv) <- 0
  
  df1.dists.inv[1:5, 1:5]
 
  # Compute the Moran's I statistic
  m1 <- Moran.I(df1$weight, df1.dists.inv)
  m2 <- Moran.I(df1$hist, df1.dists.inv)
  m3 <- Moran.I(df1$smoke, df1.dists.inv)
  m4 <- Moran.I(df1$dis, df1.dists.inv)
  
  #=================================================================================
  # Attempt 2 - adjusting for adjacent nearest neighbors
  
  
  coordinates(df1) <- ~ lon + lat
  
  # Use K nearest neighbor to create a nearest neighbor list
  knea.df1 <- knearneigh(coordinates(df1), longlat = TRUE)
  neib.df1 <- knn2nb(knea.df1)
  
  # Get the mean of the adjacent residuals
  resnb.df1 <- sapply(neib.df1, function(x) mean(lm1$Residuals$res[x]))
  resnb.df2 <- sapply(neib.df1, function(x) mean(lm2$Residuals$res[x]))
  resnb.df3 <- sapply(neib.df1, function(x) mean(lm3$Residuals$res[x]))
  resnb.df4 <- sapply(neib.df1, function(x) mean(lm4$Residuals$res[x]))
  
  # Plot residuals and the mean of adjacent residuals and see there is no independence of the 
  # residuals
  plot(lm1$Residuals$res, resnb.df1, xlab='Residuals History and Concentration', ylab='Mean adjacent residuals',col='blue')
  plot(lm2$Residuals$res, resnb.df2, xlab='Residuals Weight and Concentration', ylab='Mean adjacent residuals',col='red')
  plot(lm3$Residuals$res, resnb.df3, xlab='Residuals Smoking and Concentration', ylab='Mean adjacent residuals',col='green')
  plot(lm4$Residuals$res, resnb.df4, xlab='Residuals Disease and Concentration', ylab='Mean adjacent residuals',col='purple')
  
  # Add sampling weights and compute Monte Carlo Moran's Statistic
  # MC just calculates a sample distribution
  lw.1 <- nb2listw(neib.df1)
  moran.mc(lm1$Residuals$res, lw.1, 999)  
  
  lw.2 <- nb2listw(neib.df1)
  moran.mc(lm2$Residuals$res, lw.2, 999) 
  
  lw.3 <- nb2listw(neib.df1)
  moran.mc(lm3$Residuals$res, lw.3, 999) 
  
  lw.4 <- nb2listw(neib.df1)
  moran.mc(lm4$Residuals$res, lw.4, 999) 
  
