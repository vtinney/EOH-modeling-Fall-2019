# Script to map the difference in rate inputs distribution

library(raster)
library(rgdal)
library(dplyr)
library(geosphere)
library(spdep)
library(ape)
library(spatialreg)

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

#=======================================================================
# Model inputs

conc[conc == 0] <- NA
hist[hist == 0] <- NA
smoke[smoke == 0] <- NA
weight[weight == 0] <- NA

lm.p1 <- rasterToPoints(conc)
lm.p2 <- rasterToPoints(weight)
lm.p3 <- rasterToPoints(hist)
lm.p4 <- rasterToPoints(smoke)

df1 <- data.frame(lm.p1)
df2 <- data.frame(lm.p2)
df3 <- data.frame(lm.p3)
df4 <- data.frame(lm.p4)

colnames(df1) <- c('lon','lat','conc')
colnames(df2) <- c('lon','lat','weight')
colnames(df3) <- c('lon','lat','hist')
colnames(df4) <- c('lon','lat','smoke')

data1 <- cbind(df2, df3, df4)
data2 <- data1[,c(1,2,3,6,9)]

mat <- distm(data2[,c('lon','lat')], df1[,c('lon','lat')], fun=distVincentyEllipsoid)
data2$conc <- df1$conc[apply(mat, 1, which.min)]

df1 <- data2

lm1 <- lm(conc[] ~ hist[], data=df1)
lm2 <- lm(conc[] ~ weight[], data=df1)
lm3 <- lm(conc[] ~ smoke[], data=df1)

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
  
  #=================================================================================
  # Attempt 2 - adjusting for adjacent nearest neighbors
  
  
  coordinates(df1) <- ~ lon + lat
  
  # Use K nearest neighbor to create a nearest neighbor list
  knea.df1 <- knearneigh(coordinates(df1), longlat = TRUE)
  neib.df1 <- knn2nb(knea.df1)
  
  # Get the mean of the adjacent residuals
  resnb.df1 <- sapply(neib.df1, function(x) mean(lm1$residuals[x]))
  
  # Plot residuals and the mean of adjacent residuals and see there is no independence of the 
  # residuals
  plot(lm1$residuals, resnb.df1, xlab='Residuals History and Concentration', ylab='Mean adjacent residuals',col='blue')
  plot(lm2$residuals, resnb.df1, xlab='Residuals Weight and Concentration', ylab='Mean adjacent residuals',col='red')
  plot(lm3$residuals, resnb.df1, xlab='Residuals Smoking and Concentration', ylab='Mean adjacent residuals',col='green')
  
  
  # Add sampling weights and compute Monte Carlo Moran's Statistic
  # MC just calculates a sample distribution
  lw.1 <- nb2listw(neib.df1)
  moran.mc(lm1$residuals, lw, 999)  
  
  lw.2 <- nb2listw(neib.df1)
  moran.mc(lm2$residuals, lw, 999) 
  
  lw.3 <- nb2listw(neib.df1)
  moran.mc(lm3$residuals, lw, 999) 
  
  #Spatial lag model hist
  m1s = lagsarlm(df1$conc ~ df1$hist, data=df1, lw,tol.solve=1.0e-30)
  df1$residuals.hist <- residuals(m1s)
  moran.mc(df1$residuals.hist, lw, 999) # p value of 0.844 sucessfully removes spatial autocorrelation

  #Spatial lag model weight
  m2s = lagsarlm(df1$conc ~ df1$weight, data=df1, lw,tol.solve=1.0e-30)
  df1$residuals.weight <- residuals(m2s)
  moran.mc(df1$residuals.weight, lw, 999) # p value of 0.833 sucessfully removes spatial autocorrelation
  
  #Spatial lag model smoke
  m3s = lagsarlm(df1$conc ~ df1$smoke, data=df1, lw,tol.solve=1.0e-30)
  df1$residuals.smoke <- residuals(m3s)
  moran.mc(df1$residuals.smoke, lw, 999) # p value of  0.898 sucessfully removes spatial autocorrelation
  
  
  coordinates(df1) <- ~ lon + lat
  
  # Use K nearest neighbor to create a nearest neighbor list
  resnb.df1 <- sapply(neib.df1, function(x) mean(df1$residuals.hist[x]))
  resnb.df2 <- sapply(neib.df1, function(x) mean(df1$residuals.weight[x]))
  resnb.df3 <- sapply(neib.df1, function(x) mean(df1$residuals,smoke[x]))
  
  plot(df1$residuals.hist, resnb.df1, xlab='Residuals History and Concentration', ylab='Mean adjacent residuals',col='blue')
  plot(df1$residuals.weight, resnb.df1, xlab='Residuals Weight and Concentration', ylab='Mean adjacent residuals',col='red')
  plot(df1$residuals.smoke, resnb.df1, xlab='Residuals Smoking and Concentration', ylab='Mean adjacent residuals',col='green')
  