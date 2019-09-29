library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)

setwd("C:/Users/Veronica Tinney/Veronica Southerland Dropbox/EOH Modeling/Week 5/")

#Scenario 1

res=list()
res2=list()
sims = 30
for (i in 1:sims){
    times = 400
    N0 = 1000
    N1 = vector(length = times)
    N1[1] = N0
    N2 = vector(length=times)
    N2[1] = N0
    u1 <- rnorm(times - 1,mean=0.05,sd=0.01485)
    theta1=rnorm(times - 1,mean=0.00001,sd=0.00000296)
    w1 <- rnorm(times - 1,mean=-0.00004,sd=0.00003212)
    u2 <- rnorm(times - 1,mean=0.01,sd=0.00294)
    theta2 <- rnorm(times - 1,mean=-0.000025,sd=0.00000888)
    w2 <- rnorm(times - 1,mean=0.00003,sd=0.000007475)
    for (t in 2:times) {
      N1[t] = N1[t-1] + (u1[t-1]+(theta1[t-1]*N1[t-1])+(w1[t-1]*N2[t-1]))*N1[t-1]
      N2[t] = N2[t-1] + (u2[t-1]+(w2[t-1]*N1[t-1])+(theta2[t-1]*N2[t-1]))*N2[t-1]
    }
    res[[i]]=N1
    res2[[i]]=N2
  }


# Scenario 2
res3=list()
res4=list()
sims = 30
for (i in 1:sims){
  times = 400
  N0 = 1000
  N1 = vector(length = times)
  N1[1] = N0
  N2 = vector(length=times)
  N2[1] = N0

u1 <- rnorm(n=100,mean=0.05,sd=0.01485)
theta1 <- rnorm(n=100,mean=-0.00001,sd=0.00000296)
w1 <- rnorm(n=100,mean=-0.00011,sd=0.00003212)
u2 <- rnorm(n=100,mean=0.01,sd=0.00294)
theta2 <- rnorm(n=100,mean=-0.000025,sd=0.00000888)
w2 <- rnorm(n=100,mean=0.00003,sd=0.000007475)
for (t in 2:times) {
  N1[t] = N1[t-1] + (u1[t-1]+(theta1[t-1]*N1[t-1])+(w1[t-1]*N2[t-1]))*N1[t-1]
  N2[t] = N2[t-1] + (u2[t-1]+(w2[t-1]*N1[t-1])+(theta2[t-1]*N2[t-1]))*N2[t-1]
}
res3[[i]]=N1
res4[[i]]=N2
}

r1 <- data.frame(Reduce(rbind, res))
r2 <- data.frame(Reduce(rbind,res2))
r1 <-t(r1)
r2 <- t(r2)
r3 <- data.frame(Reduce(rbind, res3))
r3 <- t(r3)
r4 <- data.frame(Reduce(rbind, res4))
r4 <- t(r4)
full <- cbind(r1,r2,r3,r4)
write.csv(full, 'full.csv')

r1.m <- rowMeans(r1, na.rm = TRUE, dims = 1)
r2.m <- rowMeans(r2, na.rm=TRUE, dims=1)

par(mfrow=c(1,2))

matplot(1:times, cbind(r1.m,r2.m), type = "l", las = 1, ylab = "Population Size", 
        xlab = "Timestep",ylim=c(500,2000),main='Case 1')

r3.m <- rowMeans(r3, na.rm = TRUE, dims = 1)
r4.m <- rowMeans(r4, na.rm=TRUE, dims=1)

matplot(1:times, cbind(r3.m,r4.m), type = "l", las = 1, ylab = "Population Size", 
        xlab = "Timestep",ylim=c(0,1100),main='Case 2')


list.files()
mean <- read.csv('mean.csv')
colnames(mean)[1] <- 'Population'

x1 <- ggplot(mean,
       aes(x = time,
           y = mean)) +
  # Add a ribbon with the confidence band
  geom_smooth(
    aes(
      # lower and upper bound of the ribbon
      ymin = min, ymax = max,
      # Different colour for men/women
      fill = Population, colour = Population
    ),
    stat = "identity") +
  xlab("Timestep") +
  ylab("Mean population size")+
  labs(title='Scenario 1')

mean2 <- read.csv('mean2.csv')

x2 <- ggplot(mean2,
       aes(x = time,
           y = mean)) +
  # Add a ribbon with the confidence band
  geom_smooth(
    aes(
      # lower and upper bound of the ribbon
      ymin = min, ymax = max,
      # Different colour for men/women
      fill = Population, colour = Population
    ),
    stat = "identity") +
  xlab("Timestep") +
  ylab("Mean population size")+
  labs(title='Scenario 2')

plot <- plot_grid(x1,x2,ncol=1)
plot
ggsave('plot.png',width = 7,height=5)
