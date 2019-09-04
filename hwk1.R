library(ggplot2)
library(dplyr)
library(gifski)
library(gganimate)
library(purrr)
library(animation)
library(gridExtra)

setwd('C:/Users/Veronica Tinney/Veronica Southerland Dropbox/EOH Modeling/')
list.files()

df1 <- read.csv("giovanni.csv")
df2 <- read.csv("epa.aqi.csv")

df2$"Parameter.Code" <- as.numeric(as.character(df2$"Parameter.Code"))
df2 <- subset(df2, Parameter.Code == 42602)
df2 <- subset(df2, Pollutant.Standard == "NO2 1-hour")


df1$time <- seq(as.Date("2018-1-1"), as.Date("2018-12-31"), by = "1 day")
df2$Day.In.Year..Local. <- seq(as.Date("2018-1-1"), as.Date("2018-12-31"), by = "1 day")

df <- cbind(df1, df2)

#Arithmetic.Mean
df$trop <- df$mean_OMNO2d_003_ColumnAmountNO2TropCloudScreened
df$epa <- df$Arithmetic.Mean
df$dd <- df$time
df$Month <- format(df$dd,"%B")

p <- ggplot(data = df,
            mapping = aes(dd, trop,
                          color = `Month`,
                          Month)) +
  #geom_line(size = 1, color = "gray") +
  geom_point(aes(group=seq_along(dd))) +
  theme_minimal(base_family = "Carlito",
                base_size = 16)+
  ylim(-8e+15,1e+16)+
  # labels for horizontal lines
  labs(title='Nitrogen dioxide Tropospheric Column', 
       y='1/cm^2',x='Date',
       caption='Source: NASA Giovanni. 1/1/2018-12/31/2018.')+
  theme(plot.title=element_text(hjust = 0, size=12),
        axis.title=element_text(size=10)) +
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')

z <- ggplot(data = df,
            mapping = aes(dd, epa,
                          color = `Month`,
                          Month)) +
  #geom_line(size = 1, color = "gray") +
  geom_point(aes(group=seq_along(dd))) +
  theme_minimal(base_family = "Carlito",
                base_size = 16)+
  ylim(0,40)+
  # labels for horizontal lines
  labs(title='Nitrogen dioxide mean daily concentration', 
       y='ppb',x='Date',
       caption='EPA AQI daily average concentration. 1/1/2018-12/31/2018.')+
  theme(plot.title=element_text(hjust = 0, size=12),
        axis.title=element_text(size=10)) +
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')

grid.arrange(p, z, nrow = 2)

write.csv(df, 'df.csv')

oct <- read.csv('oct.csv')

p <- ggplot(data = oct,
            mapping = aes(dd, trop,
                          color = `Month`,
                          Month)) +
  geom_line(size = 1, color = "gray") +
  geom_point(aes(group=seq_along(dd))) +
  theme_minimal(base_family = "Carlito",
                base_size = 16)+
  ylim(-8e+15,1e+16)+
  # labels for horizontal lines
  labs(title='Nitrogen dioxide Tropospheric Column', 
       y='1/cm^2',x='Date',
       caption='Source: NASA Giovanni. 10/1/2018-10/31/2018.')+
  theme(plot.title=element_text(hjust = 0, size=12),
        axis.title=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1,size=7)) +
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')

z <- ggplot(data = oct,
            mapping = aes(dd, epa,
                          color = `Month`,
                          Month)) +
  geom_line(size = 1, color = "gray") +
  geom_point(aes(group=seq_along(dd))) +
  theme_minimal(base_family = "Carlito",
                base_size = 16)+
  ylim(0,40)+
  # labels for horizontal lines
  labs(title='Nitrogen dioxide mean daily concentration', 
       y='ppb',x='Date',
       caption='EPA AQI daily average concentration. 10/1/2018-10/31/2018.')+
  theme(plot.title=element_text(hjust = 0, size=12),
        axis.title=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1,size=7)) +
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')

grid.arrange(p, z, nrow = 2)
