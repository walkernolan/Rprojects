library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)

# function(input)
# input %>%
#   group_by(time, station) %>%
#   select(temperature) %>%
#   summarise(
#     temp = mean(temperature, na.rm = TRUE)
#   )


download.file(url="https://drive.google.com/file/d/1j9tAGZABsooceGiq6h9D3VBG9dlBFZqm/view?usp=sharing", destfile="~/test.csv")
trend_data <- read.csv(file="~/test.csv")
trend_data <- trend_data[-1,]
df <- trend_data[,c("time", "station", "temperature")]
#dft <-as.Date(df[,c("time")])
df[,c("time")]<-as.Date(df[,c("time")])
df[,c("temperature")]<-as.double(df[,c("temperature")])
df[is.na(df)] <- 0

df2 <- df %>%
  group_by(time, station) %>%
  select(temperature) %>%
  summarise(
    temp = mean(temperature, na.rm = TRUE)
  )

df2 <- data.frame(df2)


Q <- quantile(df2$temp, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df2$temp)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
df2<- df2[!(df2$temp < (Q[1] - 1.5*iqr) | df2$temp > (Q[2]+1.5*iqr)),]

ggplot(df2, aes(time, temp, colour = station))+geom_point()

#df2 <- df2[!(df2$temp > 100 | df2$temp < 1),]
#dfw <- pivot_wider(df2, names_from = station, values_from = temp)
#dfw[is.na(dfw)] <- 0