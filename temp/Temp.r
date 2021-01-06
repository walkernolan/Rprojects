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


download.file(url="https://erddap.sccoos.org/erddap/tabledap/autoss.csv?station%2Ctime%2Ctemperature%2Ctemperature_flagPrimary%2Ctemperature_flagSecondary%2Cconductivity%2Cconductivity_flagPrimary%2Cconductivity_flagSecondary%2Cpressure%2Cpressure_flagPrimary%2Cpressure_flagSecondary%2Cchlorophyll%2Cchlorophyll_flagPrimary%2Cchlorophyll_flagSecondary%2Csalinity%2Csalinity_flagPrimary%2Csalinity_flagSecondary%2Csigmat%2CdiagnosticVoltage%2CcurrentDraw%2Caux1%2Caux3%2Caux4%2Clatitude%2Clongitude%2Cdepth%2CO2thermistor%2CconvertedOxygen", destfile="~/test.csv")
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