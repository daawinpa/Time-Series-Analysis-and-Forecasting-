##############################################################################
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)


###################################################################################
#                        PRECIPITATION
#################################################################################

brookvilleWeatherRecord <- read.csv("C:/Users/Palma/Desktop/MS STATS/Year2 Sem1/STA 583 Time Series/Project/brookvilleWeatherRecord.csv")

brookWeather<-brookvilleWeatherRecord
brookWeather <- as.data.frame(brookWeather)
brookWeather$PRCP <-ifelse(brookWeather$PRCP<=-9999,NA,brookWeather$PRCP)
brookWeather$DATE<-ymd(brookWeather$DATE)
brookWeather$DATE<-as.POSIXct(brookWeather$DATE,format = "%Y-%m-%d")
brookWeather$DATE <- format.POSIXct(brookWeather$DATE,'%Y-%m-%d')
ts <- seq.POSIXt(as.POSIXct("1925-05-01",'%Y-%m-%d'), as.POSIXct("2016-07-31",'%Y-%m-%d'), by="d")
ts <- format.POSIXct(ts,'%Y-%m-%d')
df <- data.frame(timestamp=ts)
brookWeather<- full_join(df,brookWeather, by=c("timestamp" = "DATE"))
brookWeather$month <- month(brookWeather$timestamp)
brookWeather$year <- year(brookWeather$timestamp)

#plot(brookWeather$PRCP,type='l')

brookWeather.monthly<-aggregate( brookWeather$PRCP ~ brookWeather$month + brookWeather$year , brookWeather, sum, na.rm=TRUE, na.action=na.pass )

View(brookWeather.monthly)
summary(brookWeather.monthly)
length(brookWeather.monthly)
dim(brookWeather.monthly)

month <-brookWeather.monthly$`brookWeather$month`   
MPRCP <-brookWeather.monthly$`brookWeather$PRCP`
year <-brookWeather.monthly$`brookWeather$year` 

MPRCP <- ts(MPRCP, start=1925.33  ,frequency =12 )

plot(MPRCP ,
   main=" Monthly Total Precipitation",xlab="Time", ylab="Precipitation(Inches)",col="blue", type='l')

MPRCP.loess75 <- loess(MPRCP~time(MPRCP), span =0.75 ,data=MPRCP)

plot(MPRCP, main="LOESS Plot of Monthly Total Precipitation", ylab="Precipitation(Inches)",type='o', pch=20, col="blue")
lines(as.vector(time(MPRCP)), fitted(MPRCP.loess75 ), lwd=3, col="red")

decompMPRCP<-decompose(MPRCP)
plot(decompMPRCP)

#######################################################################################
#   MAXIMUM TEMPERATURE
###################################################################################

brookWeather$TMAX <-ifelse(brookWeather$TMAX<=-9999,NA,brookWeather$TMAX)
brookWeather.monthly<-aggregate( brookWeather$TMAX ~ brookWeather$month + brookWeather$year , brookWeather, mean, na.rm=TRUE, na.action=na.pass )
month <-brookWeather.monthly$`brookWeather$month`   
MTMAX <-brookWeather.monthly$`brookWeather$TMAX`
year <-brookWeather.monthly$`brookWeather$year` 

MTMAX <- ts(MTMAX, start=1925.33 ,frequency =12 )

plot(MTMAX,
     main=" Monthly Average Maximum Temperature",
     xlab="Time", ylab="Degrees (Fahrenheit)",col="blue", type='l')

MTMAX.loess75 <- loess(MTMAX~time(MTMAX), span =0.75 ,data=MTMAX)
plot(MTMAX, main="LOESS of Monthly Average Maximum Temperature", ylab="Temperature(Degrees Fahrenheit)",type='o', pch=20, col="blue")
lines(as.vector(MTMAX.loess75$x), fitted(MTMAX.loess75), lwd=3, col="red")

decompMTMAX<-decompose(MTMAX)
plot(decompMTMAX)

##############################################################################
#                       MINIMUM TEMPERATURE
################################################################################


brookWeather$TMIN <-ifelse(brookWeather$TMIN<=-9999,NA,brookWeather$TMIN)
brookWeather.monthly<-aggregate( brookWeather$TMIN ~ brookWeather$month + brookWeather$year , brookWeather, mean ,na.rm=TRUE, na.action=na.pass)


month <-brookWeather.monthly$`brookWeather$month`   
MTMIN <-brookWeather.monthly$`brookWeather$TMIN`
year <-brookWeather.monthly$`brookWeather$year` 

MTMIN <- ts(MTMIN, start=1925.33 ,frequency =12 )

plot(MTMIN ,
     main=" Monthly Average Minimum Temperature",
     xlab="Time", ylab="Degrees (Fahrenheit)",col="blue", type='l')



MTMIN.loess75 <- loess(MTMIN~time(MTMIN), span =0.75 ,data=MTMIN)
plot(MTMIN, main="LOESS Plot of Monthly Average Minimum Temperature", ylab="Temperature(Degrees Fahrenheit)",type='o', pch=20, col="blue")
lines(as.vector(MTMIN.loess75$x), fitted(MTMIN.loess75 ), lwd=3, col="red")

decompMTMIN<-decompose(MTMIN)
plot(decompMTMIN)

#####################################################################################
#                      OBSERVED  TEMPERATURE
#######################################################################################

brookWeather$TOBS <-ifelse(brookWeather$TOBS<=-9999,NA,brookWeather$TOBS)

brookWeather.monthly<-aggregate( brookWeather$TOBS ~ brookWeather$month + brookWeather$year , brookWeather, mean,na.rm=TRUE, na.action=na.pass )


month <-brookWeather.monthly$`brookWeather$month`   
MTOBS <-brookWeather.monthly$`brookWeather$TOBS`
year <-brookWeather.monthly$`brookWeather$year` 

MTOBS <- ts(MTOBS, start=1925.33 ,frequency =12 )

plot(MTOBS ,
      main=" Monthly  Average Observed Temperature",
      xlab="Time", ylab="Degrees (Fahrenheit)",col="blue", type='l')



MTOBS.loess75 <- loess(MTOBS~time(MTOBS), span =0.75 ,data=MTOBS)
plot(MTOBS, main="LOESS Plot of Monthly Average Observed Temperature", ylab="Temperature(Degrees Fahrenheit)", xlab="Time",type='o', pch=20, col="blue")
lines(as.vector(MTOBS.loess75$x), fitted(MTOBS.loess75 ), lwd=3, col="red")

decompMTOBS<-decompose(MTOBS)
plot(decompMTOBS)

######################################################################################