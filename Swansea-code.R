rm(list=ls())

#Installing required packages
install.packages(c("dplyr","openair","raster","ggplot2", "plotly", "worldmet", "tidyr", "expss"))  

#Loading required packages
library(dplyr)
library(openair)
library(raster)
library(ggplot2)
library(worldmet)
library(plotly)
library(tidyr)
library(expss)

##Break##

## Data collection -> Importing PM10 with met data, BC data of SM and SW (no SC data) 
# Import PM10 with met data of SWA1 (SM), SWAN (SW) from AURN 

SM_pollution_data   <- importAURN(site = "SWA1", year = 2010:2019, pollutant = "all")
CF_pollution_data   <- importAURN(site = "CARD", year = 2010:2019, pollutant = "all")

SM_pollution_data   <- SM_pollution_data [,c(1,2,6,7)]
CF_pollution_data   <- CF_pollution_data [,c(1,2,8,9)]


# Import met data of Cardiff meteorological station using worldmet package

getMeta(site = "Cardiff")
Sea_Met_data      <-  importNOAA(code = "037150-99999", year = 2010:2019)
Sea_Met_data      <-  Sea_Met_data[,c(1,9,10,13:16)]
Sea_Met_data      <-  subset(Sea_Met_data, date > as.Date("2011-03-31"))

# Save the PM, met data used for analyses as .csv files 

write.csv(SM_pollution_data,file = "SM_traffic_2010_2019_pollution_data.csv", row.names = F)
write.csv(CF_pollution_data,file = "CF_background_2010_2019_pollution_data.csv", row.names = F)
write.csv(Sea_Met_data, file = "Swansea_Meteorological_2010_2019_data.csv", row.names = F)

# Import tracer elements data from Defra and then traffic data (csv or excel files)

SM_Cu_Zn_Fe_2011_2019      <- read.csv("C:/Users/andhr/OneDrive/Documents/SWM metals data.csv")
SC_Cu_Zn_Fe_2011_2019      <- read.csv("C:/Users/andhr/OneDrive/Documents/SWC metals data.csv")

Sea_AADF_2011_2019         <- read.csv("C:/Users/andhr/OneDrive/Documents/Sea_aadf_2011_2019.csv")

##BreaK##

## Data processing -> Code to set right the columns and rows in above files to run analyses 

# SM_Cu_Zn_Fe_2011_2019 data

SM_Cu_Zn_Fe_2011_2019$Start.Date     <- as.POSIXct(strptime(SM_Cu_Zn_Fe_2011_2019$Start.Date, format = "%d/%m/%Y"), tz = "GMT")
names(SM_Cu_Zn_Fe_2011_2019)[names(SM_Cu_Zn_Fe_2011_2019)=="Start.Date"] <- "date"

i <- c(2:4)
SM_Cu_Zn_Fe_2011_2019[ , i] <- apply(SM_Cu_Zn_Fe_2011_2019[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(SM_Cu_Zn_Fe_2011_2019, class)

View(SM_Cu_Zn_Fe_2011_2019)
str(SM_Cu_Zn_Fe_2011_2019)

# SC_Cu_Zn_Fe_2011_2019 data

SC_Cu_Zn_Fe_2011_2019$date     <- as.POSIXct(strptime(SC_Cu_Zn_Fe_2011_2019$date, format = "%d/%m/%Y"), tz = "GMT")

i <- c(2:4)
SC_Cu_Zn_Fe_2011_2019[ , i]    <- apply(SC_Cu_Zn_Fe_2011_2019[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(SC_Cu_Zn_Fe_2011_2019, class)

View(SC_Cu_Zn_Fe_2011_2019)
str(SC_Cu_Zn_Fe_2011_2019)


# Sea_AADF_2011_2019 data

i <- c(1:8)
Sea_AADF_2011_2019[ , i]    <- apply(Sea_AADF_2011_2019[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(Sea_AADF_2011_2019, class)

View(Sea_AADF_2011_2019)
str(Sea_AADF_2011_2019)

##BreaK##

## Cleaning of data 

# SM_Cu_Zn_Fe_2011_2019      


timePlot(SM_Cu_Zn_Fe_2011_2019, pollutant = "Zn", name.pol = "Tyre wear tracer")
timePlot(SM_Cu_Zn_Fe_2011_2019, pollutant = "Cu", name.pol = "Brake wear tracer")
timePlot(SM_Cu_Zn_Fe_2011_2019, pollutant = "Fe", name.pol = "Resuspension tracer")

SM_Cu_Zn_Fe_2011_2019_partition  <- SM_Cu_Zn_Fe_2011_2019 %>% select(Cu,Zn,Fe)

SM <- ggplot(stack(SM_Cu_Zn_Fe_2011_2019_partition), aes(x= ind, y= values, fill = ind)) + xlab("Swansea Roadside 2011-19 Defra") + ylab("Brake wear (Cu), Tyre wear (ZN) and Resuspension (FE) tracers in ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Non-exhaust tracers") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Swansea Roadside') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
SM <- ggplotly(SM)
SM

# SC_Cu_Zn_Fe_2011_2019      

timePlot(SC_Cu_Zn_Fe_2011_2019, pollutant = "Zn", name.pol = "Tyre wear tracer")
timePlot(SC_Cu_Zn_Fe_2011_2019, pollutant = "Cu", name.pol = "Brake wear tracer")
timePlot(SC_Cu_Zn_Fe_2011_2019, pollutant = "Fe", name.pol = "Resuspension tracer")

SC_Cu_Zn_Fe_2011_2019_partition  <- SC_Cu_Zn_Fe_2011_2019 %>% select(Cu,Zn,Fe)

SC <- ggplot(stack(SC_Cu_Zn_Fe_2011_2019_partition), aes(x= ind, y= values, fill = ind)) + xlab("Swansea background 2010-19 Defra") + ylab("Brake wear (Cu), Tyre wear (ZN) and Resuspension (FE) tracers in ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Non-exhaust tracers") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Swansea background') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
SC <- ggplotly(SC)
SC

# Sea_AADF_2011_2019             

timePlot(Sea_AADF_2011_2019, pollutant = "pedal_cycles")
timePlot(Sea_AADF_2011_2019, pollutant = "all_motor_vehicles", name.pol = "AADF of Swansea Roadside")

Sea_AADF_2011_2019_motor_partition  <- Sea_AADF_2011_2019 %>% select(all_motor_vehicles, two_wheeled_motor_vehicles, cars_and_taxis, buses_and_coaches, lgvs, all_hgvs)
Sea_AADF_2011_2019_partition        <- Sea_AADF_2011_2019 %>% select(pedal_cycles, all_motor_vehicles)

ST <- ggplot(stack(Sea_AADF_2011_2019_motor_partition), aes(x= ind, y= values, fill = ind)) + xlab("AADF from DfT") + ylab("Motor vehicle counts") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Type") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Motor vehicles AADF of Swansea roadside') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
ST <- ggplotly(ST)
ST

STB <- ggplot(stack(Sea_AADF_2011_2019_partition), aes(x= ind, y= values, fill = ind)) + xlab("AADF from DfT") + ylab("Motor vehicles and pedal cycles") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Count") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Motor vehicles and pedal cycles AADF of Swansea roadside') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
STB <- ggplotly(STB)
STB

# PM and Meteorological data

timePlot(SM_pollution_data, pollutant = c("nox","pm10"), avg.time = "month")
timePlot(CF_pollution_data, pollutant = c("nox","pm10"), avg.time = "month")

##Main_Analysis##

## Part 1 of Methodology 

SM_Cu_Zn_Fe_2011_2019$Cu    <- SM_Cu_Zn_Fe_2011_2019$Cu*27.37
SM_Cu_Zn_Fe_2011_2019$Zn    <- SM_Cu_Zn_Fe_2011_2019$Zn*50
SM_Cu_Zn_Fe_2011_2019$Fe    <- SM_Cu_Zn_Fe_2011_2019$Fe*4


SC_Cu_Zn_Fe_2011_2019$Cu    <- SC_Cu_Zn_Fe_2011_2019$Cu*27.37
SC_Cu_Zn_Fe_2011_2019$Zn    <- SC_Cu_Zn_Fe_2011_2019$Zn*50
SC_Cu_Zn_Fe_2011_2019$Fe    <- SC_Cu_Zn_Fe_2011_2019$Fe*4

SM_inc_2011_2019_NExhaust   <- inner_join(SM_Cu_Zn_Fe_2011_2019, SC_Cu_Zn_Fe_2011_2019, by = "date")

colnames(SM_inc_2011_2019_NExhaust) <- c("date", "Brake_wear_SM",  "Resuspension_SM", "Tyre_wear_SM","Brake_wear_SC", "Resuspension_SC", "Tyre_wear_SC")

SM_inc_2011_2019_NExhaust$Brake_wear_SM   <- SM_inc_2011_2019_NExhaust$Brake_wear_SM - SM_inc_2011_2019_NExhaust$Brake_wear_SC
SM_inc_2011_2019_NExhaust$Brake_wear_SM         <- SM_inc_2011_2019_NExhaust$Brake_wear_SM/1000
TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Brake_wear_SM", ylab = "Brake wear concentration in ug/m3", main = "Brake wear concentration on SW", deseason = TRUE) #SM and SW are same#

SM_inc_2011_2019_NExhaust$Tyre_wear_SM    <- SM_inc_2011_2019_NExhaust$Tyre_wear_SM - SM_inc_2011_2019_NExhaust$Tyre_wear_SC
SM_inc_2011_2019_NExhaust$Tyre_wear_SM          <- SM_inc_2011_2019_NExhaust$Tyre_wear_SM/1000
TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Tyre_wear_SM", ylab = "Tyre wear concentration in ug/m3", main = "Tyre wear concentration on SW", deseason = TRUE)

SM_inc_2011_2019_NExhaust$Resuspension_SM <- SM_inc_2011_2019_NExhaust$Resuspension_SM - SM_inc_2011_2019_NExhaust$Resuspension_SC
SM_inc_2011_2019_NExhaust$Resuspension_SM       <- SM_inc_2011_2019_NExhaust$Resuspension_SM/1000
TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Resuspension_SM", ylab = "Resuspension concentration in ug/m3", main = "Resuspension dust concentration on SW", deseason = TRUE)

SM_inc_2011_2019_NExhaust$Non_exhaust_PM <- SM_inc_2011_2019_NExhaust$Brake_wear_SM + SM_inc_2011_2019_NExhaust$Tyre_wear_SM + SM_inc_2011_2019_NExhaust$Resuspension_SM


SM_inc_2011_2019_NExhaust                       <- select(SM_inc_2011_2019_NExhaust, c(1,2,3,4,8))
write.csv(SM_inc_2011_2019_NExhaust, file = "SM_inc_Non_Exhaust_final.csv", row.names = F)

SM_inc_2011_2019_NExhaust                       <- read.csv("C:/Users/andhr/OneDrive/Documents/SM_inc_Non_Exhaust_final.csv", stringsAsFactors = FALSE)

SM_inc_2011_2019_NExhaust$date                  <- as.POSIXct(strptime(SM_inc_2011_2019_NExhaust$date, format = "%d/%m/%Y"), tz = "GMT")

SM_inc_2011_2019_NExhaust_partition             <- select(SM_inc_2011_2019_NExhaust, c(2,3,4,5))

SM1 <- ggplot(stack(SM_inc_2011_2019_NExhaust_partition), aes(x= ind, y= values, fill = ind)) + xlab("Roadside incremental values") + ylab("Brake, Tyre and Resuspension wear in ug/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Tracers")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Incremental non-exhaust components of SM') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
SM1 <- ggplotly(SM1)
SM1

TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Non_exhaust_component", ylab = "NE component in ug/m3", main = "Non-exhaust component of SM", deseason = TRUE)

##repeat after removing outlier and negatives##

TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Brake_wear_SM", ylab = "Brake wear concentration in ug/m3", main = "Brake wear concentration on SW", deseason = TRUE) #SM and SW are same#

TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Tyre_wear_SM", ylab = "Tyre wear concentration in ug/m3", main = "Tyre wear concentration on SW", deseason = TRUE) #SM and SW are same#

TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Resuspension_SM", ylab = "Resuspension concentration in ug/m3", main = "Resuspension dust concentration on SW", deseason = TRUE) #SM and SW are same#

TheilSen(SM_inc_2011_2019_NExhaust, pollutant = "Non_exhaust_PM", ylab = "Non-exhaust PM in ug/m3", main = "Non-exhaust PM on SW", deseason = TRUE) #SM and SW are same#


##Pollution data merging## 

SM_inc_pollution_data           <- inner_join(SM_pollution_data, CF_pollution_data, by = "date")
SM_inc_pollution_data           <- select(SM_inc_pollution_data, c(1,3,4,6,7))
colnames(SM_inc_pollution_data) <- c("date", "SM_NOx", "SM_PM10", "CF_PM10", "CF_NOx")
SM_inc_pollution_data           <- na.omit(SM_inc_pollution_data)
SM_inc_pollution_data           <- timeAverage(SM_inc_pollution_data, avg.time = "month")

# Traffic analysis


TheilSen(Sea_AADF_2011_2019, pollutant = "all_motor_vehicles", ylab = "Annual average daily traffic", main = "Annual average daily traffic on SM")
TheilSen(Sea_AADF_2011_2019, pollutant = "lgvs", ylab = "Light good vehicles", main = "Annual average daily flow of LGVs on SM")
TheilSen(Sea_AADF_2011_2019, pollutant = "buses_and_coaches", ylab = "Buses and coaches", main = "Annual average daily flow of Buses and Coaches on SM")
TheilSen(Sea_AADF_2011_2019, pollutant = "cars_and_taxis", ylab = "Cars and Taxis", main = "Annual average daily flow of Cars and Taxis on SM")

