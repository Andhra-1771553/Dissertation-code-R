rm(list=ls())

#Installing required packages
install.packages(c("dplyr","openair","raster","ggplot2", "plotly", "worldmet", "tidyr", "expss","mcr"))  

#Loading required packages
library(dplyr)
library(openair)
library(raster)
library(ggplot2)
library(worldmet)
library(plotly)
library(tidyr)
library(expss)
library(mcr)

##Original statistical analysis##

## Data collection -> Importing PM10 with met data, tracer elements in PM10 fraction data from ClearfLo (MB and NK) and Defra(MB and WM) + [urban traffic aadf and hourly traffic counts of MB]
# Import PM10 with met data of my1, wm0 from KCL database as it comes with surface measurements and we just need PM10 - https://davidcarslaw.github.io/openair/reference/importKCL.html

MB_pollution_data   <- importKCL(site = "my1", year = 2010:2019, pollutant = "all")
WM_pollution_data   <- importKCL(site = "wm0", year = 2010:2019, pollutant = "all") 

MB_pollution_data   <- MB_pollution_data [,c(1,2,8)]
WM_pollution_data   <- WM_pollution_data [,c(1,2,7)]


# Import met data of Heathrow meteorological station using worldmet package

getMeta(site = "heathrow")
Met_data      <- importNOAA(code = "037720-99999", year = 2010:2019)
Met_data      <- Met_data[,c(1,9,10,13:16)]
Met_data      <- subset(Met_data, date > as.Date("2011-03-31"))

# Save the PM and met data used for analyses as .csv files 

write.csv(MB_pollution_data,file = "MB_traffic_2010_2019_pollution_data.csv", row.names = F)
write.csv(WM_pollution_data,file = "WM_traffic_2010_2019_pollution_data.csv", row.names = F)
write.csv(Met_data, file = "Meteorological_2010_2019_data.csv", row.names = F)

# Import tracer elements data from campaign and Defra and then traffic data (csv or excel files)

MB_ClearfLo_2010_12        <- read.csv("C:/Users/andhr/OneDrive/Documents/Marylebone Road_processed_ClearfLoPartisol_2010-2012.csv", stringsAsFactors = FALSE)
NK_ClearfLo_2010_12        <- read.csv("C:/Users/andhr/OneDrive/Documents/KC1_processed_2010-2013.csv", stringsAsFactors = FALSE)
MB_Cu_Zn_Fe_2010_2019      <- read.csv("C:/Users/andhr/OneDrive/Documents/MB - Verified Cu Zn Fe Defra data.csv")
WM_Cu_Zn_Fe_2010_2019      <- read.csv("C:/Users/andhr/OneDrive/Documents/WM - Verified Cu Zn Fe Defra data.csv")

AADF_2010_2019             <- read.csv("C:/Users/andhr/OneDrive/Documents/MY_aadf_2010_2019.csv")
TfL_Traffic_data           <- read.csv("C:/Users/andhr/OneDrive/Documents/0818-1920-TfL-Traffic Data.csv")

##Break_one##

## Data processing -> Code to set right the columns and rows in above files to run analyses 

# MB_ClearfLo_2010_12 data

MB_ClearfLo_2010_12$Date.Time  <- as.POSIXct(strptime(MB_ClearfLo_2010_12$Date.Time, format = "%d/%m/%Y %H:%M"), tz = "GMT")
names(MB_ClearfLo_2010_12)[names(MB_ClearfLo_2010_12)=="Date.Time"] <- "date"

i <- c(2:20)
MB_ClearfLo_2010_12[ , i] <- apply(MB_ClearfLo_2010_12[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(MB_ClearfLo_2010_12, class)

MB_ClearfLo_2010_12_req        <- MB_ClearfLo_2010_12 %>% select(date,CU,ZN,FE) # select dataframe columns
View(MB_ClearfLo_2010_12_req)
str(MB_ClearfLo_2010_12_req)


# NK_ClearfLo_2010_12 data

NK_ClearfLo_2010_12$Date.Time  <- as.POSIXct(strptime(NK_ClearfLo_2010_12$Date.Time, format = "%d/%m/%Y %H:%M"), tz = "GMT")
names(NK_ClearfLo_2010_12)[names(NK_ClearfLo_2010_12)=="Date.Time"] <- "date"

i <- c(2:23)
NK_ClearfLo_2010_12[ , i] <- apply(NK_ClearfLo_2010_12[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(NK_ClearfLo_2010_12, class)

NK_ClearfLo_2010_12_req        <- NK_ClearfLo_2010_12 %>% select(date,CU,ZN,FE) # select dataframe columns
View(NK_ClearfLo_2010_12_req)
str(NK_ClearfLo_2010_12_req)

# MB_Cu_Zn_Fe_2010_2019 data

MB_Cu_Zn_Fe_2010_2019$Date     <- as.POSIXct(strptime(MB_Cu_Zn_Fe_2010_2019$Date, format = "%d/%m/%Y"), tz = "GMT")
names(MB_Cu_Zn_Fe_2010_2019)[names(MB_Cu_Zn_Fe_2010_2019)=="Date"] <- "date"

i <- c(2:4)
MB_Cu_Zn_Fe_2010_2019[ , i] <- apply(MB_Cu_Zn_Fe_2010_2019[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(MB_Cu_Zn_Fe_2010_2019, class)

View(MB_Cu_Zn_Fe_2010_2019)
str(MB_Cu_Zn_Fe_2010_2019)

# WM_Cu_Zn_Fe_2010_2019 data

WM_Cu_Zn_Fe_2010_2019$End.Date     <- as.POSIXct(strptime(WM_Cu_Zn_Fe_2010_2019$End.Date, format = "%d/%m/%Y"), tz = "GMT")
names(WM_Cu_Zn_Fe_2010_2019)[names(WM_Cu_Zn_Fe_2010_2019)=="End.Date"] <- "date"

i <- c(2:4)
WM_Cu_Zn_Fe_2010_2019[ , i] <- apply(WM_Cu_Zn_Fe_2010_2019[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(WM_Cu_Zn_Fe_2010_2019, class)

View(WM_Cu_Zn_Fe_2010_2019)
str(WM_Cu_Zn_Fe_2010_2019)

# AADF_2010_2019 data

AADF_2011_2019_MB              <- AADF_2010_2019[,c(1:8)]

i <- c(1:8)
AADF_2011_2019_MB[ , i] <- apply(AADF_2011_2019_MB[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(AADF_2011_2019_MB, class)

View(AADF_2011_2019_MB)

# TfL_Traffic_data

TfL_Traffic_data$Date     <- paste(TfL_Traffic_data$Date, TfL_Traffic_data$Starthour)
TfL_Traffic_data          <- TfL_Traffic_data[,c(1,3,5,8,9)]


TfL_Traffic_data$Date     <- as.POSIXct(strptime(TfL_Traffic_data$Date, format = "%d/%m/%Y %H"), tz = "GMT")
names(TfL_Traffic_data)[names(TfL_Traffic_data)=="Date"] <- "date"


TfL_Traffic_data$Total.Volume <- as.numeric(as.character(TfL_Traffic_data$Total.Volume))
sapply(TfL_Traffic_data, class)

View(TfL_Traffic_data)
str(TfL_Traffic_data)

##Break_two##

## Cleaning of data 

# MB_ClearfLo_2010_12

timePlot(MB_ClearfLo_2010_12_req, pollutant = "ZN", name.pol = "Tyre wear tracer")
timePlot(MB_ClearfLo_2010_12_req, pollutant = "CU", name.pol = "Brake wear tracer")
timePlot(MB_ClearfLo_2010_12_req, pollutant = "FE", name.pol = "Resuspension tracer")

MB_ClearfLo_2010_12_partition  <- MB_ClearfLo_2010_12_req %>% select(CU,ZN,FE)

A <- ggplot(stack(MB_ClearfLo_2010_12_partition), aes(x= ind, y= values, fill = ind)) + xlab("MB 2010-12 ClearfLo") + ylab("Brake wear (Cu), Tyre wear (Zn) and Resuspension (Fe) tracers in ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Non-exhaust tracers") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'MB ClearfLo') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A <- ggplotly(A)
A

# NK_ClearfLo_2010_12        


timePlot(NK_ClearfLo_2010_12_req, pollutant = "ZN", name.pol = "Tyre wear tracer")
timePlot(NK_ClearfLo_2010_12_req, pollutant = "CU", name.pol = "Brake wear tracer")
timePlot(NK_ClearfLo_2010_12_req, pollutant = "FE", name.pol = "Resuspension tracer")

NK_ClearfLo_2010_12_partition  <- NK_ClearfLo_2010_12_req %>% select(CU,ZN,FE)

B <- ggplot(stack(NK_ClearfLo_2010_12_partition), aes(x= ind, y= values, fill = ind)) + xlab("NK 2010-12 ClearfLo") + ylab("Brake wear (Cu), Tyre wear (Zn) and Resuspension (Fe) tracers in ng/m3") + stat_boxplot(geom="errorbar", width = 0.25)  + geom_boxplot() + labs(fill ="Non-exhaust tracers") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'NK ClearfLo') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
B <- ggplotly(B)
B


# MB_Cu_Zn_Fe_2010_2019      


timePlot(MB_Cu_Zn_Fe_2010_2019, pollutant = "Zn", name.pol = "Tyre wear tracer")
timePlot(MB_Cu_Zn_Fe_2010_2019, pollutant = "Cu", name.pol = "Brake wear tracer")
timePlot(MB_Cu_Zn_Fe_2010_2019, pollutant = "Fe", name.pol = "Resuspension tracer")

MB_Cu_Zn_Fe_2010_2019_partition  <- MB_Cu_Zn_Fe_2010_2019 %>% select(Cu,Zn,Fe)

C <- ggplot(stack(MB_Cu_Zn_Fe_2010_2019_partition), aes(x= ind, y= values, fill = ind)) + xlab("MB 2010-19 Defra") + ylab("Brake wear (Cu), Tyre wear (ZN) and Resuspension (FE) tracers in ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Non-exhaust tracers") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'MB Defra') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
C <- ggplotly(C)
C

# WM_Cu_Zn_Fe_2010_2019      

timePlot(WM_Cu_Zn_Fe_2010_2019, pollutant = "Zn", name.pol = "Tyre wear tracer")
timePlot(WM_Cu_Zn_Fe_2010_2019, pollutant = "Cu", name.pol = "Brake wear tracer")
timePlot(WM_Cu_Zn_Fe_2010_2019, pollutant = "Fe", name.pol = "Resuspension tracer")

WM_Cu_Zn_Fe_2010_2019_partition  <- WM_Cu_Zn_Fe_2010_2019 %>% select(Cu,Zn,Fe)

D <- ggplot(stack(WM_Cu_Zn_Fe_2010_2019_partition), aes(x= ind, y= values, fill = ind)) + xlab("WM 2010-19 Defra") + ylab("Brake wear (Cu), Tyre wear (ZN) and Resuspension (FE) tracers in ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Non-exhaust tracers") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'WM Defra') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
D <- ggplotly(D)
D

# TfL_Traffic_data           

TfL_Traffic_data_East  <- subset(TfL_Traffic_data, DIRECN2 %in% c("East"))
TfL_Traffic_data_West  <- subset(TfL_Traffic_data, DIRECN2 %in% c("West"))

timePlot(TfL_Traffic_data_East, pollutant = "Total.Volume", avg.time = "day", name.pol = "Marylebone Road")
timePlot(TfL_Traffic_data_West, pollutant = "Total.Volume", avg.time = "day", name.pol = "Marylebone Road")

TfL_Traffic_data_partition_East  <- TfL_Traffic_data_East %>% select(Total.Volume)
TfL_Traffic_data_partition_West  <- TfL_Traffic_data_West %>% select(Total.Volume)


TC <- ggplot(stack(TfL_Traffic_data_partition_East), aes(x= ind, y= values, fill = ind)) + xlab("Traffic data - East") + ylab("Total volume") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Traffic") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'East direction traffic of MB') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
TC <- ggplotly(TC)
TC

TD <- ggplot(stack(TfL_Traffic_data_partition_West), aes(x= ind, y= values, fill = ind)) + xlab("Traffic data - West") + ylab("Total volume") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Traffic") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'West direction traffic of MB') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
TD <- ggplotly(TD)
TD

# PM and Meteorological data

MB_pollution_data$date  <- as.POSIXct(strptime(MB_pollution_data$date, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")
NK_pollution_data$date  <- as.POSIXct(strptime(NK_pollution_data$date, format = "%d/%m/%Y %H:%M"), tz = "GMT")

Met_data$date  <- as.POSIXct(strptime(Met_data$date, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")

polarPlot(MB_pollution_data, pollutant = "pm10")
polarPlot(NK_pollution_data, pollutant = "pm10")

timePlot(MB_pollution_data, pollutant = c("nox","pm10"), avg.time = "day")
timePlot(WM_pollution_data, pollutant = c("nox", "pm10"), avg.time = "day")

## Main_analysis ##

## First section of Methodology

# incMetals analysis of ClearfLo in 2010-12

inc_MB_ClearfLo_2010_12       <- merge(MB_ClearfLo_2010_12_req, NK_ClearfLo_2010_12_req, by = "date", all = TRUE)
inc_MB_ClearfLo_2010_12$incCu <- inc_MB_ClearfLo_2010_12$CU.x - inc_MB_ClearfLo_2010_12$CU.y
inc_MB_ClearfLo_2010_12$incFe <- inc_MB_ClearfLo_2010_12$FE.x - inc_MB_ClearfLo_2010_12$FE.y
inc_MB_ClearfLo_2010_12$incZn <- inc_MB_ClearfLo_2010_12$ZN.x - inc_MB_ClearfLo_2010_12$ZN.y
inc_MB_ClearfLo_2010_12       <- select(inc_MB_ClearfLo_2010_12, c(1,8,9,10))
inc_MB_ClearfLo_2010_12       <- inc_MB_ClearfLo_2010_12 %>% 
                                                        group_by(month = format(as.Date(date),'%m-%Y')) %>%
                                                        summarise_each(funs(if(length(na.omit(.))>=15)
                                                        mean(.,na.rm=TRUE) else NA_real_), 2:4)
inc_MB_ClearfLo_2010_12       <- inc_MB_ClearfLo_2010_12 %>% drop_na()
inc_MB_ClearfLo_2010_12       <- as.data.frame(inc_MB_ClearfLo_2010_12)


write.csv(inc_MB_ClearfLo_2010_12,file = "inc_MB_ClearfLo.csv", row.names = F)

#time format changed manually as R not working for some reason - processed file is inputted

inc_MB_ClearfLo_2010_12       <- read.csv("C:/Users/andhr/OneDrive/Documents/inc_MB_ClearfLo_processed.csv", stringsAsFactors = FALSE)
inc_MB_ClearfLo_2010_12$month <- as.POSIXct(strptime(inc_MB_ClearfLo_2010_12$month, format = "%d/%m/%Y"), tz = "GMT")
names(inc_MB_ClearfLo_2010_12)[names(inc_MB_ClearfLo_2010_12)=="month"] <- "date"

A1 <- ggplot(stack(inc_MB_ClearfLo_2010_12), aes(x= ind, y= values, fill = ind)) + xlab("incMB tracers") + ylab("ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Tracer metals") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'CleafLo traffic increment at MB') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A1

# incMetals analysis of Defra in 2010-12

MB_Defra_2010_12              <- subset(MB_Cu_Zn_Fe_2010_2019, date < as.Date("2013-01-16"))
WM_Defra_2010_12              <- subset(WM_Cu_Zn_Fe_2010_2019, date < as.Date("2013-01-17"))

inc_MB_Defra_2010_12          <- merge(MB_Defra_2010_12, WM_Defra_2010_12, by = "date", all = TRUE)
inc_MB_Defra_2010_12$incCu    <- inc_MB_Defra_2010_12$Cu.x - inc_MB_Defra_2010_12$Cu.y
inc_MB_Defra_2010_12$incFe    <- inc_MB_Defra_2010_12$Fe.x - inc_MB_Defra_2010_12$Fe.y
inc_MB_Defra_2010_12$incZn    <- inc_MB_Defra_2010_12$Zn.x - inc_MB_Defra_2010_12$Zn.y
inc_MB_Defra_2010_12          <- select(inc_MB_Defra_2010_12, c(1,8,9,10))

inc_MB_Defra_2010_12          <- inc_MB_Defra_2010_12 %>% drop_na()

write.csv(inc_MB_Defra_2010_12,file = "inc_MB_Defra.csv", row.names = F)

# Changed dates to first of month to match ClearfLo processed file 

inc_MB_Defra_2010_12          <- read.csv("C:/Users/andhr/OneDrive/Documents/inc_MB_Defra_processed.csv", stringsAsFactors = FALSE)
inc_MB_Defra_2010_12$date     <- as.POSIXct(strptime(inc_MB_Defra_2010_12$date, format = "%d/%m/%Y"), tz = "GMT")

A2    <- ggplot(stack(inc_MB_Defra_2010_12), aes(x= ind, y= values, fill = ind)) + xlab("incMB tracers") + ylab("ng/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + labs(fill ="Tracer metals") + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Defra traffic increment at MB') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A2

inc_MB_ClearfLo_2010_12              <- subset(inc_MB_ClearfLo_2010_12, date > as.Date("2011-03-31"))


A3    <- ggplot() +
          geom_line(data = inc_MB_ClearfLo_2010_12, aes(x = date, y = incCu, color = "blue")) +
          geom_line(data = inc_MB_Defra_2010_12, aes(x = date, y = incCu, color = "red")) +
          xlab('Date') +
          ylab('incCu in ng/m3')+ 
          scale_color_discrete("Campaign", labels = c("ClearfLo","Defra")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A3

A4    <- ggplot() +
          geom_line(data = inc_MB_ClearfLo_2010_12, aes(x = date, y = incFe, color = "blue")) +
          geom_line(data = inc_MB_Defra_2010_12, aes(x = date, y = incFe, color = "red")) +
          xlab('Date') +
          ylab('incFe in ng/m3')+ 
          scale_color_discrete("Campaign", labels = c("ClearfLo","Defra")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A4

A5    <- ggplot() +
          geom_line(data = inc_MB_ClearfLo_2010_12, aes(x = date, y = incZn, color = "blue")) +
          geom_line(data = inc_MB_Defra_2010_12, aes(x = date, y = incZn, color = "red")) +
          xlab('Date') +
          ylab('incZn in ng/m3')+ 
          scale_color_discrete("Campaign", labels = c("ClearfLo","Defra")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A5


MB_required_dataset <- inner_join(inc_MB_ClearfLo_2010_12, inc_MB_Defra_2010_12, by = "date")
View(MB_required_dataset)

MB_required_dataset <- apply_labels(MB_required_dataset, incCu.x = "ClearfLo_Cu", incFe.x = "ClearfLo_Fe", incZn.x = "ClearfLo_Zn", incCu.y = "Defra_Cu", incFe.y = "Defra_Fe", incZn.y = "Defra_Zn")

write.csv(MB_required_dataset, file = "MB_ClearfLo_Defra_inc_tracers.csv", row.names = F)

options(expss.digits = 2)
T1 <- MB_required_dataset %>% 
      tab_cells(incCu.x, incCu.y, incFe.x, incFe.y, incZn.x, incZn.y) %>% 
      tab_cols(total(label = "Traffic incremental tracer values")) %>%
      tab_stat_fun(Mean = w_mean, Max = w_max, Min = w_min, Median = w_median, SD = w_sd, N = w_n, method = list) %>% 
      tab_pivot() %>% 
      set_caption("Summary statistics of ClearfLo and Defra campaigns from 2010-12")
      htmlTable(T1)

MB_required_dataset_Cu  <- select(MB_required_dataset, -c(date,incFe.x, incFe.y, incZn.x, incZn.y))
colnames(MB_required_dataset_Cu) <- c("ClearfLo","Defra")

i <- c(1:2)    
MB_required_dataset_Cu[ , i] <- apply(MB_required_dataset_Cu[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(MB_required_dataset_Cu, class)    
  
A6 <- ggplot(stack(MB_required_dataset_Cu), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Brake wear tracer (Cu) in ng ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Campaign")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A6 <- ggplotly(A6)
A6

MB_required_dataset_Fe  <- select(MB_required_dataset, -c(date,incCu.x, incCu.y, incZn.x, incZn.y))
colnames(MB_required_dataset_Fe) <- c("ClearfLo","Defra")

i <- c(1:2)    
MB_required_dataset_Fe[ , i] <- apply(MB_required_dataset_Fe[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(MB_required_dataset_Fe, class)    

A7 <- ggplot(stack(MB_required_dataset_Fe), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Resuspension tracer (Fe) in ng ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Campaign")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A7 <- ggplotly(A7)
A7

MB_required_dataset_Zn  <- select(MB_required_dataset, -c(date,incFe.x, incFe.y, incCu.x, incCu.y))
colnames(MB_required_dataset_Zn) <- c("ClearfLo","Defra")

i <- c(1:2)    
MB_required_dataset_Zn[ , i] <- apply(MB_required_dataset_Zn[ , i], 2, function(x) as.numeric(as.character(x)))
sapply(MB_required_dataset_Zn, class)    

A8 <- ggplot(stack(MB_required_dataset_Zn), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Tyre wear tracer (Zn) in ng  ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Campaign")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
A8 <- ggplotly(A8)
A8

write.csv(MB_required_dataset,file = "MWT-one.csv", row.names = F)

MWT_Brakewear      <- read.csv("C:/Users/andhr/OneDrive/Documents/MWT_Brakewear.csv", header = TRUE)
colnames(MWT_Brakewear) <- c("Campaign", "Brakewear")
MWT_Tyrewear       <- read.csv("C:/Users/andhr/OneDrive/Documents/MWT_Tyrewear.csv")
MWT_Resuspension   <- read.csv("C:/Users/andhr/OneDrive/Documents/MWT_Resuspension.csv")

tapply(MWT_Brakewear$Brakewear, MWT_Brakewear$Campaign, mean, na.rm = T)
tapply(MWT_Tyrewear$Tyrewear, MWT_Tyrewear$Campaign, mean, na.rm = T)
tapply(MWT_Resuspension$Resuspension, MWT_Resuspension$Campaign, mean, na.rm = T)

wilcox.test(Brakewear ~ Campaign, mu = 0, alt = "two.sided", correct =TRUE, paired = FALSE, conf.int = TRUE, data = MWT_Brakewear)
wilcox.test(Tyrewear ~ Campaign, mu = 0, alt = "two.sided", correct =TRUE, paired = FALSE, conf.int = TRUE, data = MWT_Tyrewear)
wilcox.test(Resuspension ~ Campaign, mu = 0, alt = "two.sided", correct =TRUE, paired = FALSE, conf.int = TRUE, data = MWT_Resuspension)

#Deming charts 


x <- MB_required_dataset_Cu$ClearfLo
y <- MB_required_dataset_Cu$Defra

b1 <- mcreg(x,y,method.reg = "Deming", mref.name = "Defra", mtest.name = "ClearfLo", na.rm = TRUE)

plot(b1, Legend = TRUE, main =  "Deming regression of Cu from ClearfLo over Defra", Points.pch = 19, ci.area = TRUE , identity = FALSE, Grid = FALSE, Sub ="") 

MCResult.plot(b1, identity = TRUE, ci.area.col = grey(0.9), Legend = TRUE, legend.place = "topleft", add.grid = FALSE,ci.border = TRUE, points.pch = 20, main = "Cu in ng/m3", add.cor = FALSE, family="serif", sub = "")


x <- MB_required_dataset_Fe$ClearfLo
y <- MB_required_dataset_Fe$Defra

r1 <- mcreg(x,y,method.reg = "Deming", mref.name = "Defra", mtest.name = "ClearfLo", na.rm = TRUE)

plot(r1, Legend = TRUE, main =  "Deming regression of Fe from ClearfLo over Defra", Points.pch = 19, ci.area = FALSE , identity = FALSE, Grid = FALSE, Sub ="") 

MCResult.plot(r1, identity = TRUE, ci.area.col = grey(0.9), Legend = TRUE, legend.place = "bottomleft", add.grid = FALSE,ci.border = TRUE, points.pch = 20, main = "Fe in ng/m3", add.cor = FALSE, family="serif", sub = "")

x <- MB_required_dataset_Zn$ClearfLo
y <- MB_required_dataset_Zn$Defra

t1 <- mcreg(x,y,method.reg = "Deming", mref.name = "Defra", mtest.name = "ClearfLo", na.rm = TRUE)

plot(t1, Legend = TRUE, main =  "Deming regression of Zn from ClearfLo over Defra", Points.pch = 19, ci.area = TRUE , identity = FALSE, Grid = FALSE, Sub ="") 

MCResult.plot(b1, identity = TRUE, ci.area.col = grey(0.9), Legend = TRUE, legend.place = "topleft", add.grid = FALSE,ci.border = TRUE, points.pch = 20, main = "Zn in ng/m3", add.cor = FALSE, family="serif", sub = "")


## Next section of Methodology 

MB_Cu_Zn_Fe_2010_2019$Cu    <- MB_Cu_Zn_Fe_2010_2019$Cu*27.37
MB_Cu_Zn_Fe_2010_2019$Zn    <- MB_Cu_Zn_Fe_2010_2019$Zn*50
MB_Cu_Zn_Fe_2010_2019$Fe    <- MB_Cu_Zn_Fe_2010_2019$Fe*4


WM_Cu_Zn_Fe_2010_2019$Cu    <- WM_Cu_Zn_Fe_2010_2019$Cu*27.37
WM_Cu_Zn_Fe_2010_2019$Zn    <- WM_Cu_Zn_Fe_2010_2019$Zn*50
WM_Cu_Zn_Fe_2010_2019$Fe    <- WM_Cu_Zn_Fe_2010_2019$Fe*4

MB_inc_2010_2019_NExhaust   <- inner_join(MB_Cu_Zn_Fe_2010_2019, WM_Cu_Zn_Fe_2010_2019, by = "date")

colnames(MB_inc_2010_2019_NExhaust) <- c("date", "Brake_wear_MB", "Tyre_wear_MB", "Resuspension_MB","Brake_wear_WM", "Tyre_wear_WM", "Resuspension_WM" )

MB_inc_2010_2019_NExhaust$Brake_wear_MB     <- MB_inc_2010_2019_NExhaust$Brake_wear_MB - MB_inc_2010_2019_NExhaust$Brake_wear_WM
MB_inc_2010_2019_NExhaust$Brake_wear_MB     <- MB_inc_2010_2019_NExhaust$Brake_wear_MB/1000
TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Brake_wear_MB", ylab = "Brake wear in ug/m3", main = "Roadside brake wear concentration", deseason = TRUE)

MB_inc_2010_2019_NExhaust$Tyre_wear_MB      <- MB_inc_2010_2019_NExhaust$Tyre_wear_MB - MB_inc_2010_2019_NExhaust$Tyre_wear_WM
MB_inc_2010_2019_NExhaust$Tyre_wear_MB      <- MB_inc_2010_2019_NExhaust$Tyre_wear_MB/1000
TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Tyre_wear_MB", ylab = "Tyre wear in ug/m3", main = "Roadside tyre wear concentration", deseason = TRUE)

MB_inc_2010_2019_NExhaust$Resuspension_MB   <- MB_inc_2010_2019_NExhaust$Resuspension_MB - MB_inc_2010_2019_NExhaust$Resuspension_WM
MB_inc_2010_2019_NExhaust$Resuspension_MB   <- MB_inc_2010_2019_NExhaust$Resuspension_MB/1000
TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Resuspension_MB", ylab = "Resuspension concentration in ug/m3", main = "Road dust resuspension concentration", deseason = TRUE)

MB_inc_2010_2019_NExhaust$Non_exhaust_PM    <- MB_inc_2010_2019_NExhaust$Brake_wear_MB + MB_inc_2010_2019_NExhaust$Tyre_wear_MB + MB_inc_2010_2019_NExhaust$Resuspension_MB

MB_inc_2010_2019_NExhaust                       <- select(MB_inc_2010_2019_NExhaust, c(1,2,3,4,8))
write.csv(MB_inc_2010_2019_NExhaust, file = "MB_inc_Non_Exhaust_final.csv", row.names = F)
MB_inc_2010_2019_NExhaust                       <- read.csv("C:/Users/andhr/OneDrive/Documents/MB_inc_Non_Exhaust_final.csv", stringsAsFactors = FALSE)

MB_inc_2010_2019_NExhaust$date                  <- as.POSIXct(strptime(MB_inc_2010_2019_NExhaust$date, format = "%d/%m/%Y"), tz = "GMT")

MB_inc_2010_2019_NExhaust_partition             <- select(MB_inc_2010_2019_NExhaust, c(2,3,4,5))

MB1 <- ggplot(stack(MB_inc_2010_2019_NExhaust), aes(x= ind, y= values, fill = ind)) + xlab("Roadside incremental values") + ylab("Brake, Tyre and Resuspension wear in ug/m3") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Tracers")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + labs(title = 'Incremental non-exhaust components of MB') + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
MB1

#after removing outliers

TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Brake_wear_MB", ylab = "Brake wear in ug/m3", main = "Brake wear concentration on MB", deseason = TRUE)
TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Tyre_wear_MB", ylab = "Tyre wear in ug/m3", main = "Tyre wear concentration on MB", deseason = TRUE)
TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Resuspension_MB", ylab = "Resuspension concentration in ug/m3", main = "Resuspension dust concentration on MB", deseason = TRUE)
TheilSen(MB_inc_2010_2019_NExhaust, pollutant = "Non_exhaust_PM", ylab = "Non-exhaust PM in ug/m3", main = "Non-exhaust PM on MB", deseason = TRUE)

##first finish swansea TheilSen estimators##

##Swansea##

SwanMary              <- inner_join(MB_inc_2010_2019_NExhaust, SM_inc_2011_2019_NExhaust, by = "date")
names(SwanMary)[names(SwanMary)=="Non_exhaust_PM.x"] <- "NE_PM_MB"
names(SwanMary)[names(SwanMary)=="Non_exhaust_PM.y"] <- "NE_PM_SM"

##Summary statistics##

write.csv(SwanMary, file = "SwanMary_concentrations.csv", row.names = F)

options(epxpss.digits = 2)
MST1 <- SwanMary %>% 
          tab_cells(Brake_wear_MB, Brake_wear_SM, Resuspension_MB, Resuspension_SM, Tyre_wear_MB, Tyre_wear_SM, NE_PM_MB, NE_PM_SM) %>% 
          tab_cols(total(label = "Non-exhaust PM concentrations")) %>%
          tab_stat_fun(Mean = w_mean, Max = w_max, Min = w_min, Median = w_median, SD = w_sd, N = w_n, method = list) %>% 
          tab_pivot() %>% 
          set_caption("Summary statistics of Marylebone and Swansea from 2011-18")
        htmlTable(MST1)

##Mann whitney and box plots##
        
SwanMary_Cu           <- SwanMary[,c(1,2,6)]
SwanMary_Fe           <- SwanMary[,c(1,4,7)]
SwanMary_Zn           <- SwanMary[,c(1,3,8)]
SwanMary_NE           <- SwanMary[,c(1,5,9)]
colnames(SwanMary_NE) <- c("date", "NE_Mary", "NE_Swan")

SwanMary_Cu_partition <- SwanMary_Cu[,c(2,3)]
colnames(SwanMary_Cu_partition) <- c("Marylebone", "Swansea")

SwanMary_Fe_partition <- SwanMary_Fe[,c(2,3)]
colnames(SwanMary_Fe_partition) <- c("Marylebone", "Swansea")

SwanMary_Zn_partition <- SwanMary_Zn[,c(2,3)]
colnames(SwanMary_Zn_partition) <- c("Marylebone", "Swansea")

SwanMary_NE_partition <- SwanMary_NE[,c(2,3)]
colnames(SwanMary_NE_partition) <- c("Marylebone", "Swansea")

MS1 <- ggplot(stack(SwanMary_Cu_partition), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Brake wear concentration in ug ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Monitoring site")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
MS1 <- ggplotly(MS1)
MS1


MS2 <- ggplot(stack(SwanMary_Fe_partition), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Resuspended dust concentration in ug ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Monitoring site")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
MS2 <- ggplotly(MS2)
MS2


MS3 <- ggplot(stack(SwanMary_Zn_partition), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Tyre wear concentration in ug ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Monitoring site")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
MS3 <- ggplotly(MS3)
MS3


MS4 <- ggplot(stack(SwanMary_NE_partition), aes(x= ind, y= values, fill = ind)) + labs(y = expression(paste("Non-exhaust PM concentration in ug ", m^-3)), x = "Roadside increment") + stat_boxplot(geom="errorbar", width = 0.25) + geom_boxplot() + scale_fill_discrete(guide = guide_legend(title ="Monitoring site")) + theme(legend.title = element_text(color = "black", size = 16, face = "bold")) + theme(plot.title = element_text(size = 20, face = "bold")) + theme(text=element_text(size=16,  family="serif"))
MS4 <- ggplotly(MS4)
MS4

write.csv(SwanMary,file = "SMT_one.csv", row.names = F)

SMT_Brakewear      <- read.csv("C:/Users/andhr/OneDrive/Documents/SMT_Brakewear.csv", header = TRUE)
SMT_Tyrewear       <- read.csv("C:/Users/andhr/OneDrive/Documents/SMT_Tyrewear.csv")
SMT_Resuspension   <- read.csv("C:/Users/andhr/OneDrive/Documents/SMT_Resuspension.csv")
SMT_NonexhaustPM   <- read.csv("C:/Users/andhr/OneDrive/Documents/SMT_NonexhaustPM.csv")

tapply(SMT_Brakewear$Brakewear, SMT_Brakewear$Site, mean, na.rm = T)
tapply(SMT_Tyrewear$Tyrewear, SMT_Tyrewear$Site, mean, na.rm = T)
tapply(SMT_Resuspension$Resuspension, SMT_Resuspension$Site, mean, na.rm = T)
tapply(SMT_NonexhaustPM$NonexhaustPM, SMT_NonexhaustPM$Site, mean, na.rm = T)

wilcox.test(Brakewear ~ Site, mu = 0, alt = "two.sided", correct =TRUE, paired = FALSE, conf.int = TRUE, data = SMT_Brakewear)
wilcox.test(Tyrewear ~ Site, mu = 0, alt = "two.sided", correct =TRUE, paired = FALSE, conf.int = TRUE, data = SMT_Tyrewear)
wilcox.test(Resuspension ~ Site, mu = 0, alt = "two.sided", correct =TRUE, paired = FALSE, conf.int = TRUE, data = SMT_Resuspension)
wilcox.test(NonexhaustPM ~ Site, mu = 0, alt = "two.sided", correct = TRUE, paired = FALSE, conf.int = TRUE, data = SMT_NonexhaustPM)

##NEofPM and PM charts for (MB-WM from importKCL)##  

MB_inc_pollution_data           <- inner_join(MB_pollution_data, WM_pollution_data, by = "date")
colnames(MB_inc_pollution_data) <- c("date", "MB_NOx", "MB_PM10","WM_NOx" ,"WM_PM10")
MB_inc_pollution_data$inc_NOx   <- MB_inc_pollution_data$MB_NOx - MB_inc_pollution_data$WM_NOx
MB_inc_pollution_data$inc_PM10  <- MB_inc_pollution_data$MB_PM10 - MB_inc_pollution_data$WM_PM10
MB_inc_pollution_data           <- select(MB_inc_pollution_data, c(1,7,6))

MB_inc_pollution_data           <- timeAverage(MB_inc_pollution_data, avg.time = "month")

MB_inc_NE_Pollution_set         <- inner_join(MB_inc_2010_2019_NExhaust, MB_inc_pollution_data,by = "date")

MB_inc_NE_Pollution_set$NEofPM  <- (MB_inc_NE_Pollution_set$Non_exhaust_PM/MB_inc_NE_Pollution_set$inc_PM10)*100

MB_inc_NE_Pollution_set_annual  <- timeAverage(MB_inc_NE_Pollution_set, avg.time = "year")

TheilSen(MB_inc_NE_Pollution_set, pollutant = "NEofPM", ylab = "% points", main = "Non-exhaust percentage of PM10", deseason = TRUE)

write.csv(MB_inc_NE_Pollution_set, file = "MB_inc_NE_Pollution_set_final.csv", row.names = F)
MB_inc_NE_Pollution_set                 <- read.csv("C:/Users/andhr/OneDrive/Documents/MB_inc_NE_Pollution_set_final.csv", stringsAsFactors = FALSE)
MB_inc_NE_Pollution_set$date            <- as.POSIXct(strptime(MB_inc_NE_Pollution_set$date, format = "%d/%m/%Y"), tz = "GMT")

TheilSen(MB_inc_NE_Pollution_set, pollutant = "NEofPM", ylab = "% points", main = "Percentage of non-exhaust in PM10", deseason = TRUE)
smoothTrend(MB_inc_NE_Pollution_set, pollutant = "NEofPM", ylab = "% points", main = "Smooth Trend for percentage of non-exhaust in PM10", deseason = TRUE)
TheilSen(MB_inc_NE_Pollution_set, pollutant = "inc_PM10", ylab = "PM10 in ??g/m3", main = "Roadside increment of PM10", deseason = TRUE)

##Cardiff a bad background for Swansea - located far away and 80% of time - Cardiff greater than Swansea - PM10##

SM_inc_pollution_data      <- inner_join(SM_pollution_data, CF_pollution_data, by = "date")
SM_inc_pollution_data      <- select(SM_inc_pollution_data, c(1,3,4,6,7))
colnames(SM_inc_pollution_data) <- c("date", "SM_NOx", "SM_PM10", "CF_PM10", "CF_NOx")
SM_inc_pollution_data           <- na.omit(SM_inc_pollution_data)
write.csv(SM_inc_pollution_data, file = "SM_inc_pollution_data.csv", row.names = F)

#Result is that 45% of Cardiff data higher than Swansea roadside PM10 data#

### Traffic analysis after processed input (check SI) ###

TfL_Traffic_data_total          <- read.csv("C:/Users/andhr/OneDrive/Documents/TfL monthly traffic flows processed.csv", stringsAsFactors = FALSE)
TfL_Traffic_data_total$date     <- as.POSIXct(strptime(TfL_Traffic_data_total$date, format = "%d/%m/%Y"), tz = "GMT")
TfL_Traffic_data_total          <- subset(TfL_Traffic_data_total, date > as.Date("2011-03-31"))

#after removing outliers in the processed file - redone TheilSen#

TheilSen(TfL_Traffic_data_total, pollutant = "Daily_traffic_vol", ylab = "Average daily traffic", main = "Monthly average daily traffic on MB", deseason = TRUE)
TheilSen(TfL_Traffic_data_total, pollutant = "LGVs", ylab = "Light good vehicles", main = "Monthly average daily flow of LGVs on MB", deseason = TRUE)
TheilSen(TfL_Traffic_data_total, pollutant = "Bus_and_Coach", ylab = "Buses and coaches", main = "Monthly average daily flow of Buses and Coaches on MB", deseason = TRUE)
TheilSen(TfL_Traffic_data_total, pollutant = "Cars_and_Taxis", ylab = "Cars and taxis", main = "Monthly average daily flow of Cars and Taxis on MB", deseason = TRUE)
TheilSen(TfL_Traffic_data_total, pollutant = "all_hgvs", ylab = "Heavy good vehicles", main = "Monthly average daily flow of HGVs on MB", deseason = TRUE)

##AADF analysis with every annual component using lm based gg plot##

##Marylebone attempt##

AADF_MB_plot   <- AADF_2011_2019[,c(1,3)]
colnames(AADF_MB_plot) <- c("x","y")
df <- AADF_MB_plot


lm_eqn <- function(df){
     m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
        list(a = format(unname(coef(m)[1]), digits = 2),
             b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 2)))
    as.character(as.expression(eq));
}

p <- ggplot(data = df, aes(x=x, y=y))+
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y~x)+
  geom_point()+
  geom_text(x=2014, y=79000, label = lm_eqn(df), parse = TRUE)

p


##Decided to not give importance to lm reg plots on R##
##extracting the needful data and finishing it in excel as it's supplementary information anyway##

SM_inc_NE_Pollution_set_annual <- timeAverage(SM_inc_2011_2019_NExhaust, avg.time = "year")
MB_inc_NE_Pollution_annual     <- select(MB_inc_NE_Pollution_set_annual, c(1:5))

RegLinearPlot                  <- inner_join(MB_inc_NE_Pollution_annual, SM_inc_NE_Pollution_set_annual, by = "date")
write.csv(RegLinearPlot, "RegLinearPlot without traffic.csv",row.names = FALSE)

##WS, WD from Met_data for polar plots##

Met_data_MB            <- select(Met_data, c(1,2,3))
MB_poll_data_polar     <- subset(MB_pollution_data, date >as.Date("2011-03-31"))
WM_poll_data_polar     <- subset(WM_pollution_data, date >as.Date("2011-03-31"))
MB_poll_data_polar     <- inner_join(MB_poll_data_polar, Met_data_MB, by = "date")
polarPlot(MB_poll_data_polar, pollutant = "pm10")
WM_poll_data_polar     <- inner_join(WM_poll_data_polar, Met_data_MB, by = "date")
polarPlot(WM_poll_data_polar, pollutant = "pm10")

##use the original poll_data_polar files before merging with met##

inc_MB_poll_polar             <- inner_join(MB_poll_data_polar, WM_poll_data_polar, by = "date")
inc_MB_poll_polar$incPM10     <- inc_MB_poll_polar$pm10.x - inc_MB_poll_polar$pm10.y
inc_MB_poll_polar             <- inner_join(inc_MB_poll_polar, Met_data_MB, by = "date")
polarPlot(inc_MB_poll_polar, pollutant = "incPM10")

#Swansea polarPlot (one) as there is no urban background data#

Met_data_SM            <- select(Sea_Met_data, c(1,2,3))
SM_poll_data_polar     <- subset(SM_pollution_data, date >as.Date("2011-03-31"))
SM_poll_data_polar     <- inner_join(SM_poll_data_polar, Met_data_SM, by = "date")
polarPlot(SM_poll_data_polar, pollutant = "pm10")


##Supplementary code one##

## Traffic analysis from scratch ##

TfL_Traffic_data_total <- inner_join(TfL_Traffic_data_East, TfL_Traffic_data_West, by = "date")
TfL_Traffic_data_total <- select(TfL_Traffic_data_total, c(1,2,6))
colnames(TfL_Traffic_data_total) <- c("date", "Total_vol_E", "Total_vol_W")
TfL_Traffic_data_total$Total_vol <- TfL_Traffic_data_total$Total_vol_E + TfL_Traffic_data_total$Total_vol_W
TfL_Traffic_data_total           <- timeAverage(TfL_Traffic_data_total, avg.time = "month")
TfL_Traffic_data_total           <- timeAverage(TfL_Traffic_data_total, avg.time = "year")
TfL_Traffic_data_total$Total_vol_E <- TfL_Traffic_data_total$Total_vol_E*24
TfL_Traffic_data_total$Total_vol_W <- TfL_Traffic_data_total$Total_vol_W*24
TfL_Traffic_data_total$Total_vol   <- TfL_Traffic_data_total$Total_vol*24

TheilSen(AADF_2010_2019, pollutant = "all_motor_vehicles", ylab = "Annual average daily traffic", main = "Annual average daily traffic on MB")
TheilSen(AADF_2010_2019, pollutant = "lgvs", ylab = "Light good vehicles", main = "Annual average daily flow of LGVs on MB")
TheilSen(AADF_2010_2019, pollutant = "buses_and_coaches", ylab = "Buses and coaches", main = "Annual average daily flow of Buses and Coaches on MB")
TheilSen(AADF_2010_2019, pollutant = "cars_and_taxis", ylab = "Cars and Taxis", main = "Annual average daily flow of Cars and Taxis on MB")


# Used AADF_2010_2019 for percentages

TfL_Traffic_data_total <- inner_join(TfL_Traffic_data_East, TfL_Traffic_data_West, by = "date")
TfL_Traffic_data_total <- select(TfL_Traffic_data_total, c(1,2,6))
colnames(TfL_Traffic_data_total) <- c("date", "Total_vol_E", "Total_vol_W")
TfL_Traffic_data_total$Total_vol <- TfL_Traffic_data_total$Total_vol_E + TfL_Traffic_data_total$Total_vol_W
TfL_Traffic_data_total <- timeAverage(TfL_Traffic_data_total, avg.time = "month")

write.csv(AADF_2010_2019, file ="AADF.csv", row.names = F)
write.csv(TfL_Traffic_data_total, file = "TfL monthly traffic flows.csv", row.names = F)

## Traffic data from 2010 to 2013 - Analysis

Traffic_data_2010_2013  <- read.csv("C:/Users/andhr/OneDrive/Documents/traffic.csv", stringsAsFactors = FALSE)


Traffic_data_2010_2013$Date.Time  <- as.POSIXct(strptime(Traffic_data_2010_2013$Date.Time, format = "%d/%m/%Y"), tz = "GMT")
names(Traffic_data_2010_2013)[names(Traffic_data_2010_2013)=="Date.Time"] <- "date"

Traffic_data_2010_2013$Daily_vol <- as.numeric(as.character(Traffic_data_2010_2013$Daily_vol))

Traffic_data_2010_2013          <- timeAverage(Traffic_data_2010_2013, avg.time = "month")
write.csv(Traffic_data_2010_2013, file = "Traffic 2010 to 2013 processed.csv", row.names = F)

##Supplementary code two##

##Old code for corr plot##

MB_inc_NE_cor <- select(MB_inc_NE_MLR, c(2,3,4,10,12,14))
MB_inc_NE_cor <- na.omit(MB_inc_NE_cor)
ggcorr(MB_inc_NE_cor, label = TRUE, label_alpha = TRUE)

##Supplementary code three##

#Joined NE with met data but due to lesser points (<100) polarPlot didn't get created#

Met_data_NE_MB         <- timeAverage(Met_data_MB, avg.time="month")

MB_inc_NE_polar        <- inner_join(MB_inc_2010_2019_NExhaust, Met_data_NE_MB, by = "date")

Met_data_NE_SM         <- timeAverage(Met_data_SM, avg.time = "month")

SM_inc_NE_polar        <- inner_join(SM_inc_2011_2019_NExhaust, Met_data_NE_SM, by = "date")

polarPlot(MB_inc_NE_polar, pollutant = "Non_exhaust_PM")

polarPlot(SM_inc_NE_polar, pollutant = "Non_exhaust_PM")
