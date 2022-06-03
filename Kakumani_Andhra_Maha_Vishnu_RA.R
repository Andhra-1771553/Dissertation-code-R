#EPIC_CHICAGO
rm(list=ls())

#Installing required packages
install.packages(c("dplyr","openair","raster","ggplot2", "plotly", "worldmet", "tidyr", "expss","mcr","haven", "tidyverse"))  

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
library(haven)
library(tidyverse)

#loaded DTA file and saved as .csv for convenience while using MS Excel

Africa_GDP <- read_dta('C:/Users/andhr/Downloads/EPIC_India_Data_test/Africa_gdp.dta')

write.csv(Africa_GDP,"C:\\Users\\andhr\\Downloads\\EPIC_India_Data_Test_DTA.csv", row.names = FALSE)

WDI_Agricultural_VA <- read.csv("C:/Users/andhr/Downloads/EPIC_India_Data_test/WDI_Agricultural_VA.csv")

WDI_Agricultural_VA_Transpose <- as.data.frame(t(WDI_Agricultural_VA))

Africa_merged <- merge(WDI_Agricultural_VA, Africa_GDP, by = c("year","CountryName"))

#Solved test thus far using excel and R

#Reason to stop data analysis on R: Error in fix.by(by.x,x) : must specify uniquely valid columns
