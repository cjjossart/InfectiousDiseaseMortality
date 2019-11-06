## DATA SCIENCE FES CLASS PROJECT
#Seeking realitionship of infectious disease mortality and environmental data

#set working directory
setwd("C:/Users/chris/OneDrive/Documents/Data Science class/data")

#Packages used
library(readxl)
library(readr)
library(tidyr)

#downloading data from URL for infectious disease mortality data
###Having issues with this process
##I can download manualy, but not able to workaround automating it through R

dir.create("data")
destfile <- "data"
url <- "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_XLSX.zip"
download.file(url, "Data Science class/data")
data <- read_xlsx("/IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX", sheet = "Diarrheal diseases")

#downloading insurance coverage from URL land use and enviromental data / weather data
###Same as above 
##I will update this as time goes on
#For now I will attach files for both data and will complete trasnformation of the data and the exploratory analysis

####Importing data sets
#Infectious disease data sets, each sheet a different data set

tb_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 1, skip= 1)
HIV_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 2, skip = 1)
di_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 3, skip = 1)
lr_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 4, skip = 1)
m_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 5, skip = 1)
h_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 6, skip = 1)

#Uninsured data set by county for years 2010 and 2014

UI_2010 <- read_csv("sahie_2010.csv", skip = 79)
UI_2014 <- read_csv("sahie_2014.csv", skip = 79)

#####Transforming data
#Need to trim some rows from uninsured data sets
#limited observations to those that include all people and not specific categories, and only to each county
UI_2010n <- subset(UI_2010, UI_2010$geocat == 50 & UI_2010$agecat == 0 & UI_2010$racecat == 0 & UI_2010$sexcat == 0 & UI_2010$iprcat == 0)
UI_2014n <- subset(UI_2014, UI_2014$geocat == 50 & UI_2014$agecat == 0 & UI_2014$racecat == 0 & UI_2014$sexcat == 0 & UI_2014$iprcat == 0)
##?observations off by 1, may need to find out what the diffence is

#Add the two datasets to one and another (want to combine all observations)

#originally binded the datasets but now think I will need to merge and create different values col names for percentage uninsured.
# Need to merge both datasets on FIPS county level number or by county; both require splitting of columns or adding them
#need to make sure single numbers have 3 values so 1 --> 001.
#for 2010
is.character(UI_2010n$statefips)
UI_2010n$statefips <- as.numeric(UI_2010n$statefips)
UI_2010n$countyfips <- as.numeric(UI_2010n$countyfips)
UI_2010n$countyfips <- sprintf("%03d", UI_2010n$countyfips) #need to format so when adding get 4-5 digits in order to match the ID datsets
UI_2010n$countyfips <- as.character(UI_2010n$countyfips)#need to reformat as a character in order to make values work well

UI_2010n <- unite(UI_2010n, FIPS, statefips:countyfips, sep = "")
#for 2014
is.character(UI_2014n$statefips)
UI_2014n$statefips <- as.numeric(UI_2014n$statefips)
UI_2014n$countyfips <- as.numeric(UI_2014n$countyfips)
UI_2014n$countyfips <- sprintf("%03d", UI_2014n$countyfips) #need to format so when adding get 4-5 digits in order to match the ID datsets
UI_2014n$countyfips <- as.character(UI_2014n$countyfips)#need to reformat as a character in order to make values work well

UI_2014n <- unite(UI_2014n, FIPS, statefips:countyfips, sep = "")

#Merge based on FIPS
UI_data <- merge(UI_2010n, UI_2014n, by = "FIPS")

#can probably rename values, .x from a variable is from 2010 and .y is for 2014 variables. 
#Can limit data.frame to specific variables used.
UI_data
#Merge data sets by FIPS with id datasets
tb_ui <- merge(tb_data, UI_data, by = "FIPS") #lost some observations, need to investigate and narrow down variables to ones wanted
#also need to automate these to one line instead of copying and pasting
HIV_ui <- merge(HIV_data, UI_data, by = "FIPS")
di_ui <- merge(di_data, UI_data, by = "FIPS")
lr_ui <- merge(lr_data, UI_data, by = "FIPS")
m_ui <- merge(m_data, UI_data, by = "FIPS")
h_ui <- merge(h_data, UI_data, by = "FIPS")


#Data has been transformed and I have created 