### DATA SCIENCE FES CLASS PROJECT

### THE UNINSURED AMERICAN AND DEATH BY INFECTIOUS DISEASE: A DATA EXPLORATION

### MAIN OBJECTIVE: Seeking realitionship of infectious disease mortality and environmental data
# AIM 1 assess relationship of precentage of uninsured for US county and mortality rate by total ID
# AIM 2 assess relationship of precentage of uninsured for US county and mortality rate by specific ID
# AIM 3 assess relationship of percentage of uninsured for US county stratifying by race/ethnicity and income level for ID MR


#set working directory
setwd("C:/Users/chris/OneDrive/Documents/Data Science class/data") #on home laptop
#setwd("C:/Users/crj23/AppData/ocal/Packages/Microsoft.MicrosoftEdge_8wekyb3d8bbwe/TempState/Downloads/data (1).zip/data")
#setwd("C:/Users/crj23/Documents/data/data") #at CSSSI

#Packages used
install.packages(c("readr", "readxl", "tidyr", "dplyr","devtools","tidyverse","rtool","biscale","socviz", "cowplot"))
install.packages("Rtools")

library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(Rtools)
library(biscale)
library(sf)
library(ggplot2)
library(maps)
library(scales)
library(ggmap)
library(viridis)
library(socviz)
library(cowplot)





      ###### downloading data from URL for infectious disease mortality data: MAIN challenge thus far
      ###Having issues with this process
      ##I can download manualy, but not able to workaround automating it through R
      
      #dir.create("data")
      #destfile <- "C:/Users/chris/OneDrive/Documents/Data Science class/data/IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX"
      #url <- "http://ghdx.healthdata.org/us-data/IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_XLSX"
      #download.file(url, destfile, mode = "wb")
      #setwd("C:/Users/chris/OneDrive/Documents/Data Science class/data")
      #data <- read_xlsx("/IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX")
      
      #downloading insurance coverage from URL land use and enviromental data / weather data
      
      #https://www.census.gov/data/datasets/time-series/demo/sahie/estimates-acs.html
      ###Same as above 
      ##I will update this as time goes on
      #For now I will attach files for both data and will complete trasnformation of the data and the exploratory analysis
      
      ####Importing data sets
      #Infectious disease data sets, each sheet a different data set




### Tuberculosis Mortality rate for 2010 and 2014
    # Importing specific data set from file
    tb_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 1, skip= 1)
    
    # Cleaning up data sets to separate mortality rate and CI
    tb_data$MR2010tb <- tb_data$`Mortality Rate, 2010*`
    tb_data$MR2014tb <- tb_data$`Mortality Rate, 2014*`
    
    # Removing columns that I will not be using for this project and spliting variable into MR and lower and upper CI
    tb_data <- select(tb_data, -c(1,3:8,11))
    tb_data_s <- separate(tb_data, MR2010tb, into = c("MR2010tb", "CI_l2010tb", "CI_U2010tb"), sep = " ", extra = "merge")
    tb_data_sp <- separate(tb_data_s, MR2014tb, into = c("MR2014tb", "CI_l2014tb", "CI_U2014tb"), sep = " ", extra = "merge")
    
    #TASK: Need to remove parentheses and commas from lower and upper CI (not urgent but good for a clean data set)
    
    # Preparing variables for summation for Mortality rate of all Infectious Diseases; converting to numberic values
    tb_data_sp$MR2010tb <- as.numeric(tb_data_sp$MR2010tb) #preparing for suming for total MR
    is.numeric(tb_data_sp$MR2010tb) #checking to make sure variables are numeric
    tb_data_sp$MR2014tb <- as.numeric(tb_data_sp$MR2014tb) #preparing for suming for total MR
    is.numeric(tb_data_sp$MR2014tb) #checking to make sure variables are numeric


### HIV Mortality rate for 2010 and 2014
    # Importing specific data set from file
    HIV_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 2, skip = 1)
    
    # Cleaning up data sets to separate mortality rate and CI
    HIV_data$MR2010HIV <- HIV_data$`Mortality Rate, 2010*`
    HIV_data$MR2014HIV <- HIV_data$`Mortality Rate, 2014*`
    
    # Removing columns that I will not be using for this project and spliting variable into MR and lower and upper CI
    HIV_data <- select(HIV_data, -c(1,3:8,11))
    HIV_data_s <- separate(HIV_data, MR2010HIV, into = c("MR2010HIV", "CI_l2010HIV", "CI_U2010HIV"), sep = " ", extra = "merge")
    HIV_data_sp <- separate(HIV_data_s, MR2014HIV, into = c("MR2014HIV", "CI_l2014HIV", "CI_U2014HIV"), sep = " ", extra = "merge")
    
    #TASK: Need to remove parentheses and commas from lower and upper CI (not urgent but good for a clean data set)
    
    # Preparing variables for summation for Mortality rate of all Infectious Diseases; converting to numberic values
    HIV_data_sp$MR2010HIV <- as.numeric(HIV_data_sp$MR2010HIV) #preparing for suming for total MR
    is.numeric(HIV_data_sp$MR2010HIV) #checking to make sure variables are numeric
    HIV_data_sp$MR2014HIV <- as.numeric(HIV_data_sp$MR2014HIV) #preparing for suming for total MR
    is.numeric(HIV_data_sp$MR2014HIV) #checking to make sure variables are numeric


### Diarrhaeal Illness Mortality rate for 2010 and 2014
    # Importing specific data set from file
    di_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 3, skip = 1)
    
    # Cleaning up data sets to separate mortality rate and CI
    di_data$MR2010di <- di_data$`Mortality Rate, 2010*`
    di_data$MR2014di <- di_data$`Mortality Rate, 2014*`
    
    # Removing columns that I will not be using for this project and spliting variable into MR and lower and upper CI
    di_data <- select(di_data, -c(1,3:8,11))
    di_data_s <- separate(di_data, MR2010di, into = c("MR2010di", "CI_l2010di", "CI_U2010di"), sep = " ", extra = "merge")
    di_data_sp <- separate(di_data_s, MR2014di, into = c("MR2014di", "CI_l2014di", "CI_U2014di"), sep = " ", extra = "merge")
   
    #TASK: Need to remove parentheses and commas from lower and upper CI (not urgent but good for a clean data set)
    
    # Preparing variables for summation for Mortality rate of all Infectious Diseases; converting to numberic values
    di_data_sp$MR2010di <- as.numeric(di_data_sp$MR2010di) #preparing for suming for total MR
    is.numeric(di_data_sp$MR2010di) #checking to make sure variables are numeric
    di_data_sp$MR2014di <- as.numeric(di_data_sp$MR2014di) #preparing for suming for total MR
    is.numeric(di_data_sp$MR2014di) #checking to make sure variables are numeric


### Lower Respiratory Infectious Diseases Mortality rate for 2010 and 2014
    # Importing specific data set from file
    lr_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 4, skip = 1)
    
    # Basic Tranformation of variables
    lr_data$MR2010lr <- lr_data$`Mortality Rate, 2010*`
    lr_data$MR2014lr <- lr_data$`Mortality Rate, 2014*`
    
    # Removing columns that I will not be using for this project 
    lr_data <- select(lr_data, -c(1,3:8,11))
    
    # Separation of mortality rate and confidience intervals
    lr_data_s <- separate(lr_data, MR2010lr, into = c("MR2010lr", "CI_l2010lr", "CI_U2010lr"), sep = " ", extra = "merge")
    lr_data_sp <- separate(lr_data_s, MR2014lr, into = c("MR2014lr", "CI_l2014lr", "CI_U2014lr"), sep = " ", extra = "merge")
    
    #TASK: Need to remove parentheses and commas from lower and upper CI
    
    # Preparing variables for summation for Mortality rate of all Infectious Diseases; converting to numberic values
    lr_data_sp$MR2010lr <- as.numeric(lr_data_sp$MR2010lr) 
    is.numeric(lr_data_sp$MR2010lr)
    lr_data_sp$MR2014lr <- as.numeric(lr_data_sp$MR2014lr) 
    is.numeric(lr_data_sp$MR2014lr) 


### Meningitis Infectious Diseases Mortality rate for 2010 and 2014
    # Importing specific data set from file
    m_data <- read_xlsx("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 5, skip = 1)
     
    # Basic Tranformation of variables
    m_data$MR2010m <- m_data$`Mortality Rate, 2010*`
    m_data$MR2014m <- m_data$`Mortality Rate, 2014*`
    
    # Removing columns that I will not be using for this project 
    m_data <- select(m_data, -c(1,3:8,11))
    
    # Separation of mortality rate and confidience intervals
    m_data_s <- separate(m_data, MR2010m, into = c("MR2010m", "CI_l2010m", "CI_U2010m"), sep = " ", extra = "merge")
    m_data_sp <- separate(m_data_s, MR2014m, into = c("MR2014m", "CI_l2014m", "CI_U2014m"), sep = " ", extra = "merge")
    
    #TASK: Need to remove parentheses and commas from lower and upper CI
    
    # Preparing variables for summation for Mortality rate of all Infectious Diseases; converting to numberic values
    m_data_sp$MR2010m <- as.numeric(m_data_sp$MR2010m) 
    is.numeric(m_data_sp$MR2010m)
    m_data_sp$MR2014m <- as.numeric(m_data_sp$MR2014m) 
    is.numeric(m_data_sp$MR2014m) 
  
    
### Hepatitis Infectious Diseases Mortality rate for 2010 and 2014
    # Importing specific data set from file 
    h_data <- read_excel("IHME_USA_COUNTY_INFECT_DIS_MORT_1980_2014_NATIONAL_Y2018M03D27.XLSX",sheet = 6, skip = 1)
    
    # Basic Tranformation of variables
    h_data$MR2010h <- h_data$`Mortality Rate, 2010*`
    h_data$MR2014h <- h_data$`Mortality Rate, 2014*`
   
    # Removing columns that I will not be using for this project 
    h_data <- select(h_data, -c(1,3:8,11))
    
    # Separation of mortality rate and confidience intervals
    h_data_s <- separate(h_data, MR2010h, into = c("MR2010h", "CI_l2010h", "CI_U2010h"), sep = " ", extra = "merge")
    h_data_sp <- separate(h_data_s, MR2014h, into = c("MR2014h", "CI_l2014h", "CI_U2014h"), sep = " ", extra = "merge")
    
    #TASK: Need to remove parentheses and commas from lower and upper CI
    
    # Preparing variables for summation for Mortality rate of all Infectious Diseases; converting to numberic values
    h_data_sp$MR2010h <- as.numeric(h_data_sp$MR2010h) 
    is.numeric(h_data_sp$MR2010h)
    h_data_sp$MR2014h <- as.numeric(h_data_sp$MR2014h) 
    is.numeric(h_data_sp$MR2014h) 


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
UI_2010n <- select(UI_2010n, -c(2,4:8,25))


#for 2014
is.character(UI_2014n$statefips)
UI_2014n$statefips <- as.numeric(UI_2014n$statefips)
UI_2014n$countyfips <- as.numeric(UI_2014n$countyfips)
UI_2014n$countyfips <- sprintf("%03d", UI_2014n$countyfips) #need to format so when adding get 4-5 digits in order to match the ID datsets
UI_2014n$countyfips <- as.character(UI_2014n$countyfips)#need to reformat as a character in order to make values work well

UI_2014n <- unite(UI_2014n, FIPS, statefips:countyfips, sep = "")
UI_2014n <- select(UI_2014n, -c(2,4:8,25))

#Merge based on FIPS
UI_data <- merge(UI_2010n, UI_2014n, by = "FIPS")

#can probably rename values, .x from a variable is from 2010 and .y is for 2014 variables. 
#Can limit data.frame to specific variables used.
UI_data
#Merge data sets by FIPS with id datasets
tb_ui <- merge(UI_data, tb_data_sp, by = "FIPS") #lost some observations, need to investigate and narrow down variables to ones wanted
#also need to automate these to one line instead of copying and pasting
#HIV_ui <- merge(HIV_data_sp, UI_data, by = "FIPS")
#di_ui <- merge(di_data_sp, UI_data, by = "FIPS")
#lr_ui <- merge(lr_data_sp, UI_data, by = "FIPS")
#m_ui <- merge(m_data_sp, UI_data, by = "FIPS")
#h_ui <- merge(h_data_sp, UI_data, by = "FIPS")

#Creating total MRID2010 and MRID2014 in new data frame
t_ui1 <- merge(tb_ui, HIV_data_sp, by = "FIPS")
t_ui2 <- merge(t_ui1, di_data_sp, by = "FIPS")
t_ui3 <- merge(t_ui2, lr_data_sp, by = "FIPS")
t_ui4 <- merge(t_ui3, m_data_sp, by = "FIPS")
t_ui <- merge(t_ui4, h_data_sp, by = "FIPS")

# Inspecting the data set
View(t_ui)

#creating a total MR for all ID deaths
t_ui$MRID2010 <- (t_ui$MR2010tb + t_ui$MR2010HIV + t_ui$MR2010di + t_ui$MR2010lr + t_ui$MR2010m + t_ui$MR2010h)
t_ui$MRID2014 <- (t_ui$MR2014tb + t_ui$MR2014HIV + t_ui$MR2014di + t_ui$MR2014lr + t_ui$MR2014m + t_ui$MR2014h)
 
# Creating difference in moratality rate from 2014 to 2010
t_ui$MRIDdiff <- t_ui$MRID2014 - t_ui$MRID2010
t_ui$MRTBdiff <- t_ui$MR2014tb - t_ui$MR2010tb
t_ui$MRHIVdiff <- t_ui$MR2014HIV - t_ui$MR2010HIV
t_ui$MRDIdiff <- t_ui$MR2014di - t_ui$MR2010di
t_ui$MRLRdiff <- t_ui$MR2014lr - t_ui$MR2010lr
t_ui$MRMdiff <- t_ui$MR2014m - t_ui$MR2010m
t_ui$MRHdiff <- t_ui$MR2014h - t_ui$MR2010h

# Creating differnce of percentage of uninsured individuals in each County
t_ui$PCTUIdiff <- t_ui$PCTUI.y - t_ui$PCTUI.x
t_ui$PCTICdiff <- t_ui$PCTIC.y - t_ui$PCTIC.x

#Data visualization Need to start with data visualization 
t_ui$PCTUI2010 <- t_ui$PCTUI.x     
t_ui$PCTUI2014 <- t_ui$PCTUI.y  
t_ui$PCTIC2010 <- t_ui$PCTIC.x     
t_ui$PCTIC2014 <- t_ui$PCTIC.y 
colnames(t_ui) <- make.unique(names(t_ui))

# Checking the distribution of the different varaiables
hist(t_ui$MRTBdiff)
hist(t_ui$MRHIVdiff)
hist(t_ui$MRLRdiff)
hist(t_ui$MRDIdiff)
hist(t_ui$MRMdiff)
hist(t_ui$MRHdiff)

par(mfrow = c(1,3))
hist(t_ui$MRID2010, breaks = 10)
hist(t_ui$MRID2014, breaks = 10)
hist(t_ui$MRIDdiff)


hist(t_ui$PCTUI2010)
hist(t_ui$PCTUI2014)
hist(t_ui$PCTUIdiff)

hist(t_ui$PCTIC2010)
hist(t_ui$PCTIC2014)
hist(t_ui$PCTICdiff)

ggplot(t_ui, aes(MRID2010,PCTUI2010)) +
    geom_jitter(MRID2010,PCTUI2010)
# Getting Summary infromation from each variable of interest
summary(t_ui$MRID2010)
summary(t_ui$MRID2014)
summary(t_ui$MRIDdiff)

summary(t_ui$PCTUI2010)
summary(t_ui$PCTUI2014)
summary(t_ui$PCTUIdiff)

summary(t_ui$PCTIC2010)
summary(t_ui$PCTIC2014)
summary(t_ui$PCTICdiff)

# Data exploration through visulization with scatter plots: plotting variables of interst together
    ggplot(t_ui, aes(MRID2010, PCTUI2010)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MRID2014, PCTUI2014)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MRIDdiff, PCTUIdiff)) +
      geom_point(aes(color = "cyl"))
            
    ggplot(t_ui, aes(MR2010tb, PCTUI2010)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MR2010HIV, PCTUI2010)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MR2010di, PCTUI2010)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MR2010lr, PCTUI2010)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MR2010m, PCTUI2010)) +
      geom_point(aes(color = "cyl"))
    
    ggplot(t_ui, aes(MR2010h, PCTUI2010)) +
      geom_point(aes(color = "cyl"))

## Mapping with ggplot
# Preparing data

county_map$FIPS <- as.numeric(county_map$id) 
t_ui$FIPS <- as.numeric(t_ui$FIPS )
t_ui$FIPS <- sprintf("%05d", t_ui$FIPS) #need to format so when adding get 4-5 digits in order to match the ID datsets
county_map$FIPS <- sprintf("%05d", county_map$FIPS)
t_uin <- left_join(county_map, t_ui, by = "FIPS")
colnames(t_uin) <- make.unique(names(t_uin))


# Map for MR of ID in 2010
    p <- ggplot(data = t_uin,
           mapping = aes(x = long, y = lat, 
                         fill = MRID2010,
                         group = group))
    
    p1 <- p + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
        
    p2 <- p1 + scale_fill_viridis(option = "viridis", direction = -1, name = "Deaths per 100,000 people")
    
    p3 <- p2 + labs(title = "Infectious Disease Mortality Rate of United States Counties in 2010") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
              
    p3

# Map for MR of ID in 2014
    q <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = MRID2014,
                              group = group))
    
    q1 <- q + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
              
    q2 <- q1 + scale_fill_viridis(option = "viridis", direction = -1, name = "Deaths per 100,000 people")
    
    q3 <- q2 + labs(title = "Infectious Disease Mortality Rate of United States Counties in 2014") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
  
    q3

# Map for percent uninsured in 2010
    r <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = PCTUI2010,
                              group = group))
    
    r1 <- r + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    r2 <- r1 + scale_fill_viridis(option = "magma", direction = -1, name = "Percentage Uninsured")
    
    r3 <- r2 + labs(title = "Percentage of County Population Uninsured in the United States in 2010") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
    
    r3

# Map for percent uninsured in 2014

    s <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = PCTUI2014,
                              group = group))
    
    s1 <- s + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    s2 <- s1 + scale_fill_viridis(option = "magma", direction = -1, name = "Percentage Uninsured")
    
    s3 <- s2 + labs(title = "Percentage of County Population Uninsured in the United States in 2014") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
    
    s3

# Map for percent insured in 2010
    t <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = PCTIC2010,
                              group = group))
    
    t1 <- t + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    t2 <- t1 + scale_fill_viridis(option = "viridis", direction = -1, name = "Percentage Insured")
    
    t3 <- t2 + labs(title = "Percentage of County Population Insured in the United States in 2010") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
    
    t3

# Map for percent insured in 2014
    u <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = PCTIC2014,
                              group = group))
    
    u1 <- u + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    u2 <- u1 + scale_fill_viridis(option = "viridis", direction = -1, name = "Percentage Insured")
    
    u3 <- u2 + labs(title = "Percentage of County Population Insured in the United States in 2014") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
    
    u3

# Map of difference in ID MR from 2010 to 2014 in the USA
    v <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = MRIDdiff,
                              group = group))
    
    v1 <- v + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    v2 <- v1 + scale_fill_viridis(option = "viridis", direction = -1, name = "Deaths per 100,000 people")
    
    v3 <- v2 + labs(title = "Difference in Infectious Disease Mortality Rate of United States Counties from 2010 to 2014") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
    
    v3

# Map of difference in Percent uninsured from 2010 to 2014 in the USA
    w <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = PCTUIdiff,
                              group = group))
    
    w1 <- w + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    w2 <- w1 + scale_fill_viridis(option = "inferno", direction = -1, name = "Percentage Uninsured")
    
    w3 <- w2 + labs(title = "Difference in Percent Uninsured in United States Counties from 2010 to 2014") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
    
    w3

# Map of difference in Percent insured from 2010 to 2014 in the USA
    x <- ggplot(data = t_uin,
                mapping = aes(x = long, y = lat, 
                              fill = PCTICdiff,
                              group = group))
    
    x1 <- x + geom_polygon(color = NA, size = 0.01) + 
                coord_equal() +
                theme_map()
    
    x2 <- x1 + scale_fill_viridis(option = "inferno", direction = -1, name = "Percentage Innsured")
    
    x3 <- x2 + labs(title = "Difference in Percent Insured in United States Counties from 2010 to 2014") +
                  theme(legend.position = "bottom") +
                  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))
                
    x3

## Attempting to create a bivariate map of the relationship of uninsured percentages and MR rates
# Uninsured to MR ID in 2010
    datay <- bi_class(t_uin, x = PCTUI2010, y = MRID2010, style = "quantile", dim = 3)
    datay <- subset(datay, datay$bi_class != "NA-1" & data$bi_class != "NA-NA" ) # remove NAs
    
    y <- ggplot(data = datay, 
                mapping = aes(x = long, y = lat, 
                              fill = bi_class,
                              group = group))
    
    y1 <- y + geom_polygon(color = NA, size = 0.05) + 
      coord_equal() +
      theme_map()+
      bi_theme()
    
    y2 <- y1 +  bi_scale_fill(pal = "DkCyan", dim = 3) +
      labs( title = "Uninsured and Infectious Disease Mortality", subtitle = "United States of America in 2010", x = NULL, y = NULL) +
      theme(legend.position = "none") 
    
    
    
    legendy <- bi_legend(pal = "DkCyan", dim = 3,
                         xlab = "Percentage of Uninsured",
                         ylab = "Deaths per 100,000 people",
                         size = 8)
    
    y3 <- ggdraw() +
      draw_plot(y2, 0, 0, 0.95, 1) +
      draw_plot(legendy, 0.7, 0.3,0.2, 0.2)
    y3
    
# Uninsured to MR ID in 2014
    dataz <- bi_class(t_uin, x = PCTUI2014, y = MRID2014, style = "quantile", dim = 3)
    dataz <- subset(dataz, dataz$bi_class != "NA-1" & data$bi_class != "NA-NA" ) # remove NAs
    
    z <- ggplot(data = dataz, 
                mapping = aes(x = long, y = lat, 
                fill = bi_class,
                group = group))
    
    z1 <- z + geom_polygon(color = NA, size = 0.05) + 
                coord_equal() +
                theme_map()+
                bi_theme()
    
    z2 <- z1 +  bi_scale_fill(pal = "DkCyan", dim = 3) +
      labs( title = "Uninsured and Infectious Disease Mortality", subtitle = "United States of America in 2014", x = NULL, y = NULL) +
      theme(legend.position = "none") 
      
    
    z2
    legendy <- bi_legend(pal = "DkCyan", dim = 3,
              xlab = "Percentage of Uninsured",
              ylab = "Deaths per 100,000 people",
              size = 8)
    
    z3 <- ggdraw() +
          draw_plot(z2, 0, 0, 0.95, 1) +
          draw_plot(legendy, 0.7, 0.3,0.2, 0.2)
    z3

# Differenc in Uninsured to Difference in MR ID in 2014
    dataa <- bi_class(t_uin, x = PCTUIdiff, y = MRIDdiff, style = "fisher", dim = 3)
    dataa <- subset(dataa, dataa$bi_class != "NA-1" & dataa$bi_class != "NA-NA" & dataa$bi_class != "NA-2" ) # remove NAs
    
    a <- ggplot(data = dataa, 
                mapping = aes(x = long, y = lat, 
                              fill = bi_class,
                              group = group))
    
    a1 <- a + geom_polygon(color = NA, size = 0.05) + 
      coord_equal() +
      theme_map()+
      bi_theme()
    
    a1
    a2 <- a1 +  bi_scale_fill(pal = "DkCyan", dim = 3) +
      labs( title = "Difference of Percent Uninsured and the \n Difference Infectious Disease Mortality", subtitle = "United States of America from 2010 to 2014", x = NULL, y = NULL) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5, size = 15))
    
    a2
    legendy <- bi_legend(pal = "DkCyan", dim = 3,
                         xlab = "Percentage of Uninsured",
                         ylab = "Deaths per 100,000 people",
                         size = 8)
    
    a3 <- ggdraw() +
      draw_plot(a2, 0, 0, 0.95, 1) +
      draw_plot(legendy, 0.7, 0.1,0.2, 0.2)
    a3
    
# stratifying by state
# Creating Alabama data set
data_AL <- subset(t_uin, t_uin$state_name.x == "Alabama")

# Summarizing data Exploring
hist(data_AL$MRTBdiff)
hist(data_AL$MRHIVdiff)
hist(data_AL$MRLRdiff)
hist(data_AL$MRDIdiff)
hist(data_AL$MRMdiff)
hist(data_AL$MRHdiff)
par(mfrow = c(1,3))
hist(data_AL$MRID2010, breaks = 10)
hist(data_AL$MRID2014, breaks = 10)
hist(data_AL$MRIDdiff)
hist(data_AL$PCTUI2010)
hist(data_AL$PCTUI2014)
hist(data_AL$PCTUIdiff)
hist(data_AL$PCTIC2010)
hist(data_AL$PCTIC2014)
hist(data_AL$PCTICdiff)
# Getting Summary infromation from each variable of interest

summary(data_AL$MRID2010)
summary(data_AL$MRID2014)
summary(data_AL$MRIDdiff)

summary(data_AL$PCTUI2010)
summary(data_AL$PCTUI2014)
summary(data_AL$PCTUIdiff)

summary(data_AL$PCTIC2010)
summary(data_AL$PCTIC2014)
summary(data_AL$PCTICdiff)


# Mapping the difference of uninsured from 2010 to 2014 in USA
b <- ggplot(data = data_AL,
            mapping = aes(x = long, y = lat, 
                          fill = PCTUIdiff,
                          group = group))

b1 <- b + geom_polygon(color = NA, size = 0.01) + 
  coord_equal() +
  theme_map()

b2 <- b1 + scale_fill_viridis(option = "inferno", direction = -1, name = "Percentage Innsured")

b3 <- b2 + labs(title = "Difference in Percent Uninsured in Alabama Counties from 2010 to 2014") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))

b3


datab <- bi_class(data_AL, x = PCTUIdiff, y = MRIDdiff, style = "quantile", dim = 3)
datab <- subset(datab, datab$bi_class != "NA-1" & datab$bi_class != "NA-NA" & datab$bi_class != "NA-2" ) # remove NAs

b <- ggplot(data = datab, 
            mapping = aes(x = long, y = lat, 
                          fill = bi_class,
                          group = group))

b1 <- b + geom_polygon(color = NA, size = 0.05) + 
  coord_equal() +
  theme_map()+
  bi_theme()


b2 <- b1 +  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs( title = "Difference of Percent Uninsured and the \n Difference Infectious Disease Mortality", subtitle = "Alabama from 2010 to 2014", x = NULL, y = NULL) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5, size = 15))

b2
legendy <- bi_legend(pal = "DkCyan", dim = 3,
                     xlab = "Percentage of Uninsured",
                     ylab = "Deaths per 100,000 people",
                     size = 8)

b3 <- ggdraw() +
  draw_plot(b2, 0, 0, 0.95, 1) +
  draw_plot(legendy, 0.7, 0.1,0.2, 0.2)
b3

# Uninsured in WI
data_WI <- subset(t_uin, t_uin$state_name.x == "Wisconsin")

# Summary Statistics
summary(data_WI$MRID2010)
summary(data_WI$MRID2014)
summary(data_WI$MRIDdiff)

summary(data_WI$PCTUI2010)
summary(data_WI$PCTUI2014)
summary(data_WI$PCTUIdiff)

summary(data_WI$PCTIC2010)
summary(data_WI$PCTIC2014)
summary(data_WI$PCTICdiff)


lm(PCTUIdiff~MRIDdiff, data = data_WI)

# Mapping
c <- ggplot(data = data_WI,
            mapping = aes(x = long, y = lat, 
                          fill = PCTUIdiff,
                          group = group))

c1 <- c + geom_polygon(color = NA, size = 0.01) + 
  coord_equal() +
  theme_map()

c2 <- c1 + scale_fill_viridis(option = "inferno", direction = -1, name = "Percentage Uninnsured")

c3 <- c2 + labs(title = "Difference in Percent Uninsured in Wisconsin Counties from 2010 to 2014") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5))

c3


datac <- bi_class(data_WI, x = PCTUIdiff, y = MRIDdiff, style = "fisher", dim = 3)
datac <- subset(datac, datac$bi_class != "NA-1" & datac$bi_class != "NA-NA" & datac$bi_class != "NA-2" ) # remove NAs

d <- ggplot(data = datac, 
            mapping = aes(x = long, y = lat, 
                          fill = bi_class,
                          group = group))

d1 <- d + geom_polygon(color = NA, size = 0.05) + 
  coord_equal() +
  theme_map()+
  bi_theme()


d2 <- d1 +  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs( title = "Difference of Percent Uninsured and the \n Difference Infectious Disease Mortality", subtitle = "Alabama from 2010 to 2014", x = NULL, y = NULL) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5, size = 15))


legendy <- bi_legend(pal = "DkCyan", dim = 3,
                     xlab = "Percentage of Uninsured",
                     ylab = "Deaths per 100,000 people",
                     size = 8)

d3 <- ggdraw() +
  draw_plot(d2, 0, 0, 0.95, 1) +
  draw_plot(legendy, 0.7, 0.1,0.2, 0.2)
d3
