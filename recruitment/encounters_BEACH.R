##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        August 17, 2018 
# IRB:
# Description: Analysis of recruitment/encounter data extracted from RedCap. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_INTERVIEW\RedCap\ALL_DATA
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\RedCap\\ALL_DATA\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\RedCap\\ALL_DATA\\",sep="");data.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

# Load libraries (run the install first time for package)
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr")
library(ggplot2)
library(lubridate)
library(dplyr)

# **************************************************************************** #
# ***************  BEACHInterviewOperat_ENCOUNTER_DATA_2018-08-08_1407.csv                                              
# **************************************************************************** # 

#Read Data
data.file.name="BEACHInterviewOperat_ENCOUNTER_DATA_2018-08-08_1407.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
encounter<- read.csv(data.file.path)

# what are the variable names
names(encounter)
head(encounter)
             
# limit to only a few variables
encounter.s=encounter[,c(1,3:4)]
names(encounter.s)
head(encounter.s)

# **************************************************************************** #
# ***************  format data                                              
# **************************************************************************** # 

# what is the structure of the data
str(encounter.s)

# reformat variables
encounter.s$encounter_date_int=as.Date(encounter.s$encounter_date_int, "%m/%d/%Y")
encounter.s$encounter_type_int=as.factor(encounter.s$encounter_type_int)
encounter.s$encounter_type_int=recode(encounter.s$encounter_type_int, "1"="email", "2"="phone", "3"="other")
levels(encounter.s$encounter_type_int)

# format dates for plot
df2=encounter.s %>%
  mutate(month_name = month(encounter_date_int, label = TRUE)) %>%
  mutate(year_name = year(encounter_date_int)) %>%
  group_by(year_name,month_name,encounter_type_int) %>%
  tally() %>%
  mutate(month_yr=paste(month_name,year_name, sep="_")) 
  df2$month_yr=as.factor(df2$month_yr)
  levels(df2$month_yr)

  df2$month_yr <- ordered(df2$month_yr, levels = c("Jun_2017","Oct_2017","Nov_2017","Dec_2017","Jan_2018",
                                                "Feb_2018","Mar_2018","Apr_2018","May_2018"))
  
# **************************************************************************** #
# ***************  plot data                                              
# **************************************************************************** # 

# plot counts of "encounter_type_int" according to time
p=ggplot(df2, aes(x=month_yr, y=n, fill=encounter_type_int)) + geom_bar(stat="identity")

p+ggtitle("BEACH Interview Encounters: \n 2017-2018")
