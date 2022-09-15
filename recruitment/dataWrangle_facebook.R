##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        May 16, 2018 
# IRB:
# Description: Import facebook rdata object for BEACH study  
# Data: C:\Users\Dominick\Dropbox (UFL)\02_Projects\BEACH_STUDY\01_Recruitment\facebook\rdata

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\01_Recruitment\\facebook\\rdata\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\01_Recruitment\\facebook\\rdata\\",sep="");data.dir
# out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\01_Recruitment\\facebook\\rdata\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readxl)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)

# **************************************************************************** #
# *****              load data: beach_facebook_16May18.rdata       
# **************************************************************************** # 

load(file="beach_facebook_16May18.rdata")
head(facebook); dim(facebook)
names(facebook)

# **************************************************************************** #
# *****              days of week       
# **************************************************************************** # 

# create data
days.dat=tbl_df(as.data.frame(facebook[1]))
names(days.dat)

# select data
days.dat %>%
  select(weekday,unique_link_clicks,button_clicks,cost_per_results) %>%
  group_by(weekday) %>%
  summarize(click.m=mean(unique_link_clicks),
            button.m=mean(button_clicks),
            cost.m=mean(cost_per_results)) 

# # plot: click.m 
# ggplot(days.dat, aes(x=factor(weekday), y=click.m))+geom_bar(stat="identity")
# 
# # plot: button.m 
# ggplot(days.dat, aes(x=factor(weekday), y=button.m))+geom_bar(stat="identity")
# 
# # plot: cost.m 
# ggplot(days.dat, aes(x=factor(weekday), y=cost.m))+geom_bar(stat="identity")
# 

# **************************************************************************** #
# *****              time of week       
# **************************************************************************** # 

# create data
time.dat=tbl_df(as.data.frame(facebook[2]))
names(time.dat)

# time
# # plot: click.m 
# ggplot(newdata2, aes(x=factor(hours), y=results))+geom_bar(stat="identity")

# **************************************************************************** #
# *****              age groups       
# **************************************************************************** # 

# create data
age.dat=tbl_df(as.data.frame(facebook[3]))
names(age.dat)

