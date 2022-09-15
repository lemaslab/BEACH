##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        August 8, 2018 
# IRB:
# Description: Import/format facebook data for BEACH study  
# Data: C:\Users\Dominick\Dropbox (UFL)\02_Projects\BEACH_STUDY\01_Recruitment\facebook
# Exprt: C:\Users\Dominick\Dropbox (UFL)\02_Projects\BEACH_STUDY\01_Recruitment\facebook\rdata
# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\01_Recruitment\\facebook\\data\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\01_Recruitment\\facebook\\data\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\01_Recruitment\\facebook\\rdata\\",sep="");out.dir

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
# ***************       BEACH Report 3-23-18 to 5-9-18 .xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="BEACH Report 3-23-18 to 5-9-18 .xlsx";data.file.name

# **************************************************************************** #
# ***************                Results by Day of Week                                              
# **************************************************************************** #

# read data
day.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Results by Day of Week", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max))

# rename
days.dat=rename(day.dat, reporting_starts = `Reporting Starts`, 
               reporting_ends=`Reporting Ends`, 
               ad_set_name='Ad Set Name', 
               ad_set_delivery='Ad Set Delivery', 
               results=`Results`, 
               result_indicator='Result Indicator', 
               reach=Reach,
               impressions=Impressions, 
               cost_per_results='Cost per Results', 
               ad_set_budget='Ad Set Budget',
               budget_type='Ad Set Budget Type', 
               amount_spent_USD='Amount Spent (USD)', 
               ends=Ends,
               starts=Starts,
               frequency=Frequency, 
               unique_link_clicks='Unique Link Clicks',
               button_clicks='Link Clicks')

# compute day of week
days.dat$weekday=weekdays(as.Date(days.dat$reporting_ends,'%Y-%m-%d',tz = "UTC"))
days.dat$weekday=factor(days.dat$weekday, levels = c("Sunday","Monday","Tuesday","Wednesday",
                           "Thursday","Friday","Saturday"))

# **************************************************************************** #
# ***************                Results by Time of Day                                             
# **************************************************************************** #

# read data
time.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Results by Time of Day", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max))

# rename
time.dat=rename(time.dat, reporting_starts = `Reporting Starts`, 
               reporting_ends=`Reporting Ends`, 
               ad_set_name='Ad Set Name', 
               time_of_day=`Time of Day (Ad Account Time Zone)`,
               ad_set_delivery=`Ad Set Delivery`, 
               results=`Results`, 
               result_indicator='Result Indicator', 
               reach=Reach,
               impressions=Impressions, 
               cost_per_results='Cost per Results', 
               ad_set_budget='Ad Set Budget',
               budget_type='Ad Set Budget Type', 
               amount_spent_USD='Amount Spent (USD)', 
               ends=Ends,
               starts=Starts,
               frequency=Frequency, 
               unique_link_clicks='Unique Link Clicks',
               link_clicks='Link Clicks',
               button_clicks='Button Clicks')

# compute 24 hours in day
time.dat$hours=as.factor(seq_along(time.dat$time_of_day))
names(time.dat)

# **************************************************************************** #
# ***************                Results by Age                                             
# **************************************************************************** #

# read data
age.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Results by Age", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max))

# rename
age.dat=rename(age.dat, reporting_starts = `Reporting Starts`, 
                reporting_ends=`Reporting Ends`, 
                ad_set_name='Ad Set Name', 
                age=`Age`,
                ad_set_delivery=`Ad Set Delivery`, 
                results=`Results`, 
                result_indicator='Result Indicator', 
                reach=Reach,
                impressions=Impressions, 
                cost_per_results='Cost per Results', 
                ad_set_budget='Ad Set Budget',
                budget_type='Ad Set Budget Type', 
                amount_spent_USD='Amount Spent (USD)', 
                ends=Ends,
                starts=Starts,
                frequency=Frequency, 
                unique_link_clicks='Unique Link Clicks',
                link_clicks='Link Clicks')

# select variables
age.dat=age.dat %>%
  select(reporting_starts, reporting_ends,ad_set_name, age, ad_set_delivery,
         results, result_indicator, reach, impressions, cost_per_results, 
         ad_set_budget, budget_type, amount_spent_USD, ends, starts, frequency,
         unique_link_clicks, link_clicks) 
names(age.dat)

# **************************************************************************** #
# ***************       Create/Export Single Object                                              
# **************************************************************************** #

# combine into single object
# days.dat
# time.dat
# age.dat

facebook=list(days.dat,time.dat,age.dat)

# **************************************************************************** #
# *****      Export data                                              
# **************************************************************************** #

now=Sys.Date(); today=format(now, format="%d%b%y")
save(facebook, file=paste0(out.dir,"beach_facebook_",today,".rdata"))
