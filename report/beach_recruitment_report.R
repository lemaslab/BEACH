##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        Dec 12, 2019 
# Project:     BEACH 
# Description: Recruitment Report for BEACH Study


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(redcapAPI)
library(REDCapR)
library(dplyr)
library(lubridate)

# Get Redcap API Token: 
# Gator Link VPN
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "beach_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
print(beach_token)

# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)

# list of instruments
exportInstruments(rcon)

# list of events
exportEvents(rcon)

# pull data (records and variables)
ds_all_rows_all_fields <- redcap_read(redcap_uri=uri, token=beach_token)$data
data=ds_all_rows_all_fields
length(unique(data$test_id))


# pull subset of variables
desired_fields_v1 <- c("test_id", "redcap_event_name", "beach_learn_about_study","beach_encounter_date","beach_study_encounters_complete",
                       "prescreen_date","beach_prescreen_survey_complete",
                       "beachphone_date","beachphone_hear_about_us","beach_phone_screen_complete",
                       "beach_part_consent","beach_consent_date1")

recruitment <- redcap_read(redcap_uri=uri, token=beach_token, fields= desired_fields_v1)$data

# check data 
names(recruitment)
unique(recruitment$test_id)
str(recruitment)

# rename data
dat=recruitment
names(dat)
str(dat)

# format dates
dat$prescreen_date=as.Date(dat$prescreen_date, "%Y-%m-%d")
dat$beach_encounter_date=as.Date(dat$beach_encounter_date, "%Y-%m-%d")
dat$beachphone_date=as.Date(dat$beachphone_date, "%Y-%m-%d")
dat$beach_consent_date1=as.Date(dat$beach_consent_date1, "%Y-%m-%d")
dat$phone_year=year(dat$beachphone_date)
dat$consent_year=year(dat$beach_consent_date1)


# analysis
#--------
names(dat)
dat$beach_phone_screen_complete

# how did phone screen encounter hear about us? 
# 1, Flyer | 2, Radio | 3, Social Media | 4, Newspaper | 5, Word-of-mouth | 6, Other
phone=dat %>%
  group_by(beachphone_hear_about_us, phone_year) %>%
  filter(beach_phone_screen_complete==2 & redcap_event_name=="baseline_arm_1") %>%
  select(test_id, beach_phone_screen_complete, redcap_event_name, beachphone_hear_about_us,phone_year) %>%
  tally(beachphone_hear_about_us)
  
 
