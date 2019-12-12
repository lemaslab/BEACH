##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        Dec 11, 2019 
# Project:     BEACH Interview
# Description: Summary Analysis for BEACH Study


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(redcapAPI)
library(REDCapR)

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

# list records
exportRecords(rcon)

# problem record: need to comb through records to identify problems.
# checked: 1 - 1020
consent.records.v2=c(1:1020) # problem record 1984- fixed. names dont match. check.
# pull data
ds_some_rows_v2 <- redcap_read(
  batch_size=250,
  records= consent.records.v2,
  redcap_uri = uri, 
  token      = beach_token
  #fields     = desired_fields_v1
)$data



data=ds_some_rows_v2
length(unique(data$test_id))

desired_records_v1 <- c("BLS001A")
desired_fields_v1 <- c("test_id", "redcap_event_name", "mom3t_breast_surg")

ds_some_rows_v1 <- redcap_read(
  redcap_uri = uri, 
  token      = beach_token, 
  records    = desired_records_v1,
  fields     = desired_fields_v1
)$data







#
#
#

# Variables
desired_fields_v1 <- c("record_id","int_study_grp","interphone_date","int_consent_date","int_consent_complete",
                       "beach_interview_phone_screen_complete","int_phone_pass_fail",
                       "int_interview_date","int_interview_complete","int_audio_length_min", # study 
                       "interphone_prepreg_bmi","interphone_age","mom3t_prepreg_bmi",
                       "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", "mompa_jog", "mompa_prenatal_exer", "mompa_swim","mompa_dance", # physical activity
                       "int_guide_education","int_guide_employmnt","int_guide_occupation",
                       "biosample_collection_date", "biosample_mom_baby", "biosample_aliquot_type",
                       "crc_specimen_barcode","biosample_tube_type","biosample_aliquot_numb",
                       "biological_specimen_collection_complete","no_wkly_encounter_int", "encounter_date_int",
                       "encounter_type_int","encounter_other_int","learn_about_study_int", "study_other_int",
                       "see_flyer_int", "flyer_other_int", "ufhealth_clinic_int","beach_interview_study_encounters_complete"
                       )

# pull data
interview <- redcap_read(
  batch_size=150L,
  redcap_uri = uri, 
  token      = interview_token, 
  fields     = desired_fields_v1
  )$data

# check data pull
str(interview)
interview[1]

# rename data
dat=interview
names(dat)
str(dat)

# format dates
dat$int_consent_date=as.Date(dat$int_consent_date, "%Y-%m-%d")
dat$interphone_date=as.Date(dat$interphone_date, "%Y-%m-%d")
dat$encounter_date_int=as.Date(dat$encounter_date_int, "%Y-%m-%d")
dat$biosample_collection_date=as.Date(dat$biosample_collection_date, "%Y-%m-%d")





# "BIS002A" "BIS004A" "BIS005A" "BIS023A" "BIS025A" "BIS032A"
# "BIS034A"

# how many have completed the interview
complete=dat %>%
  filter(int_interview_complete==1)%>%
  pull(record_id)
length(complete) # 40

# table 1. 

data(tips, package = "reshape2")
glimpse(tips)

tips %>% 
  select(tip, total_bill, sex) %>% 
  gather(key = variable, value = value, -sex) %>% 
  group_by(sex, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(sex, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(Female), unlist(Male))$p.value,
         t_value = t.test(unlist(Female), unlist(Male))$statistic)


# what is difference in interview time? p=0.6
dat %>%
  group_by(int_study_grp)%>%
  summarize(mean_audio=mean(int_audio_length_min, na.rm = TRUE))
  t.test(int_audio_length_min ~ int_study_grp, data = dat)%>%
    tidy()

dat%>%
  group_by(int_study_grp)%>%
  summarize(mean_audio=mean(int_audio_length_min, na.rm = TRUE))

# table: "interphone_age","mom3t_prepreg_bmi",
# "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", 
# "mompa_jog", "mompa_prenatal_exer", "mompa_swim","mompa_dance"


# https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories
# https://sebastiansauer.github.io/multiple-t-tests-with-dplyr/
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html 
