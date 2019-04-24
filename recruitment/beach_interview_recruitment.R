##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        April 22, 2019 
# Project:     BEACH Interview
# Description: Recruitment data and analysis

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(tidyverse)
library(redcapAPI)
library(REDCapR)

# Login to Gatorlink VPN

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "interview_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
interview_token<-decrypt_dpapi_pw(credential_path)
print(interview_token)

# Create connections
rcon <- redcapConnection(url=uri, token=interview_token)

# list of instruments
exportInstruments(rcon)

# list records
# exportRecords(rcon)

# export field names
exportFieldNames(rcon)

# Variables
desired_fields_v1 <- c("record_id","int_study_grp","interphone_date","int_consent_date","int_consent_complete",
                       "int_interview_date","int_interview_complete","int_audio_length_min", # study 
                       "interphone_prepreg_bmi","interphone_age","mom3t_prepreg_bmi",
                       "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", "mompa_jog", "mompa_prenatal_exer", "mompa_swim","mompa_dance", # physical activity
                       "int_guide_education","int_guide_employmnt","int_guide_occupation",
                       "biosample_collection_date", "biosample_mom_baby", "biosample_aliquot_type",
                       "crc_specimen_barcode","biosample_tube_type","biosample_aliquot_numb",
                       "biological_specimen_collection_complete"
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

# how many consented
table(dat$int_consent_complete) # 47 

# how many have not completed interview? 
table(dat$int_interview_complete) # 7 no and # 40 yes

# who did not complete interview: confirm
did.not.complete.interview=dat%>%
  filter(int_interview_complete==0)
print(as.character(did.not.complete.interview$record_id))

# "BIS002A" "BIS004A" "BIS005A" "BIS023A" "BIS025A" "BIS032A"
# "BIS034A"

# how many have completed the interview
complete=dat %>%
  filter(int_interview_complete==1) # PRG010 need to follow-up with interview
complete$record_id

# how many donated samples?
complete=dat %>%
  filter(biological_specimen_collection_complete%in% c("0","1")) # PRG010 need to follow-up with interview
  length(unique(complete$record_id)) #39
  
# how many donated all samples?
table(complete$biosample_aliquot_type)
all.samples=complete%>%
  group_by(record_id)%>%
  mutate(sample_count=length(unique(crc_specimen_barcode)))%>%
  select(record_id,sample_count)%>%
  filter(sample_count=="5")
length(unique(all.samples$record_id)) #23 contributed all samples
dim(all.samples)

# what is difference in interview time? p=0.6
dat %>%
  group_by(int_study_grp)%>%
  summarize(mean_audio=mean(int_audio_length_min, na.rm = TRUE))
  t.test(int_audio_length_min ~ int_study_grp, data = dat)
  
  # Welch Two Sample t-test
  # data:  int_audio_length_min by int_study_grp
  # t = -0.48569, df = 34.533, p-value = 0.6303
  # alternative hypothesis: true difference in means is not equal to 0
  # 95 percent confidence interval:
  #   -10.022766   6.154345
  # sample estimates:
  #   mean in group 1 mean in group 2 
  # 45.31579        47.25000 


