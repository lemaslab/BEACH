##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        May 08, 2019 
# IRB:
# Description: Randomly Select Participants for Coding

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

# export field names
exportFieldNames(rcon)

# Variables
desired_fields_v1 <- c("record_id","int_study_grp","interphone_date","int_consent_date","int_consent_complete",
                       "int_interview_date","int_interview_complete","int_audio_length_min", # study 
                       "nvivo_upload_date","nvivo_chunking_date","nvivo_coded",
                       "nvivo_code_team_member")

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

# how many consented & interviewed
table(dat$int_consent_complete)    # 47 consented
table(dat$int_interview_complete)  # 40 interviewed
table(dat$nvivo_coded)             # 7 coded

# coded=c("BIS001A","BIS003A","BIS006A","PRG001","PRG002","PRG003","BIS007A")

coded=dat %>%
  filter(nvivo_coded==1)%>%
  select(record_id)%>%
  pull()

code.now=dat%>%
  filter(int_interview_complete==1)%>%
  filter(!record_id %in% coded)%>%
  select(record_id)%>%
  pull()
length(code.now) # 33 remaining 

# select randomized participants
t1=sample(code.now, size=11, replace=F);t1

codes.left=intersect(code.now, t1)

https://carleshf87.wordpress.com/2013/11/21/outersection-in-r/

austen=sample(code.now, size=3, replace=F) 

 



