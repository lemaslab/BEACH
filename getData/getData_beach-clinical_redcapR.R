##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 22, 2019 
# IRB:
# Description: RedCap API system for BEACH clinical project

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(redcapAPI)
library(REDCapR)

# Get Redcap API Token
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
# 
consent.records.v2=c("2") # problem record
# pull data
ds_some_rows_v2 <- redcap_read(
  batch_size=250,
  records= consent.records.v2,
  redcap_uri = uri, 
  token      = beach_token, 
  fields     = desired_fields_v1
)$data


desired_records_v1 <- c("BLS001A")
desired_fields_v1 <- c("test_id", "redcap_event_name", "mom3t_breast_surg")

ds_some_rows_v1 <- redcap_read(
  redcap_uri = uri, 
  token      = beach_token, 
  records    = desired_records_v1,
  fields     = desired_fields_v1
)$data

