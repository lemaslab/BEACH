
#' ---
#' title: "Format freezerworks aliquot data for import to redcap"
#' author: "Dominick Lemas"
#' project: "BEACH Interview"
#' date: "April 23, 2019"
#' ---


# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

work.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_interview\\");work.dir
data.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_interview\\");data.dir

# Set Working Directory
setwd(work.dir)
list.files()


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(tidyverse)
library(dplyr)

# **************************************************************************** #
# ***************  Interview_Freezer_Export_11Apr19.cvs                                              
# **************************************************************************** # 

# Read Data
data.file.name="Interview_Freezer_Export_11Apr19.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
freezer<- read.csv(data.file.path);

# look at data
dat=freezer
head(dat); str(dat); names(dat)

# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

names(dat)

# what is the ordering of variables
unique(as.character(dat$Aliquot.Type))
unique(as.character(dat$tube.type))
table(dat$tube.type)
table(dat$Freezer.Section)

# change dates
dat$Clinic.visit.date=as.Date(dat$Clinic.visit.date, "%m/%d/%Y")
dim(dat) # 243
length(unique(dat$Participant_ID)) #39
names(dat)
dat$Mom_Baby

# drop NA observations
dat.s=dat %>%
  group_by(Participant_ID, clinic_visit) %>%
  arrange(Clinic.visit.date) 
  dim(dat.s) # 243
  length(unique(dat.s$Participant_ID)) #39
  names(dat.s)
  
# how many tubes per participant?
part_count=dat.s %>%
  group_by(Participant_ID) %>%
  summarize(count=n_distinct(crc_specimen_barcode))
  mean(part_count$count) # 6.23 tubes
            
# how many sample types
dat.s %>%
  group_by(Aliquot.Type) %>%
  summarize(count=n_distinct(crc_specimen_barcode))

# how many tubes per sample type
dat.s %>%
  group_by(Freezer.Section) %>%
  summarize(count=n_distinct(crc_specimen_barcode))

# output data for import to redcap
redcap=dat.s %>%
  select(Participant_ID,clinic_visit,Clinic.visit.date,Mom_Baby,Aliquot.Type,crc_specimen_barcode,crc_specimen_number,
         tube.type,Aliquot.Number) %>%
  rename(test_id=Participant_ID, 
         biosample_study_visit=clinic_visit,
         biosample_collection_date=Clinic.visit.date,
         biosample_mom_baby=Mom_Baby,
         biosample_aliquot_type=Aliquot.Type,
         crc_specimen_barcode=crc_specimen_barcode,
         crc_specimen_number=crc_specimen_number,
         biosample_tube_type=tube.type,
         biosample_aliquot_numb=Aliquot.Number)%>%
  mutate(redcap_event_name=NA,
         redcap_repeat_instrument="biological_specimen_collection")%>%
  arrange(test_id,biosample_study_visit,biosample_collection_date,biosample_aliquot_type)%>%
  #group_by(test_id,biosample_study_visit) %>% 
  mutate(redcap_repeat_instance=row_number())%>%
  select(test_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,everything())%>%
  mutate(redcap_event_name=case_when(biosample_study_visit=="3rd_trimester" ~ "third_trimester_arm_1",
                                     biosample_study_visit=="2_week" ~ "two_week_arm_1",
                                     biosample_study_visit=="2_months" ~ "two_month_arm_1",
                                     biosample_study_visit=="6_months" ~ "six_month_arm_1",
                                     biosample_study_visit=="12_months" ~ "twelve_month_arm_1"))%>%
  mutate(redcap_event_name = factor(redcap_event_name, 
                               levels = c("third_trimester_arm_1", 
                                          "two_week_arm_1", 
                                          "two_month_arm_1",
                                          "six_month_arm_1",
                                          "twelve_month_arm_1")))%>%
  mutate(biosample_collection_date=format(biosample_collection_date, "%m/%d/%Y"))%>%
  ungroup()%>%
  mutate(biosample_study_visit=recode(biosample_study_visit, 
                     "3rd_trimester"="1", 
                     "2_week"="2",
                     "2_months"="3",
                     "6_months"="4",
                     "12_months"="5"),
         biosample_mom_baby=recode(biosample_mom_baby,
                      "mom"="0",
                      "baby"="1"),
         biosample_aliquot_type=recode(biosample_aliquot_type,
                      "plasma"="1",
                      "urine"="2",
                      "saliva"="3",
                      "milk- skim"="4",
                      "milk- whole"="5",
                      "milk-lipid"="6",
                      "stool"="7",
                      "vaginal"="8",
                      "blood"="9",
                      "formula"="10"),
         biosample_tube_type=recode(biosample_tube_type,
                      "2ml"="1",
                      "ez sample"="2",
                      "vaginal vial"="3",
                      "5ml"="4",
                      "tiny"="5",
                      "blood card"="6",
                      "other"="7",
                      "15ml"="8",
                      "saliva tube"="9",
                      "50ml"="10"))
# check data
names(redcap)
dim(redcap)

# replace NA with blanks
df <- sapply(redcap, as.character)
df[is.na(df)] <- " "
df1=as.data.frame(df)

# checks
unique(df1$redcap_event_name)
table(df1$redcap_event_name)
table(df1$biosample_study_visit)

# export test data: BLS001A
redcap.bls001=df1%>%
  filter(test_id=="BLS001A")%>%
write_csv(path =paste0(work.dir,"redcap.bls001.csv",na = ""))

# 

# need to create a report with data that needs to be followed up.
# output as html.

# redcap_event_name
# crc_specimen_number
# crc_specimen_barcode
# biosample_collection_date


 


