
#' ---
#' title: "Merge CTSI barcodes with Freezerworks"
#' author: "Dominick Lemas"
#' project: "BEACH Study"
#' date: "July 11, 2019"
#' ---


# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

work.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\");work.dir
out.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\");work.dir

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
# ***************                     READ Data                                              
# **************************************************************************** # 

# Read CTSI Barcodes Data:
#------------------------
ctsi.file.name="ctsi_barcodes_updated.csv";ctsi.file.name
ctsi.file.path=paste0(work.dir,"export_ctsi_barcodes\\",ctsi.file.name);ctsi.file.path
ctsi<- read_csv(ctsi.file.path);
head(ctsi); str(ctsi); names(ctsi)


# Read Freezerworks Barcodes Data:
#--------------------------------
freezer.file.name="BEACH_Freezer_Export_13Feb19.txt";freezer.file.name
freezer.file.path=paste0(work.dir,"export_freezerworks_barcodes\\",freezer.file.name);freezer.file.path
freezer<- read_tsv(freezer.file.path);
head(freezer); str(freezer); names(freezer)


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
  ungroup()%>%
  select(Participant_ID,Clinic.visit.date,Mom_Baby,Aliquot.Type,crc_specimen_barcode,
         tube.type,Aliquot.Number) %>%
  rename(record_id=Participant_ID, 
         biosample_collection_date=Clinic.visit.date,
         biosample_mom_baby=Mom_Baby,
         biosample_aliquot_type=Aliquot.Type,
         crc_specimen_barcode=crc_specimen_barcode,
         biosample_tube_type=tube.type,
         biosample_aliquot_numb=Aliquot.Number)%>%
  mutate(redcap_repeat_instrument="biological_specimen_collection")%>%
  arrange(record_id,biosample_collection_date,biosample_aliquot_type)%>%
  mutate(redcap_repeat_instance=row_number())%>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,everything())%>%
  mutate(biosample_collection_date=format(biosample_collection_date, "%m/%d/%Y"))%>%
  ungroup()%>%
  mutate(biosample_mom_baby=recode(biosample_mom_baby,
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
table(df1$biosample_tube_type)

# # export test data: BIS003A
# redcap.bis003=df1%>%
#   filter(record_id=="BIS003A")%>%
# write_csv(path =paste0(work.dir,"redcap.bis003.csv",na = ""))

# export data: ALL
redcap.import=df1%>%
  #filter(record_id=="BIS003A")%>%
  write_csv(path =paste0(work.dir,"redcap.freezerworks.import.csv",na = ""))


 


