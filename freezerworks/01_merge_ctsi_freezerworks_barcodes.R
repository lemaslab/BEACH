
#' ---
#' title: "Merge CTSI barcodes with Freezerworks"
#' author: "Dominick Lemas"
#' project: "BEACH Study"
#' date: "July 11, 2019"
#' ---

# need data from freezerworks with aliquot type. 

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
ctsi<- read_csv(ctsi.file.path) %>%
  rename(clinic_visit=study_visit);

# look at data
head(ctsi); str(ctsi); names(ctsi)
length(ctsi$Participant_ID)               # 2026
length(unique(ctsi$crc_specimen_barcode)) # 2026
length(unique(ctsi$Participant_ID))       # 95


# Read Freezerworks Barcodes Data:
#--------------------------------
freezer.file.name="BEACH_Freezer_Export_13Feb19.txt";freezer.file.name
freezer.file.path=paste0(work.dir,"export_freezerworks_barcodes\\",freezer.file.name);freezer.file.path
freezer<- read_tsv(freezer.file.path) %>%
  select(crc_specimen_barcode,Participant_ID, clinic_visit_date,	clinic_visit, "Globally Unique Aliquot ID") %>%
  rename(GUAliquotID="Globally Unique Aliquot ID") %>%
  mutate(crc_specimen_barcode=as.character(crc_specimen_barcode))

# look at data
head(freezer); str(freezer); names(freezer)
length(freezer$Participant_ID)               # 1860
length(unique(freezer$crc_specimen_barcode)) # 1860
length(unique(freezer$Participant_ID))       # 83


# **************************************************************************** #
# ***************               MERGE DATA SETS
# **************************************************************************** # 

str(ctsi)
str(freezer)

# freezerworks will be --> MERGED into CTSI. Size of data set will be CTSI # rows

barcodes_merged=left_join(ctsi, freezer, by=c("Participant_ID","crc_specimen_barcode"))
length(unique(barcodes_merged$crc_specimen_barcode)) # 2026
length(unique(barcodes_merged$Participant_ID))       # 95
head(barcodes_merged)
names(barcodes_merged)
str(barcodes_merged)



### Start here ##

# need to look for matches in dates and visits. 
# identify those that dont match-- output to file. 


# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# minor format 
barcodes_merged$clinic_visit_date.r=as.Date(barcodes_merged$clinic_visit_date.r, "%m/%d/%Y")
barcodes_merged$clinic_visit_date=as.Date(barcodes_merged$clinic_visit_date, "%m/%d/%Y")

# format variables
merge.r$clinic_visit.x=as.factor(merge.r$clinic_visit.x)
merge.r$clinic_visit.y=as.factor(merge.r$clinic_visit.y)

# check levels
levels(merge.r$clinic_visit.x)
levels(merge.r$clinic_visit.y)




            



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


 


