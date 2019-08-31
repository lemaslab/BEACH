
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

# need dynamic date variable to include in file names. I have this in other code. 

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
  select(-crc_specimen_number) %>%
  rename(aliquot_type="specimen_type") %>%
  mutate(crc_specimen_barcode=as.numeric(crc_specimen_barcode)) %>%
  mutate(crc_specimen_barcode=as.character(crc_specimen_barcode))

# look at data
head(ctsi); str(ctsi); names(ctsi)
length(ctsi$Participant_ID)               # 2026
length(unique(ctsi$crc_specimen_barcode)) # 2026
length(unique(ctsi$Participant_ID))       # 95
unique(ctsi$aliquot_type)

# Read Freezerworks Barcodes Data:
#--------------------------------
freezer.file.name="freezerworks_export.txt";freezer.file.name
freezer.file.path=paste0(work.dir,"export_freezerworks_barcodes\\",freezer.file.name);freezer.file.path
freezer<- read_tsv(freezer.file.path) %>%
  rename(GUAliquotID="Globally Unique Aliquot ID",
         tube_type="tube type",
         aliquot_type="Aliquot Type",
         clinic_visit_date="Clinic visit date",
         freezer_section="Freezer Section",
         project="Project",
         sample_modified_by="Sample Modified By",
         aliquot_modified_by="Aliquot Modified By",
         aliquot_number="Aliquot Number",
         mom_baby="Mom_Baby") %>%
  select(crc_specimen_barcode,Participant_ID, clinic_visit_date,	clinic_visit, GUAliquotID, tube_type, aliquot_type) %>%
  mutate(crc_specimen_barcode=as.character(crc_specimen_barcode))

# look at data
head(freezer); str(freezer); names(freezer)
length(freezer$Participant_ID)               # 2200
length(unique(freezer$crc_specimen_barcode)) # 2198
length(unique(freezer$Participant_ID))       # 99
unique(freezer$tube_type)
unique(freezer$aliquot_type)
unique(freezer$clinic_visit)

# **************************************************************************** #
# ***************  Outersect Analysi
# **************************************************************************** #

length(intersect(ctsi$crc_specimen_barcode, freezer$crc_specimen_barcode)) #1950 barcodes intersect

# function to find those that dont have merge pair. need to move to utils.r
outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}

# # this is list of barcodes that can't be merged.
outersect.followup=outersect(ctsi$crc_specimen_barcode, freezer$crc_specimen_barcode) 
length(outersect.followup)  # 323 barcodes dont have matchb/w redcap and freezerworks

# formatt
outersect.followup.df=outersect.followup %>%
  as.data.frame() %>%
  rename(crc_specimen_barcode=".")

# export data
merged.file.name="outersect_followup.csv"
merge.file.path=paste0(out.dir,"followup_freezerworks\\",merged.file.name);merge.file.path
write.csv(outersect.followup.df, file=merge.file.path,row.names=FALSE)

# **************************************************************************** #
# ***************               MERGE DATA SETS
# **************************************************************************** # 

# freezerworks will be --> MERGED into CTSI.

# checks
str(ctsi); length(unique(ctsi$crc_specimen_barcode)) # 2026
str(freezer); length(unique(freezer$crc_specimen_barcode)) # 2198
length(intersect(ctsi$crc_specimen_barcode, freezer$crc_specimen_barcode)) #1950 barcodes intersect

# merge
barcodes_merged=left_join(ctsi, freezer, by=c("crc_specimen_barcode"), suffix = c(".ctsi", ".frzwks"))
length(unique(barcodes_merged$crc_specimen_barcode)) # 2026
head(barcodes_merged)
names(barcodes_merged)
str(barcodes_merged)

# export data
merged.file.name="barcodes_merged.csv"
merge.file.path=paste0(out.dir,"followup_freezerworks\\",merged.file.name);merge.file.path
write.csv(barcodes_merged, file=merge.file.path,row.names=FALSE)


# variables that are duplicates in both "ctsi" and "freezer"
# - clinic_visit_date
# - clinic_visit
# - aliquot_type
# - Participant_ID

# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# dates 
barcodes_merged$clinic_visit_date.ctsi=as.Date(barcodes_merged$clinic_visit_date.ctsi, "%m/%d/%Y")
barcodes_merged$clinic_visit_date.frzwks=as.Date(barcodes_merged$clinic_visit_date.frzwks, "%m/%d/%Y")

# factors
barcodes_merged$clinic_visit.ctsi=as.factor(barcodes_merged$clinic_visit.ctsi)
barcodes_merged$clinic_visit.frzwks=as.factor(barcodes_merged$clinic_visit.frzwks)

barcodes_merged$aliquot_type.ctsi=as.factor(barcodes_merged$aliquot_type.ctsi)
barcodes_merged$aliquot_type.frzwks=as.factor(barcodes_merged$aliquot_type.frzwks)


# check levels
levels(barcodes_merged$clinic_visit.ctsi)
levels(barcodes_merged$clinic_visit.frzwks)

levels(barcodes_merged$aliquot_type.ctsi)
levels(barcodes_merged$aliquot_type.frzwks)


# recode/reorder levels 
df <- barcodes_merged %>%
  mutate(clinic_visit.ctsi = factor(clinic_visit.ctsi, levels = c("3rd_trimester","2_week","2_months","12_months"))) %>%
  mutate(clinic_visit.frzwks = factor(clinic_visit.frzwks, levels = c("3rd_trimester","2_week","2_months","12_months")))

# check levels after recorde/reorder
levels(df$clinic_visit.ctsi)
levels(df$clinic_visit.frzwks)


# **************************************************************************** #
# ***************       CHECK DATA AGREEMENT B/W DUPLICATE VARIABLES                                             
# **************************************************************************** # 

### STOP ###

# have you confirmed dates and participant ID's?

# ID check
df$flag_part_id=ifelse(df$Participant_ID.ctsi==df$Participant_ID.frzwks,"MATCH","NO_MATCH")
table(df$flag_part_id)
check_part_id=df %>%
  filter(flag_part_id=="NO_MATCH") %>%
  select(crc_specimen_barcode, Participant_ID.ctsi,clinic_visit.ctsi,clinic_visit_date.ctsi, 
         Participant_ID.frzwks, clinic_visit.frzwks, clinic_visit_date.frzwks ) # 102 samples

        # export data for confirmation
        #-----------------------------
        check.file.name.part_id="check_freezer_part_id_20July19.csv"  # need to add date dynamically
        check.file.path=paste0(out.dir,"check_freezerworks\\",check.file.name.part_id);check.file.path
        write.csv(check_part_id, file=check.file.path,row.names=FALSE)

        # check file, confirm label on tube in frzwrks, then reference redcap. modify CTSI file.
        

# date check
df$flag_date=ifelse(df$clinic_visit_date.ctsi==df$clinic_visit_date.frzwks,"MATCH","NO_MATCH")
table(df$flag_date)
check_dates=df %>%
  filter(flag_date=="NO_MATCH") %>%
  select(crc_specimen_barcode, Participant_ID.ctsi, clinic_visit_date.ctsi, clinic_visit_date.frzwks) # 4 dates
      # export data for confirmation
      #-----------------------------
      check.file.name.dates="check_freezer_sample_date_20July19.csv"  # need to add date dynamically
      check.file.path=paste0(out.dir,"check_freezerworks\\",check.file.name.dates);check.file.path
      write.csv(check_dates, file=check.file.path,row.names=FALSE)

      # check file, confirm date on tube in frzwrks, then reference redcap. modify CTSI file.
      
      
# visit check
df$flag_clinic_visit=ifelse(df$clinic_visit.ctsi==df$clinic_visit.frzwks,"MATCH","NO_MATCH")
table(df$flag_clinic_visit)

# after confirming the dates, part id, study visits, then create single variables for 
# each variable to import into redcap. 



# **************************************************************************** #
# ***************       FORMAT DATA FOR IMPORT TO REDCAP & FREEZERWORKS                                             
# **************************************************************************** # 

# Once above is completed, start here
names(df)

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


 


