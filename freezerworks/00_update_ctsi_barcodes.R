##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        July 13, 2019 
# IRB:

# Description: Update CTSI barcode file that links sample visit to sample number  

# Data: ~\FREEZERWORKS\BEACH_Study\export_ctsi_barcodes\

# Obj: merged CRC lab file with new samples collected.

# Status: operational, need to change study visit --> clinic visit for freezerworks

# Notes: need to check infant blood cards in merge. Freezerworks should have 
# more information on the sample type. 

# 

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Directory Locations
work.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\export_ctsi_barcodes\\");work.dir
out.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\export_ctsi_barcodes\\");work.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                     READ Data                                              
# **************************************************************************** # 

# NEW CTSI Barcode File:
data.file.name.ctsi.update="noPHI_1158Lemas_11June19.xlsx";data.file.name.ctsi.update
ctsi.new.file.path=paste0(work.dir,"raw_data\\",data.file.name.ctsi.update);ctsi.new.file.path
ctsi_new<- read_xlsx(ctsi.new.file.path, skip = 6) %>%
  rename(crc_specimen_barcode=`Specimen Bar Code`,                    # rename variables
         crc_specimen_number=`Specimen No.`,
         Participant_ID=`Sequence Number`,
         clinic_visit_date=`Collection Date`,
         specimen_subtype_01=`Specimen Type`,
         specimen_subtype_02=`Timepoint Label`
         ) %>%
  mutate(aliquot_type=ifelse(specimen_subtype_01 !="Unknown",      # variable with tissue type 
                              specimen_subtype_01, 
                              specimen_subtype_02)) %>%
  select(-c(specimen_subtype_01,specimen_subtype_02)) %>%           # drop tmp variables
    mutate(ctsi_followup=NA,
           clinic_visit=NA) %>%
  select(crc_specimen_barcode, crc_specimen_number, Participant_ID, clinic_visit_date,    
         clinic_visit, ctsi_followup, aliquot_type);names(ctsi_new)


# OLD MODIFIED CTSI Barcode File:
data.file.name.ctsi.old="noPHI_1158Lemas_06Sept18_LCedits.xlsx";data.file.name.ctsi.old
ctsi.old.file.path=paste0(work.dir,data.file.name.ctsi.old);ctsi.old.file.path
ctsi_old<- read_xlsx(ctsi.old.file.path, skip = 6) %>%
  rename(crc_specimen_barcode=`Specimen Bar Code`, 
         crc_specimen_number=`Specimen No.`,
         Participant_ID=`Sequence Number`,
         clinic_visit_date=`Collection Date`,
         specimen_subtype_01=`Specimen Type`,
         specimen_subtype_02=`Timepoint Label`,
         ctsi_followup=CTSI_Followup,
         clinic_visit=Study_Visit
         )%>%
  mutate(aliquot_type=ifelse(specimen_subtype_01 !="Unknown", specimen_subtype_01, specimen_subtype_02))%>%
  select(-c(specimen_subtype_01,specimen_subtype_02));names(ctsi_old)


# **************************************************************************** #
# ***************                     EXPLORE Data                                              
# **************************************************************************** # 

# NEW DATA
# how many participants
length(unique(ctsi_new$Participant_ID))  # 73

# how many samples
length(unique(ctsi_new$crc_specimen_barcode)) # 846

# OLD DATA
# how many participants
length(unique(ctsi_old$Participant_ID))  # 67

# how many samples
length(unique(ctsi_old$crc_specimen_barcode)) # 1180

# how many participants are in both files?
length(intersect(ctsi_new$Participant_ID, ctsi_old$Participant_ID))  # 45

# how many samples are in both files?
intersect(ctsi_new$crc_specimen_barcode, ctsi_old$crc_specimen_barcode)  # None!

# **************************************************************************** #
# ***************                     MERGE Data                                              
# **************************************************************************** #

names(ctsi_new)
names(ctsi_old)

ctsi_merged=bind_rows(ctsi_old, ctsi_new)
length(unique(ctsi_merged$crc_specimen_barcode)) # 2026 - checked
length(unique(ctsi_merged$Participant_ID))       # 95
head(ctsi_merged)
names(ctsi_merged)
str(ctsi_merged)

# I think this is correct. next session need to work into code formally
#----------------------
# jointdataset <- merge(ctsi_old, ctsi_new, by = 'crc_specimen_barcode', all=TRUE, suffix = c(".old", ".new"))  
# length(unique(jointdataset$crc_specimen_barcode)) # 2026 - checked                
# length(intersect(jointdataset$crc_specimen_barcode, ctsi_merged$crc_specimen_barcode))  # 2026

# **************************************************************************** #
# ***************             FORMATT Data FOR FREEZERWORKS                                             
# **************************************************************************** #

str(ctsi_merged)

# dates
#------
ctsi_merged$clinic_visit_date=as.Date(ctsi_merged$clinic_visit_date, "%m/%d/%Y")

# factors
#--------
# study_visit
ctsi_merged$clinic_visit=as.factor(ctsi_merged$clinic_visit)
  # check levels
    levels(ctsi_merged$clinic_visit)

  # recode/reorder levels for freezerworks
  ctsi_updated <- ctsi_merged %>%
  rename(aliquot_type_tmp=aliquot_type) %>%            # rename aliquot type to tmp. 
  mutate(clinic_visit = recode(clinic_visit, 
                          '3rd Trimester' = "3rd_trimester",
                          '2-week' = "2_week",
                          '2-month' = "2_months",
                          '12-month'="12_months")) %>%
  mutate(clinic_visit = factor(clinic_visit, levels = c("3rd_trimester","2_week","2_months","12_months"))) %>%
  drop_na(Participant_ID, crc_specimen_barcode) %>%  # drop rows with no part_id and barcode. 
  mutate(aliquot_type = recode(aliquot_type_tmp, 
                                 'Plasma' = "plasma",
                                 'Saliva' = "saliva",
                                 'Urine' = "urine",
                                 'Mucosal Scrapings'="vaginal",
                                 'Stool'="stool",
                                 'Whole Milk'="milk- whole",
                                 'Skim Milk'="milk- skim",
                                 'Milk Fat'="milk-lipid",
                                 'Skim MIlk'="milk- skim",
                                 'MIlk Fat'="milk-lipid",
                                 'Whole Blood'="blood",
                                 'Card'="blood",
                                 'WB.Card'="blood",
                                 'Spot Card'="blood",
                                 'card'="blood",
                                 'W.Milk'="milk- whole",
                                 'S. Milk'="milk- skim",
                                 'Milk fat'="milk-lipid",
                                 'Formula'="formula"))
    
  unique(ctsi_updated$aliquot_type_tmp)  # need to export and check.
  unique(ctsi_updated$aliquot_type)  # need to export and check.
  
  
# **************************************************************************** #
# ***************          FINAL CHECKS ON DATA
# **************************************************************************** #
  
# Final check levels after recorde/reorder
  levels(ctsi_updated$clinic_visit)

  length(unique(ctsi_updated$Participant_ID)) #95
  length(unique(ctsi_updated$crc_specimen_barcode)) # 2026
  
# **************************************************************************** #
# ***************  Export data set
# **************************************************************************** #

merged.file.name="ctsi_barcodes_updated_V1.csv"
merge.file.path=paste0(out.dir,merged.file.name);merge.file.path
write.csv(ctsi_updated, file=merge.file.path,row.names=FALSE)
