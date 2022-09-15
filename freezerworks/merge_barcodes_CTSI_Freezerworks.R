##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        February 08, 2019 
# IRB:
# Description: Data management to merge freezerworks and redcap data.  
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap
# Obj: merged datasets from CRC and freezerworks for clinic visit information.

# need to check dates of visit between redcap and freezerworks. how much they disagree.


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readr)
library(dplyr)
library(tidyr)

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\merge\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\merge\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\FREEZERWORKS\\BEACH_Study\\merge\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# Data file names
data.file.name.freezerworks="BEACH_Freezer_Export_13Feb19.txt";data.file.name.freezerworks
data.file.name.redcap="RedCap_Freezerworks_Link_noPHI_1158Lemas_13Feb19.txt";data.file.name.redcap

# **************************************************************************** #
# ***************  Import Data                                              
# **************************************************************************** # 

#Read RedCap Data
#----------------
redcap.file.path=paste0(data.dir,"",data.file.name.redcap);redcap.file.path
redcap<- read_tsv(redcap.file.path);

# look at data
head(redcap); str(redcap); names(redcap)
length(redcap$Participant_ID)               # 1217
length(unique(redcap$crc_specimen_barcode)) # 1181
length(unique(redcap$Participant_ID))       # 68

# format for merge
df.r=redcap%>%
  select(Participant_ID, crc_specimen_number, crc_specimen_barcode, clinic_visit_date.r, clinic_visit)

#Read fREEZERWORKS Data
#----------------
freezer.file.path=paste0(data.dir,"",data.file.name.freezerworks);freezer.file.path
freezer<- read_tsv(freezer.file.path);

# look at data
head(freezer); str(freezer); names(freezer)
length(freezer$Participant_ID)               # 1860
length(unique(freezer$crc_specimen_barcode)) # 1860
length(unique(freezer$Participant_ID))       #83

# format for merge
df.f=freezer%>%
  select(Participant_ID, crc_specimen_number, crc_specimen_barcode, "Globally Unique Aliquot ID", clinic_visit_date, clinic_visit)%>%
  rename(GUAliquotID="Globally Unique Aliquot ID")

# **************************************************************************** #
# ***************  merging data sets
# **************************************************************************** # 

# freezerworked MERGED into Redcap. Size of data set will be redcap # rows

merge.r=left_join(df.r, df.f, by=c("Participant_ID","crc_specimen_barcode"))
length(unique(merge.r$crc_specimen_barcode)) # 1181
length(unique(merge.r$Participant_ID))       # 68
head(merge.r)
names(merge.r)
str(merge.r)

# minor format 
merge.r$clinic_visit_date.r=as.Date(merge.r$clinic_visit_date.r, "%m/%d/%Y")
merge.r$clinic_visit_date=as.Date(merge.r$clinic_visit_date, "%m/%d/%Y")

# format variables
merge.r$clinic_visit.x=as.factor(merge.r$clinic_visit.x)
merge.r$clinic_visit.y=as.factor(merge.r$clinic_visit.y)

# check levels
levels(merge.r$clinic_visit.x)
levels(merge.r$clinic_visit.y)

# recode/reorder levels for freezerworks
# clinic_visit.r
df <- merge.r %>%
  mutate(clinic_visit.x = recode(clinic_visit.x, 
                          '3rd Trimester' = "3rd_trimester",
                          '2-week' = "2_week",
                          '2-month' = "2_months",
                          '12-month'="12_months")) %>%
  mutate(clinic_visit.x = factor(clinic_visit.x, levels = c("3rd_trimester","2_week","2_months","12_months"))) %>%
  mutate(clinic_visit.y = factor(clinic_visit.y, levels = c("3rd_trimester","2_week","2_months","12_months")))

# check levels after recorde/reorder
levels(df$clinic_visit.x)
levels(df$clinic_visit.y)

# **************************************************************************** #
# ***************  check agreement between data sets
# **************************************************************************** # 

# date check
df$flag_date=ifelse(df$clinic_visit_date.r==df$clinic_visit_date,"MATCH","NO_MATCH")
table(df$flag_date)

# visit check
df$flag_clinic_visit=ifelse(df$clinic_visit.x==df$clinic_visit.y,"MATCH","NO_MATCH")
table(df$flag_clinic_visit)

# names(df)
# merged.file.name.redcap="merged_redcap_freezer.txt" # need to time stamp output
# merge.file.path=paste0(out.dir,"",merged.file.name.redcap="merged_redcap_freezer.txt");merge.file.path
# write.csv(df, file=merge.file.path,row.names=FALSE)

# **************************************************************************** #
# ***************  format data set for freezerworks
# **************************************************************************** #

# check 
dim(df)
length(unique(df$Participant_ID))

merge.r1=df%>%
  select(Participant_ID,crc_specimen_barcode,clinic_visit_date.r,flag_date,clinic_visit.x,flag_clinic_visit) %>%
  rename(clinic_visit_date=clinic_visit_date.r,clinic_visit=clinic_visit.x) %>% 
  drop_na(Participant_ID, crc_specimen_barcode)  # drop rows with no part_id and barcode. 
names(merge.r1)
dim(merge.r1)
length(unique(merge.r1$Participant_ID))

# check levels
levels(merge.r1$clinic_visit)

# format for date for freezerworks
merge.r1$clinic_visit_date=format(merge.r1$clinic_visit_date, "%m/%d/%Y")

# **************************************************************************** #
# ***************  Export data set
# **************************************************************************** #

names(merge.r1)

# final data
merge.final=merge.r1

# export data (need to be tab delimited for freezerworks)  # change needed
merged.file.name.redcap="merged_redcap_freezer.txt"
merge.file.path=paste0(out.dir,"",merged.file.name.redcap="merged_redcap_freezer.txt");merge.file.path
write.csv(merge.final, file=merge.file.path,row.names=FALSE)

