##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 23, 2019 
# IRB:
# Description: Data management for CTSI/CRC billing and services data 
#              extracted from RedCap. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_STUDY\RedCap\ALL_DATA
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(tidyr)
library(dplyr)
library(ggplot2)
library(keyringr)
library(redcapAPI)
library(REDCapR)
library(lubridate)

# **************************************************************************** #
# ***************  Pull data from redcap with api                                              
# **************************************************************************** # 

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "beach_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
print(beach_token)

# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)

# crc variables
desired_fields_v1=c("test_id","redcap_event_name","redcap_repeat_instrument",
            "redcap_repeat_instance","crc_date_of_service","crc_service",
            "crc_unit_cost", "crc_billable_quant", "crc_amount_due")  

# events to retain
exportEvents(rcon)
events_to_retain  <- c("third_trimester_arm_1", "two_week_arm_1", "two_month_arm_1", "twelve_month_arm_1")

# list of instruments
exportInstruments(rcon)

# list of events
exportEvents(rcon)

# list records
exportRecords(rcon)

# export field names
exportFieldNames(rcon)

# consented records
consent.records.v1=c("BLS001A","BLS002A","BLS003A","BLS006A",
                  "BLS007A","BLS008A","BLS010A","BLS011A",
                  "BLS012A","BLS013A","BLS014A","BLS015A",
                  "BLS016A","BLS019A","BLS020A","BLS023A",
                  "BLS025A","BLS026A","BLS027A","BLS028A",
                  "BLS029A","BLS030A","BLS032A","BLS033A",
                  "BLS034A","BLS035A","BLS036A","BLS037A",
                  "BLS038A","BLS039A","BLS040A","BLS041A",
                  "BLS043A","BLS044A","BLS045A","BLS048A",
                  "BLS049A","BLS050A","BLS051A","BLS052A",
                  "BLS053A", "BLS054A","BLS055A","BLS056A",
                  "BLS057A","BLS058A","BLS059A","BLS060A")

# pull data
ds_some_rows_v1 <- redcap_read(
  batch_size=300,
  records= consent.records.v1,
  redcap_uri = uri, 
  token      = beach_token, 
  fields     = desired_fields_v1
)$data

# look at data
dat=ds_some_rows_v1
head(dat); str(dat); names(dat)

# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# months formatting
str(dat)
dat$crc_date_of_service=as.Date(dat$crc_date_of_service, "%Y-%m-%d")
dat$month=month(dat$crc_date_of_service)
dat$year=year(dat$crc_date_of_service)
dat$month_yr=paste0(dat$month,"_",dat$year)
dat$month_yr=as.factor(dat$month_yr)
levels(dat$month_yr)

# set the order of month_yr
df1<- dat %>% 
  mutate(month_yr = factor(month_yr, 
                                    levels = c("6_2017","7_2017","8_2017","9_2017", 
                                               "10_2017","11_2017","12_2017",
                                               "1_2018","2_2018","3_2018",
                                               "4_2018","5_2018","6_2018",
                                               "7_2018","8_2018","9_2018",
                                               "10_2018","11_2018","12_2018")))
levels(df1$month_yr)

# what is the ordering of redcap_events
df1$redcap_event_name=as.factor(df1$redcap_event_name)
levels(df1$redcap_event_name)

# set the order of redcap_events
df2 <- df1 %>% 
  mutate(redcap_event_name = factor(redcap_event_name, 
                                    levels = c("baseline_arm_1",
                                               "third_trimester_arm_1", 
                                               "two_week_arm_1", 
                                               "two_month_arm_1",
                                               "six_month_arm_1",
                                               "twelve_month_arm_1")))
# check odering of levels
levels(df2$redcap_event_name)
dim(df2) # 675 9

# drop NA observations
dat.s=df2 %>%
  na.omit() %>%
  group_by(test_id, redcap_event_name) %>%
  arrange(crc_date_of_service) 
dim(dat.s) # 479 9

# how much per visit/participant?
test=dat.s %>%
  group_by(test_id, redcap_event_name, month_yr) %>%
  summarize(count=n_distinct(crc_service),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))
test %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
    mean(bill_sum),
            min(bill_sum),
            max(bill_sum))

# how much per visit?
test %>%
  group_by(redcap_event_name) %>%

            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))





# My working code 

library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in the RedCap data excel sheet
df <- read.table("TheBreastfeedingAndE-CRCBilling_DATA_2019-04-15_1133.txt", header = TRUE)



# drop NA observations
dat.s=df %>%
  na.omit() %>%
  group_by(test_id, redcap_event_name) %>%
  arrange(crc_date_of_service) 

# how much per visit/participant?
test=dat.s %>%
  group_by(test_id, redcap_event_name) %>%
  summarize(count=n_distinct(crc_service),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))

test %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
            mean(bill_sum),
            min(bill_sum),
            max(bill_sum))



library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in the RedCap data excel sheet
df <- read.table("TheBreastfeedingAndE-CRCBilling_DATA_2019-04-15_1133.txt", header = TRUE)



# drop NA observations
dat.s=df %>%
  na.omit() %>%
  group_by(test_id, redcap_event_name) %>%
  arrange(crc_date_of_service) 

# how much per visit/participant?
test=dat.s %>%
  group_by(test_id, redcap_event_name) %>%
  summarize(count=n_distinct(crc_service),
            bill_mean=mean(crc_amount_due, na.rm=T),
            bill_sum=sum(crc_amount_due))

test %>%
  group_by(redcap_event_name) %>%
  summarize(count=n_distinct(test_id),
            mean(bill_sum),
            min(bill_sum),
            max(bill_sum))


#arranging the data frame by date of service and organizing by month and year
test1 <- dat.s%>%
  mutate(crc_date_of_service = as.Date(crc_date_of_service, format = "%m/%d/%Y"))%>%
  mutate(crc_date_of_service = strftime(crc_date_of_service, "%y-%m"))%>%
  arrange(crc_date_of_service)


#setting the theme for the graphs   
theme_set(theme_classic())

# distribution of visits each month(histogram) vs cost
h <- ggplot(test1, aes(crc_date_of_service, crc_amount_due)) + scale_fill_brewer(palette = "Spectral")

            bill_mean=mean(bill_sum, na.rm=T),
            bill_sum=sum(bill_sum))

#ggplot2 histgram bar graph
theme_set(theme_classic())
dat.s$crc_service=as.factor(dat.s$crc_service)

g <- ggplot(dat.s, aes(month_yr))
g +  geom_bar(aes(fill=crc_service))


# distribution of visits each month(histogram)
h <- ggplot(dat.s, aes(month_yr, bill_sum)) + scale_fill_brewer(palette = "Spectral")

h + geom_histogram(aes(fill=redcap_event_name), stat = "Identity",
                   bins=24,
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Clinical visits per Month cost breakdown", 
       subtitle="from july of 2017-January 2019") +
  theme(axis.text.x = element_text(angle=70, vjust =.6))

# cost of each event as of now(bar chart)
b <- ggplot(test1,aes(redcap_event_name,crc_amount_due)) 
b + geom_bar(stat="identity", width=.5, fill="orange3",
             size =.1)+ 
  labs(title="cost total of each redcap event",
       subtitle="costs are done through billing sum of previous data frame") +
  theme(axis.text.x = element_text(angle=70, vjust =.6))




# a box plot of the clincal visits by cost 
bp <- ggplot(test2, aes(redcap_event_name, crc_amount_due))
bp + geom_boxplot(varwidth=T, fill="red2") + 
  labs(title="Box plot", 
       subtitle="Cost grouped by Clinical Visits",
       caption="Source: test2",
       x="clinical visit",
       y="billing amount") + 
  theme(axis.text.x = element_text(angle=70, vjust =.6))



