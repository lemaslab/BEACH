# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        December 05, 2018 
# Description: Compute dietary daily averages from NDSR output  

# **************************************************************************** #
# ***************                OVERVIEW                       
# **************************************************************************** #

# Data exported from ndsr must be formatted for downstream dietary analysis. 
# Each record generates 16 files that need to be managed. 


# Input Directory: 
data.directory.name.import="BLS053A"

# Input files: 

# Output: 
data.file.name.export_01=""

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\NDSR\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\NDSR\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\BEACH_STUDY\\NDSR\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

# library(readxl)
# install.packages(data.table)
library(data.table)
library(tidyr)
library(dplyr)
library(reshape2)

# **************************************************************************** #
# ***************                 PROCEDURES                                              
# **************************************************************************** # 

# Empty data.frame

dat=data.frame(
  test_id=as.character(),
  total_energy_kcal=as.numeric()
)

# Read Data File: 03 (NAME OF FILE)
#-------------------
data.import.file.path=paste0(data.dir,data.directory.name.import,"\\",data.directory.name.import,"03.txt");data.import.file.path
dat<- read.csv(data.import.file.path, sep="\t", header=F);

