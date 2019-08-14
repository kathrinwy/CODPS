##================================================================
## Project: COD-PS Assessment and Construction, Cameroon
## Script purpose: load required packages,configure R environment, 
##                 set working directory
## Date created: 25 November, 2018
## Last updated: 25 November, 2018
##
## Author: Romesh Silva``
## Maintainers: Romesh Silva, Kathrin Weny
##================================================================
print("+----------------------------+")
print("Now running setup.environment.R")
print("+----------------------------+")
## Section:
## -initial set-up
rm(list = ls())        ## Clear variables etc from cache
options(scipen = 999)  ## disable scientific notation in R
## Set username/user-machine 
mylaptop.name <- "souleymane"
my.username <- "romesh" 
## Set working directory
CAM.directory <- "~/Documents/UNFPA/Humanitarian/COD-PS-general/WCARO-2018/individual/CAM/"
CAM.input <- "~/Documents/UNFPA/Humanitarian/COD-PS-general/WCARO-2018/individual/CAM/input/"
CAM.output <- "~/Documents/UNFPA/Humanitarian/COD-PS-general/WCARO-2018/individual/CAM/output/"
setwd(CAM.directory)
##================================================================
## Section: 
## -install and/or load required packages
## -Source useful.functions.R

## IPUMS data import:
## The package imports IPUMS fixed-width (.dat) microdata files into R 
## using the DDI codebook. It also makes it easy to apply value labels 
## and use other metadata provided by IPUMS, including the geographic boundary files.
## https://github.com/mnpopcenter/ipumsr#ipumsr
library(ipumsr)   ##Package for Census Microsamples
library(ggplot2)
library(dplyr, 
        warn.conflicts = FALSE)
library(foreign)  ##Package for handling proprietary data formats
##install.packages("devtools")
library(devtools)
##install_github("timriffe/Pyramid",subdir="Pyramid")
library(Pyramid)
library(lodown)   ##Package to import DHS data directly from WWW
library(rdhs)     ##Package to handle Demographic Health Surveys
library(DemoTools)##Package for demog data quality assessment tools 
library(DDM)      ##Package for DDM, with CDMLTs
library(childhoodmortality)  ### Child Mortality Package
library(DHS.rates) ##Package for calculation of DHS rates

library(bayesTFR)
library(bayesLife)
library(bayesPop)
library(wppExplorer)
library(wpp2015)
library(wpp2017)

## Source helper functions
source(paste(CAM.directory,
             "src/useful.functions.R", 
             sep="")
)
##================================================================