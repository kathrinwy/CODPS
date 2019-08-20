##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: load required packages,configure R environment, 
##                 set working directory
## Date created: 14 August 2019
## Last updated: 16 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Install and set-up ------------------------------------------------------

options(scipen = 999)                    # Disable scientific notation in R
setwd(NER.directory)

# Install/load packages ---------------------------------------------------

## IPUMS data import:
## The package imports IPUMS fixed-width (.dat) microdata files into R 
## using the DDI codebook. It also makes it easy to apply value labels 
## and use other metadata provided by IPUMS, including the geographic boundary files.
## https://github.com/mnpopcenter/ipumsr#ipumsr

library(ipumsr)                         # Package for Census Microsamples
library(ggplot2)
library(dplyr)
library(foreign)                        # Package for handling proprietary data formats
library(devtools)
library(readxl)
library(tidyverse)
library(readr)
library(data.table)
library(readxl)
library(httr)
packageVersion("readxl")

# install_github("timriffe/Pyramid",subdir="Pyramid")
library(Pyramid)

# install_github( "ajdamico/lodown" , dependencies = TRUE )
library(lodown)                          # Package to import DHS data directly from WWW
library(rdhs)                            # Package to handle Demographic Health Surveys

# install.packages("remotes") -------------------- Step 1 of 2 to install DemoTools
# remotes::install_github("timriffe/DemoTools") -- Step 2 of 2 to install DemoTools

library(DemoTools)                       # Package for demog data quality assessment tools 
library(DDM)                             # Package for DDM, with CDMLTs
library(childhoodmortality)              # Child Mortality Package
library(DHS.rates)                       # Package for calculation of DHS rates

library(bayesTFR)
library(bayesLife)
library(bayesPop)
library(wppExplorer)
library(wpp2015)
library(wpp2017)
library(wpp2019)

setwd(NER.code)