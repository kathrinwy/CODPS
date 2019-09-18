
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: master file

# Date created: 11 September 2019
# Last updated: 11 September 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Notes -------------------------------------------------------------------

# Set up and national models have to run before subnational models 
# codes containaing country specific ISO codes (e. g. BFA for Burkina Faso) are specific to that country

# Directories

directory <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/"
code      <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/CODPS/national_models/"
output    <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/"
input     <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/input/"

setwd(code)

source("0-set-up.R")   

# National models ---------------------------------------------------------
source("1-bayespop-projections.R")                          # using converged tfr/e0 projections and retriev pop projections

# NIGER -------------------------------------------------------------------

country <- "Niger"

source("2-read-input-data-NER.R")                           # Read INS data
source("3-tfr-subnational-projections-NER.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational-NER.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-NER.R")                      # Not coded yet

# BURKINA FASO ------------------------------------------------------------

country <- "Burkina Faso"

source("2-read-input-data-BFA.R")                           # Read IPUMS data, Census 2006
source("3-tfr-subnational-projections-BFA.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational-BFA.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BFA.R")



# Bangladesh --------------------------------------------------------------

country <- "Bangladesh"

source("2-read-input-data-BGD.R")                           # Read IPUMS data, Census 2006
source("3-tfr-subnational-projections-BGD.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational-BGD.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BGD.R")




