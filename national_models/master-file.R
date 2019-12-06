
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: master file

# Date created: 11 September 2019
# Last updated: 5 December 2019

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

#country <- "Niger"
#iso     <- "NER"

#source("2-read-input-data-NER.R")                           # Read INS data
#source("3-tfr-subnational-projections.R")               # pre-requisite: downloaded tfr file in regdata
#source("4-population-projections-subnational.R")    
#source("5-export-pop-data.R")                        
#source("6a-visualizations.R") 
#source("6b-visualizations-maps-NER.R")                      # Not coded yet

# BURKINA FASO ------------------------------------------------------------

country <- "Burkina Faso"
iso     <- "BFA"
p.code <- "BF"

source("2-read-input-data-BFA.R")                           # Read IPUMS data, Census 2006
source("3-tfr-subnational-projections.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BFA.R")

# Bangladesh --------------------------------------------------------------

country <- "Bangladesh"
iso     <- "BGD"
p.code  <- "BD"

source("2-read-input-data-BGD.R")                           # Read IPUMS data, Census 2006
source("3-tfr-subnational-projections.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BGD.R")

# Cameroon --------------------------------------------------------------

country <- "Cameroon"
iso     <- "CMR"
p.code  <- "CMR"

source("2-read-input-data-CMR.R")                           # Read IPUMS data, Census 2005
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-CMR.R")

# Mali --------------------------------------------------------------

country <- "Mali"
iso     <- "MLI"
p.code  <- "ML"

source("2-read-input-data-MLI.R")                           # Read IPUMS data, Census 2009
source("3-tfr-subnational-projections.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-MLI.R")

# Zambia ------------------------------------------------------------------

country <- "Zambia"
iso     <- "ZMB"
p.code <- "ZMB"

source("2-read-input-data-ZMB.R")                           # Read IPUMS data, Census 2010
source("3-tfr-subnational-projections.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-ZMB.R")

# Zimbabwe ----------------------------------------------------------------

country <- "Zimbabwe"
iso     <- "ZWE"
p.code <- "ZW"

source("2-read-input-data-ZWE.R")                           # Read IPUMS data, Census 2012
source("3-tfr-subnational-projections.R")               # pre-requisite: downloaded tfr file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-ZWE.R")

