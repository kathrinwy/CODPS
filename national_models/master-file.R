
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: master file

# Date created: 11 September 2019
# Last updated: 10 December 2019

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

# Burkina Faso ------------------------------------------------------------

country <- "Burkina Faso"
iso     <- "BFA"
p.code <- "BF"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2006

source("2-read-input-data-BFA.R")                           # Read IPUMS data, Census 2006
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BFA.R")

# Bangladesh --------------------------------------------------------------

country <- "Bangladesh"
iso     <- "BGD"
p.code  <- "BD"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2011

source("2-read-input-data-BGD.R")                           # Read IPUMS data, Census 2006
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BGD.R")

# Cameroon --------------------------------------------------------------

country <- "Cameroon"
iso     <- "CMR"
p.code  <- "CMR"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2005

source("2-read-input-data-CMR.R")                           # Read IPUMS data, Census 2005
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-CMR.R")

# Mali --------------------------------------------------------------

country <- "Mali"
iso     <- "MLI"
p.code  <- "ML"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2009

source("2-read-input-data-MLI.R")                           # Read IPUMS data, Census 2009
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-MLI.R")

# Niger -------------------------------------------------------------------

country <- "Niger"
iso     <- "NER"
p.code  <- "NER"
country.code <- iso3166[iso3166$name == country, ][,4]

source("2-read-input-data-NER.R")                           # Read INS data
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-NER.R")                      

# Zambia ------------------------------------------------------------------

country <- "Zambia"
iso     <- "ZMB"
p.code <- "ZMB"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2010

source("2-read-input-data-ZMB.R")                           # Read IPUMS data, Census 2010
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-NER.R")

# Zimbabwe ----------------------------------------------------------------

country <- "Zimbabwe"
iso     <- "ZWE"
p.code <- "ZW"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2012

source("2-read-input-data-ZWE.R")                           # Read IPUMS data, Census 2012
source("3-tfr-subnational-projections.R")                   # pre-requisite: downloaded tfr and locations file in regdata
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-ZWE.R")

