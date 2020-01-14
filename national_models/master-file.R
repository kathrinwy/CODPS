
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: master file

# Date created: 11 September 2019
# Last updated: 13 January 2010

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Notes -------------------------------------------------------------------

# codes containaing country specific ISO codes (e. g. BFA for Burkina Faso) are specific to that country
# pre-requisite: downloaded tfr and locations file in regdata
# using converged tfr/e0 projections and retriev pop projections

# Directories

directory <- "C:/Users/kathrinweny/Documents/pop_est/"
code      <- "C:/Users/kathrinweny/Documents/pop_est/CODPS/national_models/"
output    <- "C:/Users/kathrinweny/Documents/pop_est/output/"
input     <- "C:/Users/kathrinweny/Documents/pop_est/input/"

directory <- "G:/My Drive/2020/2-Humanitarian/1-COD-PS/pop_est/"
code      <- "G:/My Drive/2020/2-Humanitarian/1-COD-PS/pop_est/CODPS/national_models/"
output    <- "G:/My Drive/2020/2-Humanitarian/1-COD-PS/pop_est/output/"
input     <- "G:/My Drive/2020/2-Humanitarian/1-COD-PS/pop_est/input/"

setwd(code)

source("0-set-up.R")   

# National models ---------------------------------------------------------

source("1-bayespop-projections.R")                          

# Burkina Faso ------------------------------------------------------------

country <- "Burkina.Faso"
iso     <- "BFA"
p.code <- "BF"
country.code <- iso3166[iso3166$name == "Burkina Faso", ][,4]
year <- 2006
my.regtfr.file.BFA <- "regdata/tfr.txt"

source("2-read-input-data-BFA.R")                           
source("3-tfr-subnational-projections.R")                   
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

source("2-read-input-data-BGD.R")                          
source("3-tfr-subnational-projections.R")                    
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BGD.R")

# Bangladesh --------------------------------------------------------------

country <- "Botswana"
iso     <- "BWA"
p.code  <- "BWA"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2011

source("2-read-input-data-BWA.R")                          
source("3-tfr-subnational-projections.R")                    
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-BWA.R")


# Cameroon --------------------------------------------------------------

country <- "Cameroon"
iso     <- "CMR"
p.code  <- "CMR"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2005

source("2-read-input-data-CMR.R")                           
source("3-tfr-subnational-projections.R")                   
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

source("2-read-input-data-MLI.R")                           
source("3-tfr-subnational-projections.R")                   
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-MLI.R")

# Niger -------------------------------------------------------------------

country <- "Niger"
iso     <- "NER"
p.code  <- "NER"
country.code <- iso3166[iso3166$name == country, ][,4]

source("2-read-input-data-NER.R")                           
source("3-tfr-subnational-projections.R")                   
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

source("2-read-input-data-ZMB.R")                           
source("3-tfr-subnational-projections.R")                   
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-ZMB.R")

# Zimbabwe ----------------------------------------------------------------

country <- "Zimbabwe"
iso     <- "ZWE"
p.code <- "ZW"
country.code <- iso3166[iso3166$name == country, ][,4]
year <- 2012

source("2-read-input-data-ZWE.R")                           
source("3-tfr-subnational-projections.R")                   
source("4-population-projections-subnational.R")    
source("5-export-pop-data.R")                        
source("6a-visualizations.R") 
source("6b-visualizations-maps-ZWE.R")

