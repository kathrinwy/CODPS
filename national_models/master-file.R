
# Working directory -------------------------------------------------------

# At work
NER.directory <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/NER"
NER.input     <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/NER/input/"
NER.output    <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/NER/output/"
NER.code      <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/NER/CODPS/NER"

# At home
# NER.directory <- "C:/Users/kathrinweny/COD-PS/NER"
# NER.input     <- "C:/Users/kathrinweny/COD-PS/NER/input/"
# NER.output    <- "C:/Users/kathrinweny/COD-PS/NER/output/"

setwd(NER.code)

# Setup and load data -----------------------------------------------------

source("0-set-up-NER.R")                                
source("1-read-input-data-NER.R")                       # Read INS data

# Projections (tfr, e0 and population adm0) -------------------------------

source("2-bayespop-projections-tfr-NER.R")              # using converged tfr projections
source("3-bayespop-projections-e0-NER.R")               # using converged e0  projections
source("4-probabilistic-population-projections-NER.R")  

# Subnational projections -------------------------------------------------

source("5-tfr-subnational-projections-NER.R")           # using tfr file for Niger DHS 2012 and DHS 2006
source("6-population-projections-subnational-NER.R")    

# Export ------------------------------------------------------------------

source("7-export-e0-NER.R")                        

# Data visualizations -----------------------------------------------------

source("8-visualizations-NER.R")                        