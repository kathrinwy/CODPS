
# Working directory -------------------------------------------------------

# Directories

directory <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/"
code      <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/CODPS/national_models/"
output    <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/"
input     <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/input/"

setwd(code)

# The source file is valid for all countries
source("0-set-up.R")   

# NIGER -------------------------------------------------------------------

  # Setup and load data -----------------------------------------------------

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

setwd(code)


# BURKINA FASO ------------------------------------------------------------

  # Setup and load data -----------------------------------------------------

source("1-read-input-data-BFA.R")                       # Read IPUMS data

  # Projections (tfr, e0 and population adm0) -------------------------------

source("2-bayespop-projections-tfr-BFA.R")              # using converged tfr projections
source("3-bayespop-projections-e0-BFA.R")               # using converged e0  projections
source("4-probabilistic-population-projections-BFA.R")  

  # Subnational projections -------------------------------------------------

source("5-tfr-subnational-projections-BFA.R")           # using tfr file for Niger DHS 2012 and DHS 2006
source("6-population-projections-subnational-BFA.R")    

  # Export ------------------------------------------------------------------

source("7-export-e0-BFA.R")                        

  # Data visualizations -----------------------------------------------------

source("8-visualizations-BFA.R") 






