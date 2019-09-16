
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: load subnational population data, DHS/MICS fertilty data

# Date created: 11 September 2019
# Last updated: 11 September 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Last census in Burkina Faso took place in 2006

# Read in data: INS projections -------------------------------------------

setwd(input)

# Adm0 population growth from WPP -----------------------------------------

url1                    <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F02_POPULATION_GROWTH_RATE.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
growth                  <- read_excel(tf, 1L, skip = 16)

growth <- growth %>%
  dplyr::select(c("Region, subregion, country or area *", "2005-2010", "2010-2015"))%>%
  filter(`Region, subregion, country or area *` == "Burkina Faso")

## Section: Read in and prepare IPUMS Census microdata
BFA.census.2006 <- "ipumsi_00012.dat"
census.ddi      <- read_ipums_ddi("ipumsi_00012.xml")
census.data     <- read_ipums_micro(census.ddi, verbose = FALSE)
unique(census.data$DHS_IPUMSI_BF)

# DHS_IPUMSI_CM		DHS-IPUMS-I BFA regions,  1996-2010  [consistent boundaries, GIS]
# 01	Boucle de Mouhoun
# 02	Cascades
# 03	Centre including Ouagadougou
# 04	Centre-Est
# 05	Centre-Nord
# 06	Centre-Ouest
# 07	Centre-Sud
# 08	Est
# 09	Hauts Basins
# 10	Nord
# 11	Plateau Central
# 12	Sahel
# 13	Sud-Ouest

## Construct age/sex and age distributions, both by 1-yr and 5-yr
##adm0
male.adm0.census1 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm0.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm0.census1 <- census.data %>% 
  group_by(AGE) %>%  
  dplyr::summarize(sum_age = sum(PERWT))


male.adm0.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm0.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm0.census5 <- census.data %>% 
  group_by(AGE2) %>%  
  dplyr::summarize(sum_age = sum(PERWT))


##adm1
male.adm1.census1 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))
female.adm1.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm1.census1 <- census.data %>% 
  group_by(AGE, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))

###########################################################################################
# Smoothing
###########################################################################################

male.adm1.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm1.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm1.census5 <- census.data %>% 
  group_by(AGE2, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  		

## convert age2 to age.cat
## convert region.no to region-name 
male.adm1.census5 <-
  mutate(male.adm1.census5, 
         agegroup = ifelse(AGE2 == 1, "0-4",
                           ifelse(AGE2 == 2, "5-9",
                                  ifelse(AGE2 == 3, "10-14",
                                         ifelse(AGE2 == 4, "15-19",
                                                ifelse(AGE2 ==12, "20-24",
                                                       ifelse(AGE2 == 13, "25-29",
                                                              ifelse(AGE2 == 14, "30-34",
                                                                     ifelse(AGE2 == 15, "35-39",
                                                                            ifelse(AGE2 == 16, "40-44",
                                                                                   ifelse(AGE2 == 17, "45-49",
                                                                                          ifelse(AGE2 == 18, "50-54",
                                                                                                 ifelse(AGE2 == 19, "55-59",
                                                                                                        ifelse(AGE2 == 20, "60-64",
                                                                                                               ifelse(AGE2 == 21, "65-69",
                                                                                                                      ifelse(AGE2 ==22, "70-74",
                                                                                                                             ifelse(AGE2 == 23, "75-79",
                                                                                                                                    ifelse(AGE2 ==24, "80+", NA))))))))))))))))))

male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(DHS_IPUMSI_BF == 1, "Boucle de Mouhoun", 
                ifelse(DHS_IPUMSI_BF == 2, "Cascades",
                ifelse(DHS_IPUMSI_BF == 3, "Centre including Ouagadougou",
                ifelse(DHS_IPUMSI_BF == 4, "Centre-Est",
                ifelse(DHS_IPUMSI_BF == 5, "Centre-Nord",
                ifelse(DHS_IPUMSI_BF == 6, "Centre-Ouest",
                ifelse(DHS_IPUMSI_BF == 7, "Centre-Sud",
                ifelse(DHS_IPUMSI_BF == 8, "Est",
                ifelse(DHS_IPUMSI_BF == 9, "Hauts Basins",
                ifelse(DHS_IPUMSI_BF ==10, "Nord",
                ifelse(DHS_IPUMSI_BF ==11, "Plateau Central",
                ifelse(DHS_IPUMSI_BF ==12, "Sahel",
                ifelse(DHS_IPUMSI_BF ==13, "Sud-Ouest", NA))))))))))))))
     
female.adm1.census5 <-
  mutate(female.adm1.census5, 
         agegroup = ifelse(AGE2 == 1, "0-4",
                           ifelse(AGE2 == 2, "5-9",
                                  ifelse(AGE2 == 3, "10-14",
                                         ifelse(AGE2 == 4, "15-19",
                                                ifelse(AGE2 ==12, "20-24",
                                                       ifelse(AGE2 == 13, "25-29",
                                                              ifelse(AGE2 == 14, "30-34",
                                                                     ifelse(AGE2 == 15, "35-39",
                                                                            ifelse(AGE2 == 16, "40-44",
                                                                                   ifelse(AGE2 == 17, "45-49",
                                                                                          ifelse(AGE2 == 18, "50-54",
                                                                                                 ifelse(AGE2 == 19, "55-59",
                                                                                                        ifelse(AGE2 == 20, "60-64",
                                                                                                               ifelse(AGE2 == 21, "65-69",
                                                                                                                      ifelse(AGE2 ==22, "70-74",
                                                                                                                             ifelse(AGE2 == 23, "75-79",
                                                                                                                                    ifelse(AGE2 ==24, "80+", NA))))))))))))))))))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(DHS_IPUMSI_BF == 1, "Boucle de Mouhoun", 
                       ifelse(DHS_IPUMSI_BF == 2, "Cascades",
                              ifelse(DHS_IPUMSI_BF == 3, "Centre including Ouagadougou",
                                     ifelse(DHS_IPUMSI_BF == 4, "Centre-Est",
                                            ifelse(DHS_IPUMSI_BF == 5, "Centre-Nord",
                                                   ifelse(DHS_IPUMSI_BF == 6, "Centre-Ouest",
                                                          ifelse(DHS_IPUMSI_BF == 7, "Centre-Sud",
                                                                 ifelse(DHS_IPUMSI_BF == 8, "Est",
                                                                        ifelse(DHS_IPUMSI_BF == 9, "Hauts Basins",
                                                                               ifelse(DHS_IPUMSI_BF ==10, "Nord",
                                                                                      ifelse(DHS_IPUMSI_BF ==11, "Plateau Central",
                                                                                             ifelse(DHS_IPUMSI_BF ==12, "Sahel",
                                                                                                    ifelse(DHS_IPUMSI_BF ==13, "Sud-Ouest", NA))))))))))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_BF == 1, 46, 
                    ifelse(DHS_IPUMSI_BF == 2, 47,
                    ifelse(DHS_IPUMSI_BF == 3, 13,
                    ifelse(DHS_IPUMSI_BF == 4, 48,
                    ifelse(DHS_IPUMSI_BF == 5, 49,
                    ifelse(DHS_IPUMSI_BF == 6, 50,
                    ifelse(DHS_IPUMSI_BF == 7, 51,
                    ifelse(DHS_IPUMSI_BF == 8, 52,
                    ifelse(DHS_IPUMSI_BF == 9, 53,
                    ifelse(DHS_IPUMSI_BF ==10, 54,
                           ifelse(DHS_IPUMSI_BF ==11, 55,
                                  ifelse(DHS_IPUMSI_BF == 12, 56,
                                         ifelse(DHS_IPUMSI_BF ==13, 57,NA))))))))))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_BF == 1, 46, 
                           ifelse(DHS_IPUMSI_BF == 2, 47,
                                  ifelse(DHS_IPUMSI_BF == 3, 13,
                                         ifelse(DHS_IPUMSI_BF == 4, 48,
                                                ifelse(DHS_IPUMSI_BF == 5, 49,
                                                       ifelse(DHS_IPUMSI_BF == 6, 50,
                                                              ifelse(DHS_IPUMSI_BF == 7, 51,
                                                                     ifelse(DHS_IPUMSI_BF == 8, 52,
                                                                            ifelse(DHS_IPUMSI_BF == 9, 53,
                                                                                   ifelse(DHS_IPUMSI_BF ==10, 54,
                                                                                          ifelse(DHS_IPUMSI_BF ==11, 55,
                                                                                                 ifelse(DHS_IPUMSI_BF == 12, 56,
                                                                                                        ifelse(DHS_IPUMSI_BF ==13, 57,NA))))))))))))))

# Save file ---------------------------------------------------------------

female.pop.2005 <- female.adm1.census5 %>%
  filter(!is.na(agegroup))
         
male.pop.2005 <-male.adm1.census5 %>% 
     filter(!is.na(agegroup))

BFApopF <- female.pop.2005[,c(6,5,4,3)]
BFApopM <-   male.pop.2005[,c(6,5,4,3)]

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2005-2010
BFApopM$'2007' <- BFApopM$'sum_age'*(as.numeric(growth[1,2])/100 +1)
BFApopM$'2008' <- BFApopM$'2007'*(as.numeric(growth[1,2])/100 +1)
BFApopM$'2009' <- BFApopM$'2008'*(as.numeric(growth[1,2])/100 +1)
BFApopM$'2010' <- BFApopM$'2009'*(as.numeric(growth[1,2])/100 +1)

# use growth rate for 2010-2015
BFApopM$'2011' <- BFApopM$'2010'*(as.numeric(growth[1,3])/100 +1)
BFApopM$'2012' <- BFApopM$'2011'*(as.numeric(growth[1,3])/100 +1)
BFApopM$'2013' <- BFApopM$'2012'*(as.numeric(growth[1,3])/100 +1)
BFApopM$'2014' <- BFApopM$'2013'*(as.numeric(growth[1,3])/100 +1)
BFApopM$'2015' <- BFApopM$'2014'*(as.numeric(growth[1,3])/100 +1)

# use growth rate for 2005-2010
BFApopF$'2007' <- BFApopF$'sum_age'*(as.numeric(growth[1,2])/100 +1)
BFApopF$'2008' <- BFApopF$'2007'*(as.numeric(growth[1,2])/100 +1)
BFApopF$'2009' <- BFApopF$'2008'*(as.numeric(growth[1,2])/100 +1)
BFApopF$'2010' <- BFApopF$'2009'*(as.numeric(growth[1,2])/100 +1)

# use growth rate for 2010-2015
BFApopF$'2011' <- BFApopF$'2010'*(as.numeric(growth[1,3])/100 +1)
BFApopF$'2012' <- BFApopF$'2011'*(as.numeric(growth[1,3])/100 +1)
BFApopF$'2013' <- BFApopF$'2012'*(as.numeric(growth[1,3])/100 +1)
BFApopF$'2014' <- BFApopF$'2013'*(as.numeric(growth[1,3])/100 +1)
BFApopF$'2015' <- BFApopF$'2014'*(as.numeric(growth[1,3])/100 +1)

BFApopF <- BFApopF[,c(1,2,3,13)]
BFApopM <- BFApopM[,c(1,2,3,13)]


# Export ------------------------------------------------------------------

colnames(BFApopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(BFApopM) <- c("reg_code","name","age","2015") # Why 2015?

write.csv(BFApopF, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopF.csv", row.names = TRUE)
write.csv(BFApopM, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopM.csv", row.names = TRUE)

write.table(BFApopF, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopF.txt", sep = "\t", row.names = FALSE)
write.table(BFApopM, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopM.txt", sep = "\t", row.names = FALSE)

popF0.file <- BFApopF
popM0.file <- BFApopM

BFApopF <- read_delim("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopF.txt", delim="\t")
BFApopM <- read_delim("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopM.txt", delim="\t")

setwd(output)

# Retrieve e0 trajectories ------------------------------------------------

BFAe0Ftraj <- read.csv(file = "./mye0trajs/F/ascii_trajectories.csv", header=TRUE, sep=",") %>%
  dplyr::select(-Period)
write.csv(BFAe0Ftraj, paste0("./regdata/", "BFAe0Ftraj.csv"), row.names = F)

BFAe0Mtraj <- read.csv(file = "./mye0trajs/M/ascii_trajectories.csv", header=TRUE, sep=",") %>%
  dplyr::select(-Period)
write.csv(BFAe0Mtraj, paste0("./regdata/", "BFAe0Mtraj.csv"), row.names = F)

# TFR input ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

my.regtfr.file.BFA <- "regdata/tfr.BFA.txt"
read.delim(my.regtfr.file.BFA , check.names = F)

setwd(code)


