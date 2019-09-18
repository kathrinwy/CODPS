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

growth                  <- growth %>%
                           dplyr::select(c("Region, subregion, country or area *", "2005-2010", "2010-2015"))%>%
                           filter(`Region, subregion, country or area *` == "Bangladesh")

## Section: Read in and prepare IPUMS Census microdata
BDA.census.2011         <- "ipumsi_00013.dat"
census.ddi              <- read_ipums_ddi("ipumsi_00013.xml")
census.data             <- read_ipums_micro(census.ddi, verbose = FALSE)

# DHS_IPUMSI_CM		DHS-IPUMS-I BDA regions,  1996-2011  [consistent boundaries, GIS]
# 01	Barisal
# 02	Chittagong and Sylhet
# 03	Dhaka
# 04	Khulna
# 05	Rajshahi and Rangpur

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
  group_by(AGE, DHS_IPUMSI_BD) %>%  
  dplyr::summarize(sum_age = sum(PERWT))
female.adm1.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE, DHS_IPUMSI_BD) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm1.census1 <- census.data %>% 
  group_by(AGE, DHS_IPUMSI_BD) %>%  
  dplyr::summarize(sum_age = sum(PERWT))

###########################################################################################
# Smoothing
###########################################################################################

male.adm1.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2, DHS_IPUMSI_BD) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm1.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2, DHS_IPUMSI_BD) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm1.census5 <- census.data %>% 
  group_by(AGE2, DHS_IPUMSI_BD) %>%  
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
         name = ifelse(DHS_IPUMSI_BD == 1, "Barisal", 
                ifelse(DHS_IPUMSI_BD == 2, "Chittagong and Sylhet",
                ifelse(DHS_IPUMSI_BD == 3, "Dhaka",
                ifelse(DHS_IPUMSI_BD == 4, "Khulna",
                ifelse(DHS_IPUMSI_BD == 5, "Rajshahi and Rangpur", NA))))))
     
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
         name = ifelse(DHS_IPUMSI_BD == 1, "Barisal", 
                ifelse(DHS_IPUMSI_BD == 2, "Chittagong and Sylhet",
                ifelse(DHS_IPUMSI_BD == 3, "Dhaka",
                ifelse(DHS_IPUMSI_BD == 4, "Khulna",
                ifelse(DHS_IPUMSI_BD == 5, "Rajshahi and Rangpur", NA))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_BD == 1, "BD46", 
                    ifelse(DHS_IPUMSI_BD == 2, "BD49",
                    ifelse(DHS_IPUMSI_BD == 3, "BD47",
                    ifelse(DHS_IPUMSI_BD == 4, "BD48",
                    ifelse(DHS_IPUMSI_BD == 5, "BD51",NA))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_BD == 1, "BD46", 
                    ifelse(DHS_IPUMSI_BD == 2, "BD49",
                    ifelse(DHS_IPUMSI_BD == 3, "BD47",
                    ifelse(DHS_IPUMSI_BD == 4, "BD48",
                    ifelse(DHS_IPUMSI_BD == 5, "BD51",NA))))))

# Save file ---------------------------------------------------------------

female.pop.2005 <- female.adm1.census5 %>%
  filter(!is.na(agegroup))
         
male.pop.2005 <-male.adm1.census5 %>% 
     filter(!is.na(agegroup))

BDApopF <- female.pop.2005[,c(6,5,4,3)]
BDApopM <-   male.pop.2005[,c(6,5,4,3)]

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2010-2015
BDApopM$'2012' <- BDApopM$'sum_age'*(as.numeric(growth[1,3])/100 +1)
BDApopM$'2013' <- BDApopM$'2012'*(as.numeric(growth[1,3])/100 +1)
BDApopM$'2014' <- BDApopM$'2013'*(as.numeric(growth[1,3])/100 +1)
BDApopM$'2015' <- BDApopM$'2014'*(as.numeric(growth[1,3])/100 +1)


# use growth rate for 2010-2015
BDApopF$'2012' <- BDApopF$'sum_age'*(as.numeric(growth[1,3])/100 +1)
BDApopF$'2013' <- BDApopF$'2012'*(as.numeric(growth[1,3])/100 +1)
BDApopF$'2014' <- BDApopF$'2013'*(as.numeric(growth[1,3])/100 +1)
BDApopF$'2015' <- BDApopF$'2014'*(as.numeric(growth[1,3])/100 +1)

BDApopF <- BDApopF[,c(1,2,3,8)]
BDApopM <- BDApopM[,c(1,2,3,8)]


# Export ------------------------------------------------------------------

colnames(BDApopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(BDApopM) <- c("reg_code","name","age","2015") # Why 2015?

write.csv(BDApopF, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BDApopF.csv", row.names = TRUE)
write.csv(BDApopM, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BDApopM.csv", row.names = TRUE)

write.table(BDApopF, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BDApopF.txt", sep = "\t", row.names = FALSE)
write.table(BDApopM, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BDApopM.txt", sep = "\t", row.names = FALSE)

popF0.file <- BDApopF
popM0.file <- BDApopM

BDApopF <- read_delim("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BDApopF.txt", delim="\t")
BDApopM <- read_delim("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BDApopM.txt", delim="\t")

setwd(output)

# Retrieve e0 trajectories ------------------------------------------------

BDAe0Ftraj <- read.csv(file = "./mye0trajs/F/ascii_trajectories.csv", header=TRUE, sep=",") %>%
  dplyr::select(-Period)
write.csv(BDAe0Ftraj, paste0("./regdata/", "BDAe0Ftraj.csv"), row.names = F)

BDAe0Mtraj <- read.csv(file = "./mye0trajs/M/ascii_trajectories.csv", header=TRUE, sep=",") %>%
  dplyr::select(-Period)
write.csv(BDAe0Mtraj, paste0("./regdata/", "BDAe0Mtraj.csv"), row.names = F)

# TFR input ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

my.regtfr.file.BDA <- "regdata/tfr.BDA.txt"
read.delim(my.regtfr.file.BDA , check.names = F)

setwd(code)


