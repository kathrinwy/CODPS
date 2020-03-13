# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: load subnational population data, DHS/MICS fertilty data

# Date created: 11 September 2019
# Last updated: 9 January 2020

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
                           filter(`Region, subregion, country or area *` == "Burkina Faso")

## Section: Read in and prepare IPUMS Census microdata
BFA.census.2006         <- "ipumsi_00012.dat"
census.ddi              <- read_ipums_ddi("ipumsi_00012.xml")
census.data             <- read_ipums_micro(census.ddi, verbose = FALSE)

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
table(census.data$AGE2)
# Age2 98 is an NA
census.data$AGE2[which(census.data$AGE2 == 98)] <- NA

table(census.data$AGE)
# Age 999 is an NA
census.data$AGE[which(census.data$AGE == 999)] <- NA

## Age-Sex distribution, ADM0
asd.adm0 <- census.data$PERWT[1] * table(census.data$AGE,
                       census.data$SEX)

## Age-Sex distribution, ADM1
asd.adm1 <- table(census.data$AGE,
                  census.data$SEX,
                  census.data$DHS_IPUMSI_BF)*census.data$PERWT[1]

#############################################################################
# Smoothing -----------------------------------------------------------------
# Calculate Noumbissi Index for ADM1 level data by male and female separately
#############################################################################

f.pop.all   <- list()
m.pop.all   <- list()

# For humanitarian use: if either male/female Noubmissi in any given adm1 region exceeds .25 deviation
# from perfeciton (1) in any of the 10-digit Noumbissis → whole country is smoothed (by region and sex)  [impliying 
# that smoothing relatively good data is less disadvantagous than not smoothing bad data]

# Decided on 13 March 2020 by PDB, TD

#asd.adm1[1,2,3]
#c("age", "sex", "region")

row.names      <- c(0:9)
column.names   <- c(1:2) # 1- female || 2- male
matrix.names   <- c(unique(census.data$DHS_IPUMSI_BF))
noumbissi      <- array(data = NA, dim =c(10,2,length(asd.adm1[1,1,])), dimnames = list(row.names,column.names, matrix.names)) 

for(k in row.names){                             # all age digits
  for(j in column.names){                        # both sexes 1- female || 2- male
    for(i in 1:length(asd.adm1[1,1,])){          # all ADM1 levels
      
      noumbissi[k+1, j, i] <- check_heaping_noumbissi(asd.adm1[,j,i],0:98,ageMin = 20,ageMax = 80,digit=k) 
    }
  }
}

table(census.data$AGE)
# Female
f.pop <- asd.adm1[,2,]

for(i in 1:length(asd.adm1[1,2,])){
  
  ifelse(any(noumbissi > 1.25 | noumbissi < 0.75) == T, 
         f.pop <- spencer(asd.adm1[,2,i],0:98),
         f.pop <- asd.adm1[,2,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  f.pop[1:10]    <- asd.adm1[1:10,2,i]
  f.pop[90:98]  <- c(asd.adm1[91:99,2,i])
  
  f.pop.all[[i]] <- f.pop
}


# Male
m.pop <- asd.adm1[,1,]

for(i in 1:length(asd.adm1[1,1,])){
  
  ifelse(any(noumbissi > 1.25 | noumbissi < 0.75) == T, 
         m.pop <- spencer(asd.adm1[,1,i],0:98),
         m.pop <- asd.adm1[,1,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  m.pop[1:10]    <- asd.adm1[1:10,1,i]
  m.pop[90:98]  <- c(asd.adm1[91:99,1,i])
  
  m.pop.all[[i]] <- m.pop
}

###########################################################################################

# Flat dataset
male.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(male.adm1.census1) <- c("AGE2", "PERWT", "DHS_IPUMSI_BF")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(m.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(DHS_IPUMSI_BF = i) 
  
  names(data) <- c("AGE2", "PERWT", "DHS_IPUMSI_BF")
  
  male.adm1.census1 <- rbind(male.adm1.census1, data)

}

female.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(female.adm1.census1) <- c("AGE2", "PERWT", "DHS_IPUMSI_BF")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(f.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(DHS_IPUMSI_BF = i) 
  
  names(data) <- c("AGE2", "PERWT", "DHS_IPUMSI_BF")
  
  female.adm1.census1 <- rbind(female.adm1.census1, data)
  
}

## convert region.no to region-name 

male.adm1.census1$AGE2 <- as.numeric(male.adm1.census1$AGE2)

male.adm1.census1 <-
  mutate(male.adm1.census1, 
         agegroup = ifelse(0 <= AGE2 & AGE2 <= 4, "0-4",
                           ifelse(5 <= AGE2 & AGE2 <= 9, "5-9",
                           ifelse(10 <= AGE2 & AGE2 <= 14, "10-14",
                           ifelse(15 <= AGE2&  AGE2 <= 19, "15-19",
                           ifelse(20 <= AGE2 & AGE2 <= 24, "20-24",
                           ifelse(25 <= AGE2 & AGE2 <= 29, "25-29",
                           ifelse(30 <= AGE2 & AGE2 <= 34, "30-34",
                           ifelse(35 <= AGE2 & AGE2 <= 39, "35-39",
                           ifelse(40 <= AGE2 & AGE2 <= 44, "40-44",
                           ifelse(45 <= AGE2 & AGE2 <= 49, "45-49",
                           ifelse(50 <= AGE2 & AGE2 <= 54, "50-54",
                           ifelse(55 <= AGE2 & AGE2 <= 59, "55-59",
                           ifelse(60 <= AGE2 & AGE2 <= 64, "60-64",
                           ifelse(65 <= AGE2 & AGE2 <= 69, "65-69",
                           ifelse(70 <= AGE2 & AGE2 <= 74, "70-74",
                           ifelse(75 <= AGE2 & AGE2 <= 79, "75-79",
                           ifelse(AGE2 >=80, "80+", NA))))))))))))))))))

# Create 5-year age groups

male.adm1.census5 <- male.adm1.census1 %>%
  group_by(agegroup, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

male.adm1.census5$agegroup <- factor(male.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))

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
                                                                        ifelse(DHS_IPUMSI_BF == 9, "Hauts-Bassins",
                                                                               ifelse(DHS_IPUMSI_BF ==10, "Nord",
                                                                                      ifelse(DHS_IPUMSI_BF ==11, "Plateau Central",
                                                                                             ifelse(DHS_IPUMSI_BF ==12, "Sahel",
                                                                                                    ifelse(DHS_IPUMSI_BF ==13, "Sud-Ouest", NA))))))))))))))

female.adm1.census1$AGE2 <- as.numeric(female.adm1.census1$AGE2)

female.adm1.census1 <-
  mutate(female.adm1.census1, 
          agegroup = ifelse(0 <= AGE2 & AGE2 <= 4, "0-4",
          ifelse(5 <= AGE2 & AGE2 <= 9, "5-9",
          ifelse(10 <= AGE2 & AGE2 <= 14, "10-14",
          ifelse(15 <= AGE2&  AGE2 <= 19, "15-19",
          ifelse(20 <= AGE2 & AGE2 <= 24, "20-24",
          ifelse(25 <= AGE2 & AGE2 <= 29, "25-29",
          ifelse(30 <= AGE2 & AGE2 <= 34, "30-34",
          ifelse(35 <= AGE2 & AGE2 <= 39, "35-39",
          ifelse(40 <= AGE2 & AGE2 <= 44, "40-44",
          ifelse(45 <= AGE2 & AGE2 <= 49, "45-49",
          ifelse(50 <= AGE2 & AGE2 <= 54, "50-54",
          ifelse(55 <= AGE2 & AGE2 <= 59, "55-59",
          ifelse(60 <= AGE2 & AGE2 <= 64, "60-64",
          ifelse(65 <= AGE2 & AGE2 <= 69, "65-69",
          ifelse(70 <= AGE2 & AGE2 <= 74, "70-74",
          ifelse(75 <= AGE2 & AGE2 <= 79, "75-79",
          ifelse(AGE2 >=80, "80+", NA))))))))))))))))))

# Create 5-year age groups
female.adm1.census5 <- female.adm1.census1 %>%
  group_by(agegroup, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

female.adm1.census5$agegroup <- factor(female.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))

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
         ifelse(DHS_IPUMSI_BF == 9, "Hauts-Bassins",
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
                    ifelse(DHS_IPUMSI_BF ==12, 56,
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
         ifelse(DHS_IPUMSI_BF ==12, 56,
         ifelse(DHS_IPUMSI_BF ==13, 57,NA))))))))))))))

# Extract data -------------------------------------------------------------

female.pop.2006 <- female.adm1.census5%>%
  filter(!is.na(agegroup))
  
male.pop.2006   <- male.adm1.census5 %>%
  filter(!is.na(agegroup))

BFApopF <- female.pop.2006[,c(5,4,1,3)]
BFApopM <-   male.pop.2006[,c(5,4,1,3)]

# Undercount adjustment ---------------------------------------------------

undercount <- read.csv.sql("undercount.csv", 
                           sql = "select * from file where country == 'BFA'")
unlink('C:/Users/weny/AppData/Local/Temp/RtmpuqaK45/file40f46963532')
BFApopF    <- merge(BFApopF, undercount[, c("name", "undercount")], by = "name")
BFApopF$sum_age <- BFApopF$sum_age*(1+BFApopF$undercount)

BFApopM    <- merge(BFApopM, undercount[, c("name", "undercount")], by = "name")
BFApopM$sum_age <- BFApopM$sum_age*(1+BFApopM$undercount)

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

BFApopF <- BFApopF[,c(2,1,3,14)]
BFApopM <- BFApopM[,c(2,1,3,14)]

# Export ------------------------------------------------------------------

colnames(BFApopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(BFApopM) <- c("reg_code","name","age","2015") # Why 2015?

BFApopF <- BFApopF[order(BFApopF$reg_code, BFApopF$age),]
BFApopM <- BFApopM[order(BFApopM$reg_code, BFApopM$age),]

write.table(BFApopF, paste0(output, "regdata/", iso, "popF.txt"), sep = "\t", row.names = FALSE)
write.table(BFApopM, paste0(output, "regdata/", iso, "popM.txt"), sep = "\t", row.names = FALSE)

setwd(code)
