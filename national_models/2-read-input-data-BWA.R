# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: load subnational population data, DHS/MICS fertilty data

# Date created: 11 September 2019
# Last updated: 14 January 2020

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
                           filter(`Region, subregion, country or area *` == country)

## Section: Read in and prepare IPUMS Census microdata
BWA.census.2012         <- "ipumsi_00024.dat"
census.ddi              <- read_ipums_ddi("ipumsi_00024.xml")
census.data             <- read_ipums_micro(census.ddi, verbose = FALSE)

unique(census.data$GEOLEV1)

# Age 999 is an NA
census.data$AGE[which(census.data$AGE == 999)] <- NA
table(census.data$AGE)
## Construct age/sex and age distributions, both by 1-yr and 5-yr

## Age-Sex distribution, ADM0
asd.adm0 <- census.data$PERWT[1] * table(census.data$AGE,
                       census.data$SEX)

## Age-Sex distribution, ADM1
asd.adm1 <- table(census.data$AGE,
                  census.data$SEX,
                  census.data$GEOLEV1)*census.data$PERWT[1]

###########################################################################################
# Smoothing

f.pop.all <- list()
m.pop.all   <- list()
##Calculate Noumbissi Index for ADM1 level data:

#asd.adm1[1,2,3]
#c("age", "sex", "region")

female.Noumbissi0 <- rep(0,length(asd.adm1[1,2,])) # length across all regions
male.Noumbissi0   <- rep(0,length(asd.adm1[1,1,]))

for (j in 1:length(asd.adm1[1,1,])){
  female.Noumbissi0[j] <- Noumbissi(asd.adm1[,2,j],0:94,ageMin = 20, ageMax = 85,digit=0)
  male.Noumbissi0[j] <- Noumbissi(asd.adm1[,1,j],0:94,ageMin = 20, ageMax = 85,digit=0)
} 

female.Noumbissi5 <- rep(0,length(asd.adm1[1,2,]))
male.Noumbissi5 <- rep(0,length(asd.adm1[1,1,]))

for (j in 1:length(asd.adm1[1,1,])){
  female.Noumbissi5[j] <- Noumbissi(asd.adm1[,2,j],0:94,ageMin = 20, ageMax = 85,digit=5)
  male.Noumbissi5[j] <- Noumbissi(asd.adm1[,2,j],0:94,ageMin = 20, ageMax = 85,digit=5)
} 

##Calculate mean relative difference for digit preference between ages 
## ending in -0 and -5  
rel.diff.female.05 <- 100*(female.Noumbissi0 - female.Noumbissi5)/(0.5*(female.Noumbissi0 + female.Noumbissi5))
rel.diff.male.05 <- 100*(male.Noumbissi0 - male.Noumbissi5)/(0.5*(male.Noumbissi0 + male.Noumbissi5))
mean(rel.diff.female.05)
mean(rel.diff.male.05)

##If female age preference for ages ending  0s and 5s differs by moe than 15%,
##then smooth single-year ages for females using Spencer's smoothing technique 
##(for ages 10-89), else use the raw data directly

f.pop <- asd.adm1[,2,]

for(i in 1:length(asd.adm1[1,2,])){
ifelse(mean(rel.diff.female.05) > 15, 
       f.pop <- spencer(asd.adm1[,2,i],0:94),
       f.pop <- asd.adm1[,2,i]
)
# Replace 0-9 and ages above 90 with asd.adm1 by default
f.pop[1:10]    <- asd.adm1[1:10,2,i]
f.pop[91:100]  <- c(asd.adm1[91:94,2,i], NA)

f.pop.all[[i]] <- f.pop
}
##If male age preference for ages ending  0s and 5s differs by moe than 15%,
##then smooth single-year ages for males using Spencer's smoothing technique 
##(for ages 10-89), else use the raw data directly


m.pop <- asd.adm1[,1,]

for(i in 1:length(asd.adm1[1,1,])){
ifelse(mean(rel.diff.male.05) > 15, 
       m.pop <- spencer(asd.adm1[,1,i],0:94),
       m.pop <- asd.adm1[,1,i]
)

# Replace 0-9 and ages above 90 with asd.adm1 by default
m.pop[1:10]    <- asd.adm1[1:10,1,i]
m.pop[91:100]  <- c(asd.adm1[91:94,1,i], NA)

m.pop.all[[i]] <- m.pop
}



###########################################################################################


# Flat dataset
male.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(male.adm1.census1) <- c("AGE2", "PERWT", "GEOLEV1")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(m.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEOLEV1 = i) 
  
  names(data) <- c("AGE2", "PERWT", "GEOLEV1")
  
  male.adm1.census1 <- rbind(male.adm1.census1, data)

}

female.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(female.adm1.census1) <- c("AGE2", "PERWT", "GEOLEV1")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(f.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEOLEV1 = i) 
  
  names(data) <- c("AGE2", "PERWT", "GEOLEV1")
  
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
  group_by(agegroup, GEOLEV1) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

male.adm1.census5$agegroup <- factor(male.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))
table(male.adm1.census5$GEOLEV1)
male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(GEOLEV1 == 1, "Gaborone", 
                ifelse(GEOLEV1 == 2, "Francistown",
                ifelse(GEOLEV1 == 3, "Lobaste",
                ifelse(GEOLEV1 == 4, "Selebi Phikwe",
                ifelse(GEOLEV1 == 5, "Central Tutume Sowa",
                ifelse(GEOLEV1 == 6, "Ngwaketse Ngwaketse West Ngwaketse Southern Southern Jwaneng",
                ifelse(GEOLEV1 == 7, "Borolong",
                ifelse(GEOLEV1 == 8, "South East",
                ifelse(GEOLEV1 == 9, "Kweneng Kweneng South Kweneng North",
                ifelse(GEOLEV1 ==10, "Kgatleng",
                ifelse(GEOLEV1 ==11, "Central Serowe Palapye",
                ifelse(GEOLEV1 ==12, "Central Mahalapye",
                ifelse(GEOLEV1 ==13, "Central Bobonong",
                ifelse(GEOLEV1 ==14, "Central Boteti Orapa",
                ifelse(GEOLEV1 ==15, "North East",
                ifelse(GEOLEV1 ==16, "Ngamiland East",
                ifelse(GEOLEV1 ==17, "Ngamiland West Delta",
                ifelse(GEOLEV1 ==18, "Chobe",
                ifelse(GEOLEV1 ==19, "Ghanzi Central Kgalagadi Game Reserve (CKGR)",
                ifelse(GEOLEV1 ==20, "Tshabong (Kgalagadi South)",
                ifelse(GEOLEV1 ==21, "Hukunsti",NA))))))))))))))))))))))

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
  group_by(agegroup, GEOLEV1) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

female.adm1.census5$agegroup <- factor(female.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(GEOLEV1 == 1, "Gaborone", 
                       ifelse(GEOLEV1 == 2, "Francistown",
                              ifelse(GEOLEV1 == 3, "Lobaste",
                                     ifelse(GEOLEV1 == 4, "Selebi Phikwe",
                                            ifelse(GEOLEV1 == 5, "Central Tutume Sowa",
                                                   ifelse(GEOLEV1 == 6, "Ngwaketse Ngwaketse West Ngwaketse Southern Southern Jwaneng",
                                                          ifelse(GEOLEV1 == 7, "Borolong",
                                                                 ifelse(GEOLEV1 == 8, "South East",
                                                                        ifelse(GEOLEV1 == 9, "Kweneng Kweneng South Kweneng North",
                                                                               ifelse(GEOLEV1 ==10, "Kgatleng",
                                                                                      ifelse(GEOLEV1 ==11, "Central Serowe Palapye",
                                                                                             ifelse(GEOLEV1 ==12, "Central Mahalapye",
                                                                                                    ifelse(GEOLEV1 ==13, "Central Bobonong",
                                                                                                           ifelse(GEOLEV1 ==14, "Central Boteti Orapa",
                                                                                                                  ifelse(GEOLEV1 ==15, "North East",
                                                                                                                         ifelse(GEOLEV1 ==16, "Ngamiland East",
                                                                                                                                ifelse(GEOLEV1 ==17, "Ngamiland West Delta",
                                                                                                                                       ifelse(GEOLEV1 ==18, "Chobe",
                                                                                                                                              ifelse(GEOLEV1 ==19, "Ghanzi Central Kgalagadi Game Reserve (CKGR)",
                                                                                                                                                     ifelse(GEOLEV1 ==20, "Tshabong (Kgalagadi South)",
                                                                                                                                                            ifelse(GEOLEV1 ==21, "Hukunsti",NA))))))))))))))))))))))
# Extract data -------------------------------------------------------------

female.pop.2012 <- female.adm1.census5%>%
  filter(!is.na(agegroup))
  
male.pop.2012   <- male.adm1.census5 %>%
  filter(!is.na(agegroup))

BWApopF <- female.pop.2012[,c(4,2,1,3)]
BWApopM <-   male.pop.2012[,c(4,2,1,3)]

# Undercount adjustment ---------------------------------------------------

undercount <- read.csv.sql("undercount.csv", 
                           sql = "select * from file where country == 'BWA'")
unlink('C:/Users/weny/AppData/Local/Temp/RtmpuqaK45/file40f46963532')
BWApopF    <- merge(BWApopF, undercount[, c("name", "undercount")], by = "name")
BWApopF$sum_age <- BWApopF$sum_age*(1+BWApopF$undercount)

BWApopM    <- merge(BWApopM, undercount[, c("name", "undercount")], by = "name")
BWApopM$sum_age <- BWApopM$sum_age*(1+BWApopM$undercount)

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2010-2015
BWApopM$'2013' <- BWApopM$'sum_age'*(as.numeric(growth[1,3])/100 +1)
BWApopM$'2014' <- BWApopM$'2013'*(as.numeric(growth[1,3])/100 +1)
BWApopM$'2015' <- BWApopM$'2014'*(as.numeric(growth[1,3])/100 +1)

# use growth rate for 2010-2015
BWApopF$'2013' <- BWApopF$'sum_age'*(as.numeric(growth[1,3])/100 +1)
BWApopF$'2014' <- BWApopF$'2013'*(as.numeric(growth[1,3])/100 +1)
BWApopF$'2015' <- BWApopF$'2014'*(as.numeric(growth[1,3])/100 +1)

BWApopF <- BWApopF[,c(2,1,3,ncol(BWApopF))]
BWApopM <- BWApopM[,c(2,1,3,ncol(BWApopM))]

# Export ------------------------------------------------------------------

colnames(BWApopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(BWApopM) <- c("reg_code","name","age","2015") # Why 2015?

BWApopF <- BWApopF[order(BWApopF$reg_code, BWApopF$age),]
BWApopM <- BWApopM[order(BWApopM$reg_code, BWApopM$age),]

write.table(BWApopF, paste0(output, "regdata/", iso, "popF.txt"), sep = "\t", row.names = FALSE)
write.table(BWApopM, paste0(output, "regdata/", iso, "popM.txt"), sep = "\t", row.names = FALSE)

setwd(code)
