# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: load subnational population data, DHS/MICS fertilty data

# Date created: 29 November 2019
# Last updated: 31 December 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Read in data: IPUMS data -------------------------------------------

setwd(input)

# Adm0 population growth from WPP -----------------------------------------
url1                    <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F02_POPULATION_GROWTH_RATE.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
growth                  <- read_excel(tf, 1L, skip = 16)

growth                  <- growth %>%
                           dplyr::select(c("Region, subregion, country or area *", "2010-2015"))%>%
                           filter(`Region, subregion, country or area *` == "Zambia")

## Section: Read in and prepare IPUMS Census microdata
ZMB.census.2010         <- "ipumsi_00018.dat"
census.ddi              <- read_ipums_ddi("ipumsi_00018.xml")
census.data             <- read_ipums_micro(census.ddi, verbose = FALSE)

table(census.data$AGE2)
# Age2 98 is an NA
census.data$AGE2[which(census.data$AGE2 == 98)] <- NA

table(census.data$AGE)
# Age 999 is an NA
census.data$AGE[which(census.data$AGE == 999)] <- NA


# Geolevel 1 contains consistent boundaries where Eastern, Muchinga and Northern are merged due to the creation of Muchinga
# from 5 districts in Northern (Chinsali, Isoka, Mpika, Nakonde, Mafinga) and one in Eastern (Chama) in 2011: https://www.muc.gov.zm/
# 894001	Central
# 894002	Copperbelt
# 894003	Eastern, Muchinga, Northern
# 894004	Luapula
# 894005	Lusaka
# 894008	North Western
# 894009	Southern
# 894010	Western

# 894003007 Chinsali
# 894003008 Isoka, Mafinga, Nakonde
# 894003009 Mpika
# 894003006 Chama

# GEO1_ZM contains the non-harmonized regions
# 1 Central
# 2 Copperbelt
# 3 Eastern
# 4 Luapula
# 5 Lusaka
# 6 Muchinga
# 7 Northern
# 8 North-Western
# 9 Southern
# 10 Western

## Construct age/sex and age distributions, both by 1-yr and 5-yr

## Age-Sex distribution, ADM0
asd.adm0 <- census.data$PERWT[1] * table(census.data$AGE,
                       census.data$SEX)

## Age-Sex distribution, ADM1
asd.adm1 <- table(census.data$AGE,
                  census.data$SEX,
                  census.data$GEO1_ZM)*census.data$PERWT[1]

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
matrix.names   <- c(unique(census.data$GEO1_ZM))
noumbissi      <- array(data = NA, dim =c(10,2,length(asd.adm1[1,1,])), dimnames = list(row.names,column.names, matrix.names)) 

for(k in row.names){                             # all age digits
  for(j in column.names){                        # both sexes 1- female || 2- male
    for(i in 1:length(asd.adm1[1,1,])){          # all ADM1 levels
      
      noumbissi[k+1, j, i] <- check_heaping_noumbissi(asd.adm1[,j,i],0:80,ageMin = 20,ageMax = 80,digit=k) 
    }
  }
}


# Female
f.pop <- asd.adm1[,2,]

for(i in 1:length(asd.adm1[1,2,])){
  
  ifelse(any(noumbissi > 1.25 | noumbissi < 0.75) == T, 
         f.pop <- spencer(asd.adm1[,2,i],0:80),
         f.pop <- asd.adm1[,2,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  f.pop[1:10]    <- asd.adm1[1:10,2,i]

  f.pop.all[[i]] <- f.pop
}


# Male
m.pop <- asd.adm1[,1,]

for(i in 1:length(asd.adm1[1,1,])){
  
  ifelse(any(noumbissi > 1.25 | noumbissi < 0.75) == T, 
         m.pop <- spencer(asd.adm1[,1,i],0:80),
         m.pop <- asd.adm1[,1,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  m.pop[1:10]    <- asd.adm1[1:10,1,i]

  m.pop.all[[i]] <- m.pop
}

###########################################################################################



# Flat dataset
male.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(male.adm1.census1) <- c("AGE2", "PERWT", "GEO1_ZM")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(m.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEO1_ZM = i) 

  names(data) <- c("AGE2", "PERWT", "GEO1_ZM")
  
  male.adm1.census1 <- rbind(male.adm1.census1, data)
  
}

female.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(female.adm1.census1) <- c("AGE2", "PERWT", "GEO1_ZM")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(f.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEO1_ZM = i) 
  
  names(data) <- c("AGE2", "PERWT", "GEO1_ZM")

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
  dplyr::select(c("PERWT", "GEO1_ZM", "agegroup")) %>%
  group_by(agegroup, GEO1_ZM) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

male.adm1.census5$agegroup <- factor(male.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))

male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(GEO1_ZM == 1, "Central", 
                ifelse(GEO1_ZM == 2, "Copperbelt",
                ifelse(GEO1_ZM == 3, "EasternNorthernMuchinga",
                ifelse(GEO1_ZM == 4, "Luapula",
                ifelse(GEO1_ZM == 5, "Lusaka",
                ifelse(GEO1_ZM == 6, "North Western",
                ifelse(GEO1_ZM == 7, "Southern",
                ifelse(GEO1_ZM == 8, "Western", NA)))))))))


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
  group_by(agegroup, GEO1_ZM) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

female.adm1.census5$agegroup <- factor(female.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                                "60-64", "65-69", "70-74", "75-79", "80+"))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(GEO1_ZM == 1, "Central", 
                       ifelse(GEO1_ZM == 2, "Copperbelt",
                              ifelse(GEO1_ZM == 3, "EasternNorthernMuchinga",
                                     ifelse(GEO1_ZM == 4, "Luapula",
                                            ifelse(GEO1_ZM == 5, "Lusaka",
                                                   ifelse(GEO1_ZM == 6, "North Western",
                                                          ifelse(GEO1_ZM == 7, "Southern",
                                                                 ifelse(GEO1_ZM == 8, "Western", NA)))))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(GEO1_ZM == 1, 10, 
                       ifelse(GEO1_ZM == 2, 20,
                              ifelse(GEO1_ZM == 3, 30,
                                     ifelse(GEO1_ZM == 4, 40,
                                            ifelse(GEO1_ZM == 5, 50,
                                                   ifelse(GEO1_ZM == 6, 60,
                                                          ifelse(GEO1_ZM == 7, 70,
                                                                 ifelse(GEO1_ZM == 8, 80, NA)))))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(GEO1_ZM == 1, 10, 
                           ifelse(GEO1_ZM == 2, 20,
                                  ifelse(GEO1_ZM == 3, 30,
                                         ifelse(GEO1_ZM == 4, 40,
                                                ifelse(GEO1_ZM == 5, 50,
                                                       ifelse(GEO1_ZM == 6, 60,
                                                              ifelse(GEO1_ZM == 7, 70,
                                                                     ifelse(GEO1_ZM == 8, 80, NA)))))))))

# Save file ---------------------------------------------------------------

female.pop.2010 <- female.adm1.census5%>%
  filter(!is.na(agegroup))

male.pop.2010   <- male.adm1.census5 %>%
  filter(!is.na(agegroup))

ZMBpopF <- female.pop.2010[,c(5,4,1,3)]
ZMBpopM <-   male.pop.2010[,c(5,4,1,3)]

# Undercount adjustment ---------------------------------------------------
#unlink('C:/Users/kathrinweny/AppData/Local/Temp/Rtmp0MMeVo/file1d5036742a3b', recursive=TRUE)
undercount <- read.csv.sql("undercount.csv", 
                           sql = "select * from file where country == 'ZMB'")

ZMBpopF    <- merge(ZMBpopF, undercount[, c("name", "undercount")], by = "name")
ZMBpopF$sum_age <- ZMBpopF$sum_age*(1+ZMBpopF$undercount)

ZMBpopM    <- merge(ZMBpopM, undercount[, c("name", "undercount")], by = "name")
ZMBpopM$sum_age <- ZMBpopM$sum_age*(1+ZMBpopM$undercount)

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2010-2015
ZMBpopM$'2011' <- ZMBpopM$'sum_age'*(as.numeric(growth[1,2])/100 +1)
ZMBpopM$'2012' <- ZMBpopM$'2011'*(as.numeric(growth[1,2])/100 +1)
ZMBpopM$'2013' <- ZMBpopM$'2012'*(as.numeric(growth[1,2])/100 +1)
ZMBpopM$'2014' <- ZMBpopM$'2013'*(as.numeric(growth[1,2])/100 +1)
ZMBpopM$'2015' <- ZMBpopM$'2014'*(as.numeric(growth[1,2])/100 +1)

# use growth rate for 2010-2015
ZMBpopF$'2011' <- ZMBpopF$'sum_age'*(as.numeric(growth[1,2])/100 +1)
ZMBpopF$'2012' <- ZMBpopF$'2011'*(as.numeric(growth[1,2])/100 +1)
ZMBpopF$'2013' <- ZMBpopF$'2012'*(as.numeric(growth[1,2])/100 +1)
ZMBpopF$'2014' <- ZMBpopF$'2013'*(as.numeric(growth[1,2])/100 +1)
ZMBpopF$'2015' <- ZMBpopF$'2014'*(as.numeric(growth[1,2])/100 +1)

ZMBpopF <- ZMBpopF[,c(2,1,3,9)]
ZMBpopM <- ZMBpopM[,c(2,1,3,9)]

# Export ------------------------------------------------------------------

colnames(ZMBpopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(ZMBpopM) <- c("reg_code","name","age","2015") # Why 2015?

ZMBpopF <- ZMBpopF[order(ZMBpopF$reg_code, ZMBpopF$age),]
ZMBpopM <- ZMBpopM[order(ZMBpopM$reg_code, ZMBpopM$age),]

write.table(ZMBpopF, paste0(output, "regdata/", iso, "popF.txt"), sep = "\t", row.names = FALSE)
write.table(ZMBpopM, paste0(output, "regdata/", iso, "popM.txt"), sep = "\t", row.names = FALSE)





