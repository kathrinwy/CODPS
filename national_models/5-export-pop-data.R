# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: export

# Date created: 11 September 2019
# Last updated: 1 December 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

setwd(output)

# Sub-regional

location.file  <- paste0(output, "regdata/", iso, "locations.txt")
regions        <- (subset(read.delim(location.file), location_type == 4))[,3:4]

# 2015 and 2020 populations per region
traj <- get.pop.prediction(reg.pop.dir)

# Male population
popM_2015  <- as.data.frame(traj$'quantilesMage'[1:nrow(traj$countries),1:27,5,1]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2015
popM_2015  <- rownames_to_column(popM_2015, "reg_code")
popM_2015  <- popM_2015 %>%
  gather(age, pop_2015, -reg_code)

popM_2020  <- as.data.frame(traj$'quantilesMage'[1:nrow(traj$countries),1:27,5,2]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2020
popM_2020  <- rownames_to_column(popM_2020, "reg_code")
popM_2020  <- popM_2020 %>%
  gather(age, pop_2020, -reg_code)

popM <- merge(popM_2015, popM_2020, by = c("reg_code", "age"))

popM$sex <- "male"

# Female population
popF_2015  <- as.data.frame(traj$'quantilesFage'[1:nrow(traj$countries),1:27,5,1]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2015
popF_2015  <- rownames_to_column(popF_2015, "reg_code")
popF_2015  <- popF_2015 %>%
  gather(age, pop_2015, -reg_code)

popF_2020  <- as.data.frame(traj$'quantilesFage'[1:nrow(traj$countries),1:27,5,2]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2020
popF_2020  <- rownames_to_column(popF_2020, "reg_code")
popF_2020  <- popF_2020 %>%
  gather(age, pop_2020, -reg_code)

popF <- merge(popF_2015, popF_2020, by = c("reg_code", "age"))

popF$sex <- "female"

# Combine male and female population

pop <- rbind(popM, popF)

pop <- merge(pop, regions, by = "reg_code")

pop <- pop[,c(6,1, 5, 2, 3, 4 )]

# Recode Age group 
pop <- 
  mutate(pop,
         age = ifelse(age == 0, "0", 
         ifelse(age == 5, "'1-4'",
         ifelse(age == 10, "'5-9'",
         ifelse(age == 15, "'10-14'",
         ifelse(age == 20, "15-19",
         ifelse(age == 25, "20-24",
         ifelse(age == 30, "25-29",
         ifelse(age == 35, "30-34",
         ifelse(age == 40, "35-39",
         ifelse(age ==45, "40-44",
         ifelse(age == 50, "45-49",
        ifelse(age == 55, "50-54",
         ifelse(age == 60, "55-59",
          ifelse(age == 65, "60-64", 
          ifelse(age == 70, "65-69",  
        ifelse(age == 75, "70-74", 
          ifelse(age == 80, "75-79", 
            ifelse(age == 85, "80-84", 
         ifelse(age == 90, "85-89", 
         ifelse(age == 95, "90-94", 
         ifelse(age == 100, "95-99", 
          ifelse(age == 105, "100-104",
           ifelse(age == 110, "105-109", 
           ifelse(age == 115, "110-114", 
             ifelse(age == 120, "115-119", 
               ifelse(age == 125, "120-124", 
               ifelse(age == 130, "125-129", NA))))))))))))))))))))))))))))

# Order age groups
pop$age <- as.factor(pop$age)
pop$age = factor(pop$age, levels(pop$age)[c(1,2, 17, 3,10:16, 18:27, 4:9)]) 

# Filter age groups that do not exist between 2015 and 2020

pop <- pop %>%
  #filter(age != "0") %>%
  filter(age != "90-94") %>%
  filter(age != "95-99") %>%
  filter(age != "100-104") %>%
  filter(age != "105-109") %>%
  filter(age != "110-114") %>%
  filter(age != "115-119") %>%
  filter(age != "120-124") %>%
  filter(age != "125-129")

           
# Add P-codes

pop$reg_code <- paste0(p.code, pop$reg_code)

names(pop)  <- c("ADM1_EN", "ADM1_PCODE", "Sex", "Age", "pop_2015", "pop_2020")

# Export ------------------------------------------------------------------

# commented out as usually factors in Excel and CSV format are messed up and require manual manipulation
write.csv(pop, file = file.path(paste0(iso,"_adm1_pop_2015_2020.csv")), row.names = FALSE, quote = FALSE)

setwd(code)