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

traj$quantilesMage
popM_2015  <- as.data.frame(traj$'quantilesMage'[1:nrow(traj$countries),1:27,2,1]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2015
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
         age = 
         ifelse(age == 0, "'0-4'", 
         ifelse(age == 5, "'5-9'",
         ifelse(age == 10, "'10-14'",
         ifelse(age == 15, "15-19",
         ifelse(age == 20, "20-24",
         ifelse(age == 25, "25-29",
         ifelse(age == 30, "30-34",
         ifelse(age == 35, "35-39",
         ifelse(age == 40, "40-44",
         ifelse(age == 45, "45-49",
         ifelse(age == 50, "50-54",
         ifelse(age == 55, "55-59",
         ifelse(age == 60, "60-64",
         ifelse(age == 65, "65-69", 
         ifelse(age == 70, "70-74",  
         ifelse(age == 75, "75-79", 
         ifelse(age == 80, "80-84", 
         ifelse(age == 85, "85-89", 
         ifelse(age == 90, "90-94", 
         ifelse(age == 95, "95-99", 
         ifelse(age == 100, "100-104", 
         ifelse(age == 105, "105-109",
         ifelse(age == 110, "110-114", 
         ifelse(age == 115, "115-119", 
         ifelse(age == 120, "120-124", 
         ifelse(age == 125, "125-129", 
         ifelse(age == 130, "130-135", NA))))))))))))))))))))))))))))

# Filter age groups that do not exist between 2015 and 2020
#pop <- pop %>%
 # filter(age != "'0-1'") # subsumed in 0-4 age group

pop$age <- ifelse(pop$age == "80-84", "80+",
                  ifelse(pop$age == "85-89", "80+", 
                         ifelse(pop$age == "90-94", "80+", 
                                ifelse(pop$age == "95-99", "80+",
                                       ifelse(pop$age == "100-104", "80+",
                                              ifelse(pop$age == "105-109", "80+",
                                                     ifelse(pop$age == "110-114", "80+",
                                                            ifelse(pop$age == "115-119", "80+",
                                                                   ifelse(pop$age == "120-124","80+",
                                                                          ifelse(pop$age == "125-129", "80+", 
                                                                                 ifelse(pop$age == "130-135", "80+", pop$age)))))))))))



pop <- aggregate(cbind(pop_2015, pop_2020) ~ name + reg_code + sex + age, pop, sum)

# Order age groups
pop$age <- as.factor(pop$age)
pop$age = factor(pop$age, levels(pop$age)[c(1,4,3,2,5:18)]) 

# Add P-codes

pop$reg_code <- paste0(p.code, pop$reg_code)

names(pop)  <- c("ADM1_EN", "ADM1_PCODE", "Sex", "Age", "pop_2015", "pop_2020")

# Export ------------------------------------------------------------------

write.csv(pop, file = file.path(paste0(iso,"_adm1_pop_2015_2020.csv")), row.names = FALSE, quote = FALSE)

# Note in the case of Zamiba, regions divided by a ',' will be put into different columns --- currently manual adjustment needed
setwd(code)