# This file preps the veg data to fit it in the model

# CONSTRUCTED POOLS
# fill in 2007 data and subset complete data 2000-2017 
# 1. subset the data
# 2. count the number of pools 
# 3. count the number of years
# 4. sum frequency data
# 5. spread the table
# 6. create a matrix of seeds added each year

# REFERENCE POOLS
# subset complete data 2000-2015
# 1. subset the data
# 2. count the number of pools
# 3. count the number of years
# 4. sum frequency data
# 5. spread the table

#------------------------------
# Load data and package
# Remember to set your data pathway first!
source("compile_composition.R") 
# View(const_com)
# View(ref_com)
library(tidyverse)

#------------------------------
# CONSTRUCTED POOLS:
# Remove control plots and organize by year, pool, and LACOdens
const_com_LACO <- const_com %>%
  filter(Treatment.1999 != "Control") %>% #remove control plots
  select(Year, Pool, LACOdens, Size) %>%
  spread(key = Year, value = LACOdens) #202 pools with seeding treatment #lots of missing data in 2007

# How many pools have complete LACOdens data?
const_com_noNA <- const_com_LACO[complete.cases(const_com_LACO),] #only 72 pools have complete data

# Fill in 2007 data with dummy data and subset complete data 2000-2017
const_com_dummy <- const_com_LACO 
const_com_dummy$`2007` <- 1
const_dummy_sub <- const_com_dummy[complete.cases(const_com_dummy),] #142 pools

#1d. subset the complete data and fill in 2007 data with zeros
const_dummy_join <- inner_join(const_com, const_dummy_sub, by.y = "Pool") %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#2d. count the number of pools
n_pools <- length(unique(const_dummy_join$Pool))

#3d. count the number of years
n_years <- length(unique(const_dummy_join$Year))

#4b. sum frequency data
#Exotic grass (EG) group contains BRHO, HOMA, and LOMU:
const_dummy_join$sum_EG <- rowSums(cbind(const_dummy_join$BRHO, const_dummy_join$HOMA, const_dummy_join$LOMU))

#Native forb (NF) group contains PLST and DOCO:
const_dummy_join$sum_NF <- rowSums(cbind(const_dummy_join$PLST, const_dummy_join$DOCO))

#5b. spread the dataset
#each matrix should have a [n_pools x n_years] dimension
LACOdens <- const_dummy_join %>%
  select(Year, Pool, LACOdens) %>%
  spread(key = Year, value = LACOdens) %>%
  select(-Pool)
ERVAdens <- const_dummy_join %>%
  select(Year, Pool, ERVAdens) %>%
  spread(key = Year, value = ERVAdens) %>%
  select(-Pool)
sumEGcover <- const_dummy_join %>%
  select(Year, Pool, sum_EG) %>%
  spread(key = Year, value = sum_EG) %>%
  select(-Pool)
sumNFcover <- const_dummy_join %>%
  select(Year, Pool, sum_NF) %>%
  spread(key = Year, value = sum_NF) %>%
  select(-Pool)

#6b. create a matrix of seeds added each year
seedtrt <- const_dummy_join %>%
  select(Pool, Treatment.1999, Treatment.2000) %>%
  unique(const_dummy_join$Pool, incomparables = FALSE) %>%
  mutate(Y1 = ifelse(Treatment.1999 == "Control", 0, 100)) %>%
  mutate(Y2 = ifelse(Treatment.2000 %in% c("Control", "NO Lasthenia"), 0, 100)) %>%
  mutate(Y3 = ifelse(Treatment.2000 == "Lasthenia", 100, 0))


#----------------------
# REFERENCE POOLS:
#Nine pools with complete LACO data 2002-2015 
#1. Take the mean of sub-sampling plots and organize by year, pool, and LACO
ref_com_mean <- ref_com %>%
  select(-Quadrat) %>%
  group_by(Year, Pool) %>%
  filter(Year %in% c(2002:2015)) %>%
  summarise_each(funs(mean)) %>%
  mutate_each(funs(as.integer(.)))

ref_com_LACO <- ref_com_mean %>%
  select(Year, Pool, LACO) %>%
  spread(key = Year, value = LACO) %>%  
  filter(Pool %in% c(9, 20, 34, 38, 52, 63, 77, 31, 27)) %>%
  mutate_each(funs(as.integer(.)))

ref_com_join <- inner_join(ref_com_mean, ref_com_LACO, by.y = "Pool") %>%
  filter(Year %in% c(2002:2015))

#2. count the number of pools
ref_n_pools <- length(unique(ref_com_join$Pool))

#3. count the number of years
ref_n_years <- length(unique(ref_com_join$Year))

#4. sum frequency data
#Exotic grass (EG) group contains BRHO, HOMA, and LOMU:
ref_com_join$sum_EG <- as.integer(rowSums(cbind(ref_com_join$BRHO, ref_com_join$HOMA, ref_com_join$LOMU)))

#Native forb (NF) group contains PLST and DOCO:
ref_com_join$sum_NF <- as.integer(rowSums(cbind(ref_com_join$PLST, ref_com_join$DOCO)))

#5b. spread the dataset
#each matrix should have a [n_pools x n_years] dimension
ref_LACOcover <- ref_com_join %>%
  select(Year, Pool, LACO) %>%
  spread(key = Year, value = LACO) %>%
  select(-Pool)
ref_ERVAcover <-  ref_com_join  %>%
  select(Year, Pool, ERVA) %>%
  spread(key = Year, value = ERVA) %>%
  select(-Pool)
ref_sumEGcover <-  ref_com_join  %>%
  select(Year, Pool, sum_EG) %>%
  spread(key = Year, value = sum_EG) %>%
  select(-Pool)
ref_sumNFcover <-  ref_com_join  %>%
  select(Year, Pool, sum_NF) %>%
  spread(key = Year, value = sum_NF) %>%
  select(-Pool)

