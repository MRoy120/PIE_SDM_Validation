#'--------------------------
#' SLR Data Cleaning
#' August 16, 2021
#'-------------------------

## 
# Load in Libraries
library(dplyr)
library(readr)
library(tidyr)

##
# Load in SLR Data ####
slr <- read_csv("data/8443970_meantrend.csv")

##
# Filter out data only from 2011 to present
slr_clean <- slr %>%
  filter(grepl('20', Year)) %>%
  filter(!grepl('200', Year)) %>%
  filter(!grepl('2010', Year)) %>%
  filter(!grepl('2021', Year))

##
# Get an average of MSL for every three months (quarterly)

# First, fill in the missing value for 2017
slr_long <- slr_clean %>%
  select(Year, Month, Monthly_MSL) %>%
  pivot_wider(names_from = Year,
              values_from = Monthly_MSL) %>%
  pivot_longer(cols = `2011`:`2020`,
               names_to = "year",
               values_to = "monthly_msl") %>%
  rename(month = Month)
  
# Second, add columns for quarterly identifiers
slr_clean_2 <- slr_long %>%
  mutate(quarter = rep(c("first", "second", "third", "fourth"), each = 30)) %>%
  mutate(number = rep(c(1:4), each = 30))

# Third and finally, calculate the average monthly mean sea level rise
# by quarter to make it compatible with the representative data
mean_slr <- slr_clean_2 %>%
  group_by(year, quarter, number) %>%
  summarise(mean_msl = mean(monthly_msl)) %>%
  arrange(number) %>%
  ungroup() %>%
  select(year, mean_msl) %>%
  mutate(mean_mm = mean_msl*1000) %>%
  na.omit() %>%
  mutate(slr_rate = (mean(mean_mm)/sum(length(year))))

##
# Make the mean slr data into a csv
write.csv(mean_slr, "data/mean_slr.csv")


# mean_slr_year <- mean_slr %>%
#   group_by(year) %>%
#   summarise(mean_slr_year = mean(mean_mm))

# rows <- tibble(Year = rep(2011:2020, each=12)) %>%
#   group_by(Year) %>%
#   mutate(Month = rep(1:12))

# slr_clean %>% 
#   bind_rows(slr_clean %>% 
#               filter(Month %in% c(1:12)) %>%
#               summarise_if(is.numeric, )) %>% 
#   mutate(Month = ifelse(is.na(Month), "0", Month))



