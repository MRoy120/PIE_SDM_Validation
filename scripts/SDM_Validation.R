#'--------------------------
#' SDM Model Validation
#' August 14, 2021
#'-------------------------

##
# Load in Libraries
library(dplyr)
library(readr)
library(tidyr)
library(DescTools)
library(purrr)

##
# Load in Model Data ####
model <- read_csv("data/10_year_model_data.csv")
str(model)

## 
# Clean Model Data ####
# Select and rename only Years, Low Marsh Area, High Marsh Area,
# Total Marsh Area, Upland Border Extent, 
# Available area for Marsh Encroachment, Fiddler Crabs, and
# Rate of Sea Level Rise

model_clean <- model %>%
  select(`Years`, `Low Marsh Area`, `High Marsh Area`, `Total Marsh Area`,
         `Upland Border Extent`, `Available Area for Marsh Encroachment`, `Fiddler Crabs`,
         `Rate of Sea Level Rise`) %>%
  rename(time_step = Years,
         low_marsh_area = `Low Marsh Area`,
         high_marsh_area = `High Marsh Area`,
         total_marsh_area = `Total Marsh Area`,
         upland_extent = `Upland Border Extent`,
         marsh_encroach_area = `Available Area for Marsh Encroachment`,
         fiddler_crabs = `Fiddler Crabs`,
         sea_level_rise = `Rate of Sea Level Rise`) %>%
  filter(time_step != 236)

##
# Write it as a new cleaned csv
write.csv(model_clean, "data/10_year_model_data_clean.csv")

##
# Make long data and get the mean, variance, and covariance for each parameter
# Long Data
model_clean_long <- pivot_longer(model_clean,
                                 cols = low_marsh_area:sea_level_rise,
                                 names_to = "parameter",
                                 values_to = "parameter_values")

# Getting Mean Value
model_mean <- model_clean_long %>%
  group_by(parameter) %>%
  summarise(mean_param = mean(parameter_values))

##
# Getting Variance
model_variance <- model_clean_long %>%
  group_by(parameter) %>%
  summarise(var_param = var(parameter_values))

##
# Getting Covariance
# Get covariance values and convert to data frame using the wide clean data
model_covariance <- as.data.frame(cov(model_clean[,c(2:8)], model_clean[,c(2:8)]))

# Make initial variable column, rearrange to the front, and make a tibble
model_covariance <- model_covariance %>%
  mutate(initial_variable = rownames(model_covariance)) %>%
  select(initial_variable, low_marsh_area, high_marsh_area, total_marsh_area, 
          upland_extent, marsh_encroach_area, fiddler_crabs, sea_level_rise) %>%
  as_tibble()

# Convert to long data
model_covariance <- model_covariance %>%
  pivot_longer(cols = 2:8,
               names_to = "col_names",
               values_to = "covariance")

##
# Load in Representative Data ####
slr <- read_csv("data/mean_slr.csv")

##
# Clean Representative Data ####


##
# Theil's U ####

##
# Calculate Theil's U for Mean for SLR
slr_model <- model_clean %>%
  filter(time_step != 7.5)

TheilU(slr$slr_rate, slr_model$sea_level_rise, type=1)
TheilU(slr$slr_rate, slr_model$sea_level_rise, type=2)

##
# Calculate Theil's U for Variance


##
# Calculate Theil's U for CoVariance


# Fiddler Crabs ####
crab_data <- model_clean %>%
  select(time_step, fiddler_crabs) %>%
  filter(time_step %in% c(1, 2, 3, 4, 5, 6)) %>%
  mutate(fiddlers_model = fiddler_crabs,
         fiddlers_measured = c(1, 2, 1.5, 6, 3, 5.5))

TheilU(crab_data$fiddlers_measured, crab_data$fiddlers_model, type = 1)
TheilU(crab_data$fiddlers_measured, crab_data$fiddlers_model, type = 2)

fiddler_MSE <- mean((crab_data$fiddlers_measured - crab_data$fiddlers_model)^2)

fiddler_mod <- lm(data = crab_data, fiddlers_model ~ fiddlers_measured)
fiddler_rsq <- summary(fiddler_mod)$r.squared

U_M <- (((mean(crab_data$fiddlers_model)^2) - (mean(crab_data$fiddlers_measured)^2))/fiddler_MSE)
U_S <- (((sd(crab_data$fiddlers_model)^2) - (sd(crab_data$fiddlers_measured)^2))/fiddler_MSE)
U_C <- (((2*(1-fiddler_rsq))*(sd(crab_data$fiddlers_model)*sd(crab_data$fiddlers_measured)))/fiddler_MSE)

total_U <- (abs(U_M) + abs(U_S) + abs(U_C))
U_M2 <- abs(U_M/total_U)
U_S2 <- abs(U_S/total_U)
U_C2 <- abs(U_C/total_U)

crab_long <- tidyr::pivot_longer(data = crab_data,
                                 cols = fiddlers_model:fiddlers_measured,
                                 names_to = "data_type",
                                 values_to = "data")

library(ggplot2)
ggplot2::ggplot(data = crab_long, aes(x = time_step,
                                      y = data,
                                      color = data_type)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth()


