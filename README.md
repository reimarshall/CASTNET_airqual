# CASTNET_airqual
Multivariate linear regression analysis of air quality among CASTNET sites
# close all, clear all
rm(list=ls())
graphics.off()

# set working directory
getwd()
setwd("/Users/reisplace/Desktop/YaleBiostats")

# Install required packages
#install.packages("readxl") # for reading Excel files
#install.packages("dplyr")  # for data manipulation
#install.packages("doBy")
#install.packages("corrplot")
#install.packages("ggcorrplot")
#install.packages("car")
#install.packages("psych")

# Load the necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(doBy)
library(ggcorrplot)
library(reshape2)
library(car)
library(psych)

# 0. reading datasets from file
ozone_data = read.csv("ozone_2023.csv")
met_data = read.csv("metdata_2023.csv")
gas_data = read.csv("hourly_gas_2023.csv")
sites_data = read_xlsx("activesuspendedcastnetsites.xlsx")

# Merging datasets by common keys
merged_data = met_data %>%
  inner_join(ozone_data, by = c("SITE_ID", "DATE_TIME"))
merged_data = merged_data %>%
  inner_join(gas_data, by = c("SITE_ID", "DATE_TIME"))

#________________________________________________________________________________________________________________________________________________________________________________________________________

#1. Are there meteorological factors that exacerbate (or ameliorate) air quality?

##1.OZONE LEVELS VS METEOROLOGICAL DATA

###Negative values in air quality data are often artifacts from sensor noise, calibration issues, or low-concentration measurements near the detection limit. Handling these values requires careful consideration to maintain data integrity.
###By excluding negative gas values, we ensure that our analysis focuses only on physically meaningful measurements, leading to more accurate insights into the relationship between meteorological factors and air quality.
####Set to Zero: Since negative concentrations are not physically possible, you can replace negative values with zero, which assumes that the pollutant is not present in those cases. This method is simple and commonly used in air quality analysis.
merged_data = merged_data %>%
  mutate(VALUE = ifelse(VALUE < 0, 0, VALUE))

####removing outliers: 
# Calculate Q1, Q3, and IQR for the TEMPERATURE column
Q1_temp <- quantile(merged_data$TEMPERATURE, 0.25, na.rm = TRUE)
Q3_temp <- quantile(merged_data$TEMPERATURE, 0.75, na.rm = TRUE)
IQR_temp <- Q3_temp - Q1_temp
# Define the lower and upper bounds based on IQR
lower_bound_temp <- Q1_temp - 1.5 * IQR_temp
upper_bound_temp <- Q3_temp + 1.5 * IQR_temp
# Filter the data based on these bounds for TEMPERATURE
merged_data <- merged_data %>% 
  filter(TEMPERATURE >= lower_bound_temp & TEMPERATURE <= upper_bound_temp)


###Selecting relevant columns: ozone and meteorological factors
air_quality_vars = "OZONE.y"
met_vars = c("TEMPERATURE", "RELATIVE_HUMIDITY", "SOLAR_RADIATION", "PRECIPITATION", "WINDSPEED", "WIND_DIRECTION", "WETNESS")
##excluded sigma-theta because it also includes information on wind direction --> source: https://www3.epa.gov/castnet/docs/CASTNET_Factsheet_2019.pdf

### Ensuring that all columns are numeric
merged_data = merged_data %>%
  mutate(across(all_of(c(air_quality_vars, met_vars)), as.numeric))

# Reshaping the data to long format for faceting
plot_data = merged_data %>%
  select(OZONE.y, all_of(met_vars)) %>%
  pivot_longer(cols = all_of(met_vars), names_to = "met_var", values_to = "Value")

# Plotting with facets
ggplot(plot_data, aes(x = Value, y = OZONE.y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Ozone Levels vs Meteorological Variables", 
       x = "Meteorological Variable Value", y = "Ozone Level(ppb)") +
  facet_wrap(~ met_var, scales = "free_x") +
  theme_minimal()

# Calculating correlation matrix
correlations = merged_data %>%
  select(all_of(c(air_quality_vars, met_vars))) %>%
  cor(use = "complete.obs")
print(correlations)
# Visualizing correlation matrix
ggcorrplot(correlations, lab = TRUE)


# Linear regression model for ozone with meteorological factors
ozone_model = lm(OZONE.y ~ TEMPERATURE + RELATIVE_HUMIDITY + SOLAR_RADIATION + PRECIPITATION + WINDSPEED + WIND_DIRECTION + WETNESS, data = merged_data)
summary(ozone_model)
##assessing multicolinearity
vif_values <- vif(ozone_model)
print(vif_values)
#___________________________________________________________________________________________________________________________________________________________

#Assessing correlations between meteorological parameters and types of trace-level gases
# Defining the meteorological variables
met_vars = c("TEMPERATURE", "RELATIVE_HUMIDITY", "SOLAR_RADIATION", "PRECIPITATION", "WINDSPEED", "WIND_DIRECTION", "WETNESS")

##Plotting the correlations between meteorological variables and trace-level gases
### Filter data for CO
CO_data = subset(merged_data, PARAMETER == "CO")
NO_data = subset(merged_data, PARAMETER == "NO")
NOY_data = subset(merged_data, PARAMETER == "NOY")
SO2_GA_data = subset(merged_data, PARAMETER == "SO2_GA")
_______________________________________________________________________________
####removing outliers: 
# Filter outliers for CO data
CO_data <- subset(merged_data, PARAMETER == "CO")
describe(CO_data$VALUE)
summary(CO_data$VALUE)
IQR_CO = IQR(CO_data$VALUE, na.rm = TRUE)
upper_bound_CO <- 177 + 1.5 * IQR_CO
# Filter the data based on these bounds for TEMPERATURE
CO_data <- CO_data %>% 
  filter(VALUE <= upper_bound_CO)

________________________________________________________________________________
# Filter outliers for NO data
NO_data <- subset(merged_data, PARAMETER == "NO")
NO_data <- NO_data %>% 
  filter(VALUE <= 900)
describe(NO_data$VALUE)
summary(NO_data$VALUE)

________________________________________________________________________________
# Filter outliers for NOY data
NOY_data <- subset(merged_data, PARAMETER == "NOY")
describe(NOY_data$VALUE)
summary(NOY_data$VALUE)

________________________________________________________________________________
# Filter outliers for SO2_GA data
SO2_GA_data <- subset(merged_data, PARAMETER == "SO2_GA")
describe(SO2_GA_data$VALUE)
summary(SO2_GA_data$VALUE)

_______________________________________________________________________________
### Reshaping the data to long format for easier plotting
CO_long = melt(CO_data, id.vars = "VALUE", measure.vars = met_vars, 
                variable.name = "Meteorological_Variable", value.name = "Value")
NO_long <- melt(NO_data, id.vars = "VALUE", measure.vars = met_vars, 
                variable.name = "Meteorological_Variable", value.name = "Value")
NOY_long <- melt(NOY_data, id.vars = "VALUE", measure.vars = met_vars, 
                 variable.name = "Meteorological_Variable", value.name = "Value")
SO2_GA_long <- melt(SO2_GA_data, id.vars = "VALUE", measure.vars = met_vars, 
                    variable.name = "Meteorological_Variable", value.name = "Value")

### Generate the combined plot with facets
plot_pollutant <- function(data_long, title) {
  ggplot(data_long, aes(x = Value, y = VALUE)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    facet_wrap(~ Meteorological_Variable, scales = "free_x") +
    labs(title = title,
         x = "Meteorological Variable Value", y = "Concentration") +
    theme_minimal()
}

### Generating plots for each pollutant
plot_pollutant(CO_long, "CO Concentration (ppb) vs Meteorological Variables")
plot_pollutant(NO_long, "NO Concentration (ppb) vs Meteorological Variables")
plot_pollutant(NOY_long, "NOy Concentration (ppb) vs Meteorological Variables")
plot_pollutant(SO2_GA_long, "SO2 Concentration (ppb) vs Meteorological Variables")


##Calculating the correlation values for each gas pollutant
### List of gases to analyze
CO_correlation_data = CO_data[, c("VALUE", met_vars)]
# Calculate correlation matrix
correlation_matrix <- cor(CO_correlation_data, use = "complete.obs")
# Display the correlation of VALUE with each meteorological variable
correlation_matrix["VALUE", ]



NO_correlation_data = NO_data[, c("VALUE", met_vars)]
# Calculate correlation matrix
correlation_matrix2 <- cor(NO_correlation_data, use = "complete.obs")
# Display the correlation of VALUE with each meteorological variable
correlation_matrix2["VALUE", ]

NOY_correlation_data = NOY_data[, c("VALUE", met_vars)]
# Calculate correlation matrix
correlation_matrix3 <- cor(NOY_correlation_data, use = "complete.obs")
# Display the correlation of VALUE with each meteorological variable
correlation_matrix3["VALUE", ]

SO2_correlation_data = SO2_GA_data[, c("VALUE", met_vars)]
# Calculate correlation matrix
correlation_matrix4 <- cor(SO2_correlation_data, use = "complete.obs")
# Display the correlation of VALUE with each meteorological variable
correlation_matrix4["VALUE", ]
________________________________________________________________________________
## Defining the formula for the regression model
formula = VALUE ~ TEMPERATURE + RELATIVE_HUMIDITY + SOLAR_RADIATION + PRECIPITATION + WINDSPEED + WIND_DIRECTION + WETNESS

# Fit models for each gas type by filtering the data for each parameter
CO_model = lm(formula, data = CO_data)
##assessing multicolinearity
vif_values2 <- vif(CO_model)
print(vif_values2)
________________________________________________________________________________
NO_model = lm(formula, data = NO_data)
##assessing multicolinearity
vif_values3 <- vif(NO_model)
print(vif_values3)
________________________________________________________________________________
NOY_model = lm(formula, data = NOY_data)
##assessing multicolinearity
vif_values4 <- vif(NOY_model)
print(vif_values4)
________________________________________________________________________________
SO2_GA_model = lm(formula, data = SO2_GA_data)
##assessing multicolinearity
vif_values5 <- vif(SO2_GA_model)
print(vif_values5)
________________________________________________________________________________
# Display summaries of each model
summary(CO_model)
summary(NO_model)
summary(NOY_model)
summary(SO2_GA_model)

#__________________________________________________________________________________________________________________________________________________________________________________________________

# close all, clear all
rm(list=ls())
graphics.off()

# 0. reading datasets from file
ozone_data = read.csv("ozone_2023.csv")
met_data = read.csv("metdata_2023.csv")
gas_data = read.csv("hourly_gas_2023.csv")
sites_data = read_xlsx("activesuspendedcastnetsites.xlsx")

# Merging datasets by common keys
merged_data = met_data %>%
  inner_join(gas_data, by = c("SITE_ID", "DATE_TIME"))
#__________________________________________________________________________________________________________________________________________________________________________________________________

#2.   Are there geographical zones that tend to have better or worse air quality?
____
#2.A
# Calculating average of ozone levels for each site
ozone = merged_data %>%
  group_by(SITE_ID) %>%
  summarize(avg_ozone = mean(OZONE.y, na.rm = TRUE))

# Histogram of OZONE values
ggplot(merged_data, aes(x = OZONE.y)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  labs(title = "Histogram of Ozone Values", x = "Ozone Value", y = "Frequency")
##data is normally distributed by visual methods


##Assessing normality for ozone levels by SITE_ID
ggplot(merged_data, aes(sample = OZONE.y)) +
  facet_wrap(~SITE_ID) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plots of OZONE.y by SITE_ID")
        # Histogram for each SITE_ID
        ggplot(merged_data, aes(x = OZONE.y)) +
          facet_wrap(~SITE_ID) +
          geom_histogram(bins = 30, color = "black", fill = "gray") +
          labs(title = "Histograms of OZONE.y by SITE_ID", x = "OZONE.y", y = "Frequency")
        # Perform Bartlett test for homoscedasticity
        bartlett_test_result1 <- bartlett.test(OZONE.y ~ SITE_ID, data = merged_data)
        # Display the result
        print(bartlett_test_result1)
        
        levene_test_result <- leveneTest(OZONE.y ~ SITE_ID, data = merged_data)
        print(levene_test_result)
        ##output: p<0.05 --> heterocedasticity

        # Perform Welch's ANOVA
        welch_anova_result <- oneway.test(OZONE.y ~ SITE_ID, data = merged_data, var.equal = FALSE)
        print(welch_anova_result)

___________________________________________________________
# Calculating daily average of CO levels for each site
CO = CO_data %>%
  group_by(SITE_ID) %>%
  summarize(average_CO = mean(VALUE, na.rm = TRUE))

# Histogram of CO values
ggplot(CO_data, aes(x = VALUE)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  labs(title = "Histogram of CO Values", x = "CO Value", y = "Frequency")
###data seems to be normally distributed by visual methods


##Assessing normality for CO levels by SITE_ID
ggplot(CO_data, aes(sample = VALUE)) +
  facet_wrap(~SITE_ID) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plots of CO by SITE_ID")

          # Histogram for each SITE_ID
          ggplot(CO_data, aes(x = VALUE)) +
            facet_wrap(~SITE_ID) +
            geom_histogram(bins = 30, color = "black", fill = "gray") +
            labs(title = "Histograms of CO by SITE_ID", x = "CO", y = "Frequency")

          # Perform Bartlett test for homoscedasticity
          bartlett_test_result2 <- bartlett.test(VALUE ~ SITE_ID, data = CO_data)
          # Display the result
          print(bartlett_test_result2)
          
          levene_test_result2 <- leveneTest(VALUE ~ SITE_ID, data = CO_data)
          print(levene_test_result2)
          ##output: p<0.05 --> heterocedasticity

# Perform Welch's ANOVA
welch_anova_result2 <- oneway.test(VALUE ~ SITE_ID, data = CO_data, var.equal = FALSE)
print(welch_anova_result2)


___________________________________________________________
# Calculating daily average of NO levels for each site
NO = NO_data %>%
  group_by(SITE_ID) %>%
  summarize(average_NO = mean(VALUE, na.rm = TRUE))

# Histogram of NO values
ggplot(NO_data, aes(x = VALUE)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  labs(title = "Histogram of NO Values", x = "NO Value", y = "Frequency") +
  xlim (0,1)
###data does not seem to be normally distributed by visual methods

# Histogram of NO values
ggplot(NO_data, aes(x = VALUE)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  labs(title = "Histogram of NO Values", x = "NO Value", y = "Frequency") +
  xlim (0,1) +
  ylim(0, 5500)
###data does not seem to be normally distributed by visual methods

##Assessing normality for NO levels by SITE_ID
ggplot(NO_data, aes(sample = VALUE)) +
  facet_wrap(~SITE_ID) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plots of NO by SITE_ID") +
  xlim (0,1) +
  ylim(0, 2)

# Histogram for each SITE_ID
ggplot(NO_data, aes(x = VALUE)) +
  facet_wrap(~SITE_ID) +
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  labs(title = "Histograms of NO by SITE_ID", x = "NO", y = "Frequency") +
  xlim (0,1) +
  ylim (0,10)

# Perform Bartlett test for homoscedasticity
bartlett_test_result3 <- bartlett.test(VALUE ~ SITE_ID, data = NO_data)
# Display the result
print(bartlett_test_result3)

levene_test_result3 <- leveneTest(VALUE ~ SITE_ID, data = NO_data)
print(levene_test_result3)
##output: p<0.05 --> heterocedasticity

# Perform Welch's ANOVA
welch_anova_result3 <- oneway.test(VALUE ~ SITE_ID, data = NO_data, var.equal = FALSE)
print(welch_anova_result3)

___________________________________________________________
# Calculating daily average of NOY levels for each site
NOY = NOY_data %>%
  group_by(SITE_ID) %>%
  summarize(average_NOY = mean(VALUE, na.rm = TRUE))

# Histogram of NOY values
ggplot(NOY_data, aes(x = VALUE)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  labs(title = "Histogram of NOY Values", x = "NOY Value", y = "Frequency") +
  xlim (0,10)
###data does not seem to be normally distributed by visual methods


##Assessing normality for NOY levels by SITE_ID
ggplot(NOY_data, aes(sample = VALUE)) +
  facet_wrap(~SITE_ID) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plots of NOY by SITE_ID") +
  xlim (0,10)

# Histogram for each SITE_ID
ggplot(NOY_data, aes(x = VALUE)) +
  facet_wrap(~SITE_ID) +
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  labs(title = "Histograms of NOY by SITE_ID", x = "NOY", y = "Frequency") +
  xlim (0,10)

# Perform Bartlett test for homoscedasticity
bartlett_test_result4 <- bartlett.test(VALUE ~ SITE_ID, data = NOY_data)
# Display the result
print(bartlett_test_result4)

levene_test_result4 <- leveneTest(VALUE ~ SITE_ID, data = NOY_data)
print(levene_test_result4)
##output: p<0.05 --> heterocedasticity

# Perform Welch's ANOVA
welch_anova_result4 <- oneway.test(VALUE ~ SITE_ID, data = NOY_data, var.equal = FALSE)
print(welch_anova_result4)

___________________________________________________________
# Calculating daily average of SO2_GA levels for each site
SO2 = SO2_GA_data %>%
  group_by(SITE_ID) %>%
  summarize(average_SO2 = mean(VALUE, na.rm = TRUE))

# Histogram of SO2 values
ggplot(SO2_GA_data, aes(x = VALUE)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  labs(title = "Histogram of SO2 Values", x = "SO2 Value", y = "Frequency") +
  xlim (0,3)
###data does not seem to be normally distributed by visual methods


##Assessing normality for SO2 levels by SITE_ID
ggplot(SO2_GA_data, aes(sample = VALUE)) +
  facet_wrap(~SITE_ID) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plots of NOY by SITE_ID") 

# Histogram for each SITE_ID
ggplot(SO2_GA_data, aes(x = VALUE)) +
  facet_wrap(~SITE_ID) +
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  labs(title = "Histograms of SO2 by SITE_ID", x = "SO2", y = "Frequency") +
  xlim (0,10) +
  ylim (0,1500)
  

# Perform Bartlett test for homoscedasticity
bartlett_test_result5 <- bartlett.test(VALUE ~ SITE_ID, data = SO2_GA_data)
# Display the result
print(bartlett_test_result5)

levene_test_result5 <- leveneTest(VALUE ~ SITE_ID, data = SO2_GA_data)
print(levene_test_result5)
##output: p<0.05 --> heterocedasticity

# Perform Welch's ANOVA
welch_anova_result5 <- oneway.test(VALUE ~ SITE_ID, data = SO2_GA_data, var.equal = FALSE)
print(welch_anova_result5)
