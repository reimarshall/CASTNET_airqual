# CASTNET_aIntroduction
The Clean Air Status and Trends Network (CASTNET) is an atmospheric monitoring program established by the 1990 Clean Air Act amendments to assess the effectiveness of emission reduction programs. The network tracks pollutant concentrations and other environmental deposits at 99 sites across the United States and Canada to gauge the current atmospheric health1. Pollutants such as carbon dioxide, nitrous oxide, and methane contribute to global warming through the “greenhouse effect,” when radiation from the sun is trapped and absorbed into the atmosphere rather than reflecting into outer space, raising the temperature of the earth’s atmosphere2. Research and data analysis on meteorological factors that exacerbate (or alleviate) poor air quality across geographical zones is necessary as climate change is proving fatal for millions across the globe3. Poor air quality is linked to respiratory illnesses, cancer, and other chronic diseases, and accurate data on the most effective emission reduction programs is important for progress toward reducing climate change rates and global disease burden. In this code build, we leverage CASTNET datasets, which support the evaluation of primary and secondary National Ambient Air Quality Standards, to identify meteorological risk factors for air quality across CASTNET sites and compare mean ozone and key trace gas levels among different study locations.
Methods
Negative values in the air quality data, often resulting from sensor noise, calibration issues, or measurements near the detection limit, were carefully handled to maintain data integrity. We replaced negative concentrations with zero, a common approach in air quality analysis, assuming the pollutant was not present in those instances.4 Additionally, we managed outliers in trace gas levels and temperature data to reduce distortion in our analysis.
We analyzed correlations between ozone and each meteorological factor, extending this analysis to trace gases with sufficient data—carbon monoxide (CO), nitrogen oxides (NO), sulfur dioxide (SO₂), and reactive nitrogen oxides (NOy). These gases were chosen based on data availability; others lacked sufficient data for linear regression analysis. Following, we conducted a multivariate linear regression analysis using selected meteorological factors as independent variables to evaluate their combined and individual effects on each gas level. A multicollinearity assessment was done in order to select the best meteorological predictors on each model. 
In our analysis of mean gas levels by zone (SITE_ID), we examined the distribution of each gas to ensure robust comparisons. We tested for normality in gas level distributions, a key assumption for accurately interpreting mean differences across zones, and assessed homoscedasticity. Based on these results, we selected either parametric tests for normally distributed data or nonparametric tests when normality was not met. 
Results
Meteorological factors and air quality
Table 1 presents the adjusted correlation coefficients (aCoeff) between levels of ozone and trace gases (CO, NO, NOY, SO₂) and various meteorological factors, indicating statistically significant relationships (p<0.005) in several cases:
Ozone: Ozone levels are positively correlated with temperature (0.525) and solar radiation (0.0079), while showing a significant negative correlation with relative humidity (-0.357) and wetness (-0.3148).
Carbon monoxide (CO): CO levels exhibit significant positive correlations with temperature (1.2190) and precipitation (1.3826), and a strong negative correlation with wind speed (-1.8835) and wind direction (-0.0356).
Nitrogen Oxide (NO): NO levels are significantly affected by temperature (-9.06e-3), solar radiation (5.10e-4), precipitation (-1.53e-2), windspeed (-2.53e-2), and wind direction (-2.95e-4), indicating that these meteorological factors play a substantial role in its distribution.
Reactive oxides of Nitrogen (NOy): NOY levels have a weak negative relationship with temperature (-0.0198), solar radiation (-0.0019), precipitation (-0.2215), windspeed (-0.3956), wind direction (-0.0089), and wetness (-0.5581), suggesting that these factors generally reduce NOY concentrations.
Sulfur dioxide (SO₂): SO₂ levels show a slight positive correlation with temperature (0.0152) and wind speed (0.0736), but a significant negative correlation with relative humidity (-0.0087) and wetness (0.3321), implying that humid conditions and high wetness levels decrease SO₂ concentration. 
These adjusted correlations highlight the influence of temperature, humidity, windspeed, and other meteorological factors on the levels of ozone and trace gases, with both positive and negative associations depending on the gas and factor involved. 
Geographical zones and air quality
Table 2 shows the average levels of ozone and trace gases (CO, NO, NOY, SO₂) in parts per billion (ppb) across various study sites (SITE_ID). Ozone levels vary significantly by location, with Pinedale, WY (46.25 ppb) and Rocky Mountain National Park, CO (46.16 ppb) showing the highest averages, while Bondville, IL (32.03 ppb) has the lowest. CO levels, where available, also differ markedly, with Great Smoky Mountains, TN (165.36 ppb) showing the highest concentration. NO levels are generally low across sites, but Canyonlands National Park, UT (2.01 ppb) and Great Smoky Mountains, TN (2.16 ppb) report higher values compared to others. Significant variations are observed for NOY and SO₂ levels, with notable concentrations in Great Smoky Mountains, TN and Mackville, KY. All p-values are extremely low (<2e-16), indicating statistically significant differences in mean gas levels across sites. The F-statistics further confirm these differences across study sites for each gas.

Discussion
Ozone emissions have disastrous effects on human health and livelihood, as indicated by the results of the CASTNET monitoring. For example, a decrease in precipitation, associated with increased ozone in the atmosphere, will harm crop production. Aside from the obvious famine and job loss associated with a decreased amount of viable food crops cultivated by humans, low precipitation impacts plants that serve as food sources for many insects and other small animals, therefore impacting the entire food chain. Mirzabaev et al. described the converging impact of climate change related to drought and precipitation on low crop quantity, and how these factors converge to cause problems with hunger and adequate nutrition 5. (Figure 1)

Humidity is also related to human adaptability to climate change, as humidity in the atmosphere increases the severity of water-related natural disasters such as hurricanes and decreases human heat tolerance. Though one would expect that the environmental changes would lead to an increased amount of humidity, the data suggests that precipitation actually has a statistically significant negative correlation with increased ozone in the atmosphere. The data is in agreement with the National Center for Atmospheric Research’s recent findings related to moisture in dry regions of the United States; though atmospheric moisture was expected to increase based on the thermodynamic climate change models, the United States is experiencing a decrease in atmospheric moisture. The dryness may accelerate drought and encourage wildfire, and future research about the relationship between atmospheric pollutants and humidity is needed to understand this occurrence6.
Conclusion
Current literature and analysis highlight the critical impact of air quality on both public health and climate change, emphasizing the harmful effects of air pollutants. In this study, we present statistical correlations between meteorological factors, ozone concentrations, and trace gas levels, along with average pollutant levels across seven study sites. Our findings reveal a strong relationship between ozone concentrations and temperature, suggesting an underlying mechanism contributing to climate change. Understanding this relationship is essential for developing effective solutions and serves as a predictive tool to estimate the future impacts of climate change if mitigation efforts are delayed or neglected.
References
Clean Air Status and Trends Network (CASTNET), www3.epa.gov/castnet/docs/CASTNET-Factsheet-2021.pdf. Accessed 2 Nov. 2024. 
“Overview of Greenhouse Gases | US EPA.” Environmental Protection Agency (EPA), 11 April 2024, https://www.epa.gov/ghgemissions/overview-greenhouse-gases. Accessed 3 November 2024.
“Air Pollution and Your Health.” National Institute of Environmental Health Sciences, U.S. Department of Health and Human Services, www.niehs.nih.gov/health/topics/agents/air-pollution. Accessed 2 Nov. 2024. 
Jiang N, Akter R, Ross G, White S, Kirkwood J, Gunashanhar G, Thompson S, Riley M, Azzi M. On thresholds for controlling negative particle (PM2.5) readings in air quality reporting. Environ Monit Assess. 2023 Sep 12;195(10):1187. doi: 10.1007/s10661-023-11750-4. PMID: 37698727; PMCID: PMC10497433.
Alisher Mirzabaev, Rachel Bezner Kerr, Toshihiro Hasegawa, Prajal Pradhan, Anita Wreford, Maria Cristina Tirado von der Pahlen, Helen Gurney-Smith, Severe climate change risks to food security and nutrition, Climate Risk Management, Volume 39, 2023, 100473, ISSN 2212-0963, https://doi.org/10.1016/j.crm.2022.100473.
Hosansky, David. “Climate change isn't producing expected increase in atmospheric moisture over dry regions | NCAR & UCAR News.” News, 17 January 2024, https://news.ucar.edu/132936/climate-change-isnt-producing-expected-increase-atmospheric-moisture-over-dry-regions. Accessed 3 November 2024.irqual
Multivariate linear regression analysis of air quality among CASTNET sites

# CODE:

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
