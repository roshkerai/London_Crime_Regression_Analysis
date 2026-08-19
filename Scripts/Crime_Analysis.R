# Install packages 
library(readr) # Need to read in the csv files used in the code
install.packages("MASS")        # For Negative binomial regression
install.packages("betareg")     # For Beta regression
install.packages("statmod")     # For Beta regression
install.packages("carData")     # For Durbin-Watson test (and car library)
install.packages("knitr")       # To create table in R
install.packages("dplyr")       # For log linearity assumption mutate function (and chloropleth map)
install.packages("ggplot2")     # For plots using ggplot
install.packages("tidyr")       # For log linearity assumption
install.packages("ggcorrplot")  # For creating correlation plot
install.packages("gridExtra")   # For arrange many plots in one frame
install.packages("performance") # For VIF calculations for multicollinearity assumption
install.packages("survival")    # For assumption of beta regression
install.packages("AER")         # For overdispersion test (Cameron and Trivedi)
install.packages("sf")          # For shapefiles to create chloropleth map
install.package("tmap")         # For choropleth map
install.packages("plotly")      # For creating interactive plot
install.packages("patchwork")   # For arrange plots in one frame

Crime <- read.csv("Files for R code/Crime Data for Regression Analysis.csv")
str(Crime)
View(Crime)

###################### Adjusting the different datasets #######################

# Different adjusted/scaled/original data frames
# colnames(Crime_Original) <- c('Region','Crime_rate_each_year_Per_Million','Year','Population_In_Millions','Jobseekers_Claimants_Rate','Median_House_Price','Average_Income','Job_Density')

Crime_original <- Crime
colnames(Crime_original) <- c('Region','Crime_number_each_year','Year','Population','Jobseekers_Claimants_Percentage','Median_House_Price','Average_Income','Job_Density')

# Dataset 1: Crime number and Population Scaled
Crime_1 <- Crime
Crime_1$X2...Population <- (Crime_1$X2...Population)/1000000
Crime_1$Y...No..of.total.crimes.per.year <- (Crime_1$Y...No..of.total.crimes.per.year/Crime_1$X2...Population)
colnames(Crime_1) <- c('Region','Crime_rate_each_year_per_million','Year','Population_in_millions','Jobseekers_Claimants_Percentage','Median_House_Price','Average_Income','Job_Density')

# Dataset 2: Crime count, Population Scaled, Household income and average income scaled
Crime_2 <- Crime
Crime_2$X2...Population <- (Crime_2$X2...Population)/1000000
Crime_2$X4...Median.House.Price..Pounds. <- Crime_2$X4...Median.House.Price..Pounds./1000
Crime_2$X5...Average.Income.level..Pounds. <- Crime_2$X5...Average.Income.level..Pounds./1000
colnames(Crime_2) <- c('Region','Crime_number_each_year','Year','Population_in_millions','Jobseekers_Claimants_Percentage','Median_House_Price_in_thousands','Average_Income_in_thousands','Job_Density')

# Dataset 3: Min-Max Scaling of Crime (Mainly for betareg)
Crime_3 <- Crime
max_crime <- max(Crime_3$Y...No..of.total.crimes.per.year)
min_crime <- min(Crime_3$Y...No..of.total.crimes.per.year)
Crime_3$Y...No..of.total.crimes.per.year <- (Crime_3$Y...No..of.total.crimes.per.year - min_crime)/(max_crime - min_crime)
Crime_3$X2...Population <- (Crime_3$X2...Population)/1000000
Crime_3$X4...Median.House.Price..Pounds. <- Crime_3$X4...Median.House.Price..Pounds./1000
Crime_3$X5...Average.Income.level..Pounds. <- Crime_3$X5...Average.Income.level..Pounds./1000
colnames(Crime_3) <- c('Region','Crime_rate_each_year_scaled','Year','Population_in_millions','Jobseekers_Claimants_Percentage','Median_House_Price_in_thousands','Average_Income_in_thousands','Job_Density')

# Dataset 4: Scaled in thousands and millions
Crime_4 <- Crime
Crime_4$Y...No..of.total.crimes.per.year <- (Crime_4$Y...No..of.total.crimes.per.year)/1000
Crime_4$X2...Population <- (Crime_4$X2...Population)/1000000
Crime_4$X4...Median.House.Price..Pounds. <- Crime_4$X4...Median.House.Price..Pounds./1000
Crime_4$X5...Average.Income.level..Pounds. <- Crime_4$X5...Average.Income.level..Pounds./1000
colnames(Crime_4) <- c('Region','Crime_number_each_year_in_thousands','Year','Population_in_millions','Jobseekers_Claimants_Percentage','Median_House_Price_in_thousands','Average_Income_in_thousands','Job_Density')


# Subsetting data to get seperate dataset for each region
central_data_original <- subset(Crime_original, Region == "Central")
north_data_original <- subset(Crime_original, Region == "North")
south_data_original <- subset(Crime_original, Region == "South")
east_data_original <- subset(Crime_original, Region == "East")
west_data_original <- subset(Crime_original, Region == "West")

# Subsetting data to get seperate dataset for each region (For Crime_1)
central_data_1 <- subset(Crime_1, Region == "Central")
north_data_1 <- subset(Crime_1, Region == "North")
south_data_1 <- subset(Crime_1, Region == "South")
east_data_1 <- subset(Crime_1, Region == "East")
west_data_1 <- subset(Crime_1, Region == "West")

# Subsetting data to get seperate dataset for each region (For Crime_2)
central_data_2 <- subset(Crime_2, Region == "Central")
north_data_2 <- subset(Crime_2, Region == "North")
south_data_2 <- subset(Crime_2, Region == "South")
east_data_2 <- subset(Crime_2, Region == "East")
west_data_2 <- subset(Crime_2, Region == "West")

# Subsetting data to get seperate dataset for each region (For Crime_3)
central_data_3 <- subset(Crime_3, Region == "Central")
north_data_3 <- subset(Crime_3, Region == "North")
south_data_3 <- subset(Crime_3, Region == "South")
east_data_3 <- subset(Crime_3, Region == "East")
west_data_3 <- subset(Crime_3, Region == "West")

# Subsetting data to get seperate dataset for each region (For Crime_4)
central_data_4 <- subset(Crime_4, Region == "Central")
north_data_4 <- subset(Crime_4, Region == "North")
south_data_4 <- subset(Crime_4, Region == "South")
east_data_4 <- subset(Crime_4, Region == "East")
west_data_4 <- subset(Crime_4, Region == "West")

# Following regression for Research Question 2
############### Beta Regression Models for each individual area #################
library(betareg) # Needed for betareg function
library(statmod) # Needed for betareg function 
betareg_central <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = central_data_3)
summary(betareg_central)

betareg_north <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = north_data_3)
summary(betareg_north)

betareg_south <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = south_data_3)
summary(betareg_south)

betareg_east <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = east_data_3)
summary(betareg_east)

betareg_west <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = west_data_3)
summary(betareg_west)

################### Count Regression Models for each area ######################

# Poisson Regression models for each individual area
poisreg_central <-glm(formula = Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = central_data_2, family = poisson) 
print(summary(poisreg_central))

poisreg_north <-glm(formula = Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = north_data_2, family = poisson) 
print(summary(poisreg_north))

poisreg_south <-glm(formula = Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = south_data_2, family = poisson) 
print(summary(poisreg_south))

poisreg_east <-glm(formula = Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = east_data_2, family = poisson) 
print(summary(poisreg_east))

poisreg_west <-glm(formula = Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = west_data_2, family = poisson) 
print(summary(poisreg_west))


# Negative Binomial Regression models for each individual area
require(MASS) # Needed for glm.nb function

nbreg_central <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = central_data_2)
print(summary(nbreg_central))

nbreg_north <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = north_data_2)
print(summary(nbreg_north))

nbreg_south <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = south_data_2)
print(summary(nbreg_south))

nbreg_east <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = east_data_2)
print(summary(nbreg_east))

nbreg_west <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = west_data_2)
print(summary(nbreg_west))

# For Research Question 3
########################## Region interpretation ###############################
# For research question 3
Crime_2$Region <- as.factor(Crime_2$Region)

nbreg_no_region <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = Crime_2)
print(summary(nbreg_no_region))

nbreg_region <- glm.nb(Crime_number_each_year ~ Region + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = Crime_2)
print(summary(nbreg_region))

anova(nbreg_no_region, nbreg_region, test = "Chisq")
    
############# Model Validity assumptions for poisson regression ################

# 1) Assumption of response being Poisson

# Main method 1: Histogram with Poisson distribution curve overlay

# Using scaled Crime values for visualization clarity. The underlying distribution remains unchanged
par(mfrow=c(2,3)) # Layout to have 5 plots at once

hist(central_data_4$Crime_number_each_year_in_thousands, breaks = 15, probability = TRUE,
     main = "Histogram of Crime Count with Poisson Overlay (Central)", col = "lightblue"
     , xlab = "Number of crimes (In thousands)")
lambda <- mean(central_data_4$Crime_number_each_year_in_thousands)
x_vals <- 0:max(central_data_4$Crime_number_each_year_in_thousands)
pois_probs <- dpois(x_vals, lambda = lambda)
points(x_vals, pois_probs, type = "h", col = "red", lwd = 2)

hist(north_data_4$Crime_number_each_year_in_thousands, breaks = 15, probability = TRUE,
     main = "Histogram of Crime Count with Poisson Overlay (North)", col = "lightblue"
     , xlab = "Number of crimes (In thousands)")
lambda <- mean(north_data_4$Crime_number_each_year_in_thousands)
x_vals <- 0:max(north_data_4$Crime_number_each_year_in_thousands)
pois_probs <- dpois(x_vals, lambda = lambda)
points(x_vals, pois_probs, type = "h", col = "red", lwd = 2)

hist(south_data_4$Crime_number_each_year_in_thousands, breaks = 15, probability = TRUE,
     main = "Histogram of Crime Count with Poisson Overlay (South)", col = "lightblue"
     , xlab = "Number of crimes (In thousands)")
lambda <- mean(south_data_4$Crime_number_each_year_in_thousands)
x_vals <- 0:max(south_data_4$Crime_number_each_year_in_thousands)
pois_probs <- dpois(x_vals, lambda = lambda)
points(x_vals, pois_probs, type = "h", col = "red", lwd = 2)

hist(east_data_4$Crime_number_each_year_in_thousands, breaks = 15, probability = TRUE,
     main = "Histogram of Crime Count with Poisson Overlay (East)", col = "lightblue"
     , xlab = "Number of crimes (In thousands)")
lambda <- mean(east_data_4$Crime_number_each_year_in_thousands)
x_vals <- 0:max(east_data_4$Crime_number_each_year_in_thousands)
pois_probs <- dpois(x_vals, lambda = lambda)
points(x_vals, pois_probs, type = "h", col = "red", lwd = 2)

hist(west_data_4$Crime_number_each_year_in_thousands, breaks = 15, probability = TRUE,
     main = "Histogram of Crime Count with Poisson Overlay (West)", col = "lightblue"
     , xlab = "Number of crimes (In thousands)")
lambda <- mean(west_data_4$Crime_number_each_year_in_thousands)
x_vals <- 0:max(west_data_4$Crime_number_each_year_in_thousands)
pois_probs <- dpois(x_vals, lambda = lambda)
points(x_vals, pois_probs, type = "h", col = "red", lwd = 2)

par(mfrow=c(1,1))

# Main Method 2: Q-Q plots for each region
par(mfrow=c(2,3))

qqnorm(central_data_2$Crime_number_each_year, main="QQ Plot of Crime (Central)")
qqline(central_data_2$Crime_number_each_year, col = "steelblue", lwd = 2)

qqnorm(north_data_2$Crime_number_each_year, main="QQ Plot of Crime (North)")
qqline(north_data_2$Crime_number_each_year, col = "steelblue", lwd = 2)

qqnorm(south_data_2$Crime_number_each_year, main="QQ Plot of Crime (South)")
qqline(south_data_2$Crime_number_each_year, col = "steelblue", lwd = 2)

qqnorm(east_data_2$Crime_number_each_year, main="QQ Plot of Crime (East)")
qqline(east_data_2$Crime_number_each_year, col = "steelblue", lwd = 2)

qqnorm(west_data_2$Crime_number_each_year, main="QQ Plot of Crime (West)")
qqline(west_data_2$Crime_number_each_year, col = "steelblue", lwd = 2)

par(mfrow=c(1,1))

# 2) Assumption of Independence 
# Method 1: Autocorrelation plot
# Data to Time series
central_ts <- ts(central_data_4$Crime_number_each_year_in_thousands,start = 2001, end = 2013, frequency = 1)
north_ts <- ts(north_data_4$Crime_number_each_year_in_thousands,start = 2001, end = 2013, frequency = 1)
south_ts <- ts(south_data_4$Crime_number_each_year_in_thousands,start = 2001, end = 2013, frequency = 1)
east_ts <- ts(east_data_4$Crime_number_each_year_in_thousands,start = 2001, end = 2013, frequency = 1)
west_ts <- ts(west_data_4$Crime_number_each_year_in_thousands,start = 2001, end = 2013, frequency = 1)

par(mfrow=c(2,3))
# Autocorrelation plots
acf(central_ts, main = "Autocorrelation for Central")
acf(north_ts, main = "Autocorrelation for North")
acf(south_ts, main = "Autocorrelation for South")
acf(east_ts, main = "Autocorrelation for East")
acf(west_ts, main = "Autocorrelation for West")

par(mfrow=c(1,1))

# Mathod 2: Durbin-Watson test
library(car)

durbinWatsonTest(poisreg_central)
durbinWatsonTest(poisreg_north)
durbinWatsonTest(poisreg_south)
durbinWatsonTest(poisreg_east)
durbinWatsonTest(poisreg_west)


# 3) Assumption of Mean = Variance
mean(central_data_2$Crime_number_each_year)
var(central_data_2$Crime_number_each_year)
mean(north_data_2$Crime_number_each_year)
var(north_data_2$Crime_number_each_year)
mean(south_data_2$Crime_number_each_year)
var(south_data_2$Crime_number_each_year)
mean(east_data_2$Crime_number_each_year)
var(east_data_2$Crime_number_each_year)
mean(west_data_2$Crime_number_each_year)
var(west_data_2$Crime_number_each_year)

regions <- c("Central", "North", "South", "East", "West")
means <- c(219959.6, 107399.8, 148997, 228315.6, 151708.3)
variances <- c(634893540, 123538041, 276356312, 517415336, 180653772) 
crime_mean_var <- data.frame(Region = regions, Mean = means, Variance = variances)
library(knitr)
kable(crime_mean_var, caption = "Mean and Variance of Crime for Each Region")

# 4) Assumption of log-linearity
library(dplyr) # For mutate function
library(ggplot2)
library(tidyr)

# Main Method 1: Plotting Log(Mean crime count) against the predictors for all regions
# Reshape the dataset to long format (if your data is wide)
crime_long <- Crime_2 %>%
  pivot_longer(cols = -c(Region, Year, Crime_number_each_year),  # Keep Region, Year, and Crime_Count
               names_to = "Predictor", values_to = "Predictor_Value") %>%
  group_by(Region, Year, Predictor, Predictor_Value) %>%
  summarise(mean_crime = mean(Crime_number_each_year), .groups = "drop") %>% # Taking the mean
  mutate(log_mean_crime = log(mean_crime))  # Apply log transformation to mean crime count

# Faceted plot: Log(Mean Crime Count) vs Predictors over Time for Each Region
ggplot(crime_long, aes(x = Predictor_Value, y = log_mean_crime, color = as.factor(Year))) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(group = Region), method = "lm", se = FALSE, linetype = "dashed", color = "black") +  
  facet_grid(Region ~ Predictor, scales = "free_x") +  # Facet by Region & Predictor
  labs(title = "Log(Mean Crime Count) vs Predictors Over the years (by Region)",
       x = "Predictor Value",
       y = "Log(Mean Crime Count)",
       color = "Year") +
  theme_minimal()

# Method 2: Residual plots
par(mfrow=c(2,3))

plot(poisreg_central$fitted.values, residuals(poisreg_central, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (Central)")
abline(h = 0, col = "red")

plot(poisreg_north$fitted.values, residuals(poisreg_north, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (North)")
abline(h = 0, col = "red")

plot(poisreg_south$fitted.values, residuals(poisreg_south, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (South)")
abline(h = 0, col = "red")

plot(poisreg_east$fitted.values, residuals(poisreg_east, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (East)")
abline(h = 0, col = "red")

plot(poisreg_west$fitted.values, residuals(poisreg_west, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (West)")
abline(h = 0, col = "red")

par(mfrow=c(1,1)) # Resets layout to having singular plot

######### Model validity assumptions for Negative binomial regression ##########

# 1) Assumption of no Multi-collinearity 

# Method 1: Correlation plots for each region (predictor variables only)
# Plots will be displayed after running all lines for these plots (5 plots in 1)
install.packages("ggcorrplot")
library(ggcorrplot)
# Correlation plot for central
central_corr <- central_data_4[,-1:-2]
colnames(central_corr) <- c('Year','Population','Jobseekers_Claimants','House_Price','Average_Income','Job_Density')

correlation_matrix_central <- cor(central_corr)
correlation_matrix_central
correlation_matrix_central <- round(correlation_matrix_central, digits = 3)
corrplot_central <- ggcorrplot(correlation_matrix_central, tl.srt=90, lab = TRUE, title = "Correlation Plot for Central") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


central_corr <- central_data_4[,-1:-2]
colnames(central_corr) <- c('Year','Population','Jobseekers_Claimants','House_Price','Average_Income','Job_Density')

correlation_matrix_central <- cor(central_corr)
correlation_matrix_central
correlation_matrix_central <- round(correlation_matrix_central, digits = 3)
corrplot_central <- ggcorrplot(correlation_matrix_central, tl.srt=90, lab = TRUE, title = "Correlation Plot for Central") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


# Correlation plot for north
north_corr <- north_data_4[,-1:-2]
colnames(north_corr) <- c('Year','Population','Jobseekers_Claimants','House_Price','Average_Income','Job_Density')

correlation_matrix_north <- cor(north_corr)
correlation_matrix_north
correlation_matrix_north <- round(correlation_matrix_north, digits = 3)
corrplot_north <- ggcorrplot(correlation_matrix_north, tl.srt=90, lab = TRUE, title = "Correlation Plot for North") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

# Correlation plot for south
south_corr <- south_data_4[,-1:-2]
colnames(south_corr) <- c('Year','Population','Jobseekers_Claimants','House_Price','Average_Income','Job_Density')

correlation_matrix_south <- cor(south_corr)
correlation_matrix_south
correlation_matrix_south <- round(correlation_matrix_south, digits = 3)
corrplot_south <- ggcorrplot(correlation_matrix_south, tl.srt=90, lab = TRUE, title = "Correlation Plot for South") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

# Correlation plot for east
east_corr <- east_data_4[,-1:-2]
colnames(east_corr) <- c('Year','Population','Jobseekers_Claimants','House_Price','Average_Income','Job_Density')

correlation_matrix_east <- cor(east_corr)
correlation_matrix_east 
correlation_matrix_east  <- round(correlation_matrix_east , digits = 3)
corrplot_east <- ggcorrplot(correlation_matrix_east , tl.srt=90, lab = TRUE, title = "Correlation Plot for East") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

# Correlation plot for west
west_corr <- west_data_4[,-1:-2]
colnames(west_corr) <- c('Year','Population','Jobseekers_Claimants','House_Price','Average_Income','Job_Density')

correlation_matrix_west <- cor(west_corr)
correlation_matrix_west 
correlation_matrix_west  <- round(correlation_matrix_west , digits = 3)
corrplot_west <- ggcorrplot(correlation_matrix_west , tl.srt=90, lab = TRUE, title = "Correlation Plot for West") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

install.packages("gridExtra")
library(gridExtra)
grid.arrange(corrplot_central, corrplot_north, corrplot_south, corrplot_east, corrplot_west, nrow = 2)

# Method 2: Variance Inflation Factor (VIF)

install.packages("performance")
library(performance)
check_collinearity(nbreg_central)
check_collinearity(nbreg_north)
check_collinearity(nbreg_south)
check_collinearity(nbreg_east)
check_collinearity(nbreg_west)

# Removing the 'Average Income' predictor
nbreg_central1 <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Job_Density, data = central_data_2)
print(summary(nbreg_central1))
nbreg_north1 <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage +  Median_House_Price_in_thousands + Job_Density, data = north_data_2)
print(summary(nbreg_north1))
nbreg_south1 <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage +  Median_House_Price_in_thousands + Job_Density, data = south_data_2)
print(summary(nbreg_south1))
nbreg_east1 <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage +  Median_House_Price_in_thousands + Job_Density, data = east_data_2)
print(summary(nbreg_east1))
nbreg_west1 <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage +  Median_House_Price_in_thousands + Job_Density, data = west_data_2)
print(summary(nbreg_west1))

check_collinearity(nbreg_central1)
check_collinearity(nbreg_north1)
check_collinearity(nbreg_south1)
check_collinearity(nbreg_east1)
check_collinearity(nbreg_west1)

################ Model Validity assumptions for Beta regression ################
install.packages("survival")
library(fitdistrplus)
library(ggplot2)

# 1) Assumption of response being beta distributed
# Method 1: Histogram with Beta regression curve overlay (by region)
# All plots will be executed after running all lines

y_scaled1 <- central_data_3$Crime_rate_each_year_scaled
fit1 <- fitdist(y_scaled1, "beta", method = "mle")
alpha_hat1 <- fit1$estimate["shape1"]
beta_hat1 <- fit1$estimate["shape2"]
beta_density1 <- function(x) dbeta(x, shape1 = alpha_hat1, shape2 = beta_hat1)
beta_hist_central <- ggplot(data.frame(y_scaled1), aes(x = y_scaled1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = beta_density1, color = "red", size = 1.2) +
  labs(title = "Histogram with Beta Distribution Curve (Central)",
       x = "Scaled Crime Rate", y = "Density") +
  theme_minimal()

y_scaled2 <- north_data_3$Crime_rate_each_year_scaled
fit2 <- fitdist(y_scaled2, "beta", method = "mle")
alpha_hat2 <- fit2$estimate["shape1"]
beta_hat2 <- fit2$estimate["shape2"]
beta_density2 <- function(x) dbeta(x, shape1 = alpha_hat2, shape2 = beta_hat2)
beta_hist_north <- ggplot(data.frame(y_scaled2), aes(x = y_scaled2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = beta_density2, color = "red", size = 1.2) +
  labs(title = "Histogram with Beta Distribution Curve (North)",
       x = "Scaled Crime Rate", y = "Density") +
  theme_minimal()

y_scaled3 <- south_data_3$Crime_rate_each_year_scaled
fit3 <- fitdist(y_scaled3, "beta", method = "mle")
alpha_hat3 <- fit3$estimate["shape1"]
beta_hat3 <- fit3$estimate["shape2"]
beta_density3 <- function(x) dbeta(x, shape1 = alpha_hat3, shape2 = beta_hat3)
beta_hist_south <- ggplot(data.frame(y_scaled3), aes(x = y_scaled3)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = beta_density3, color = "red", size = 1.2) +
  labs(title = "Histogram with Beta Distribution Curve (South)",
       x = "Scaled Crime Rate", y = "Density") +
  theme_minimal()

y_scaled4 <- east_data_3$Crime_rate_each_year_scaled
fit4 <- fitdist(y_scaled4, "beta", method = "mle")
alpha_hat4 <- fit4$estimate["shape1"]
beta_hat4 <- fit4$estimate["shape2"]
beta_density4 <- function(x) dbeta(x, shape1 = alpha_hat4, shape2 = beta_hat4)
beta_hist_east <- ggplot(data.frame(y_scaled4), aes(x = y_scaled4)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = beta_density4, color = "red", size = 1.2) +
  labs(title = "Histogram with Beta Distribution Curve (East)",
       x = "Scaled Crime Rate", y = "Density") +
  theme_minimal()

y_scaled5 <- west_data_3$Crime_rate_each_year_scaled
fit5 <- fitdist(y_scaled5, "beta", method = "mle")
alpha_hat5 <- fit5$estimate["shape1"]
beta_hat5 <- fit5$estimate["shape2"]
beta_density5 <- function(x) dbeta(x, shape1 = alpha_hat5, shape2 = beta_hat5)
beta_hist_west <- ggplot(data.frame(y_scaled5), aes(x = y_scaled5)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) + # Scaled histogram
  stat_function(fun = beta_density5, color = "red", size = 1.2) +
  labs(title = "Histogram with Beta Distribution Curve (West)",
       x = "Scaled Crime Rate", y = "Density") +
  theme_minimal()

library(gridExtra)
grid.arrange(beta_hist_central, beta_hist_north, beta_hist_south, beta_hist_east, beta_hist_west, nrow = 2)

# Method 2: QQ plot (same results as qqplots from poisson reg so no need to compute)

# 2) Assumption of no Multi-collinearity
# Method: Variance Inflation Factor (VIF)
library(performance)
check_collinearity(betareg_central)
check_collinearity(betareg_north)
check_collinearity(betareg_south)
check_collinearity(betareg_east)
check_collinearity(betareg_west)

# Removing the 'population' predictor
betareg_central1 <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Job_Density, data = central_data_3)
summary(betareg_central1)

betareg_north1 <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Job_Density, data = north_data_3)
summary(betareg_north1)

betareg_south1 <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Job_Density, data = south_data_3)
summary(betareg_south1)

betareg_east1 <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Job_Density, data = east_data_3)
summary(betareg_east1)

betareg_west1 <- betareg(Crime_rate_each_year_scaled ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Job_Density, data = west_data_3)
summary(betareg_west1)

check_collinearity(betareg_central1)
check_collinearity(betareg_north1)
check_collinearity(betareg_south1)
check_collinearity(betareg_east1)
check_collinearity(betareg_west1)

# 3) Assumption of linearity
# Residual plots
par(mfrow=c(2,3))
# using pearson as we don't have log link 
plot(betareg_central$fitted.values, residuals(betareg_central, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted (Central)")
abline(h = 0, col = "red")

plot(betareg_north$fitted.values, residuals(betareg_north, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted (North)")
abline(h = 0, col = "red")

plot(betareg_south$fitted.values, residuals(betareg_south, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted (South)")
abline(h = 0, col = "red")

plot(betareg_east$fitted.values, residuals(betareg_east, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted (East)")
abline(h = 0, col = "red")

plot(betareg_west$fitted.values, residuals(betareg_west, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted (West)")
abline(h = 0, col = "red")

par(mfrow=c(1,1)) # Resets layout to having singular plot

################ Testing whether the models were a good fit ####################
library(ggplot2)

# Fitting the count regression models

# Used crime4 instead of crime2 as the y needs to be scaled down
# Plots will be displayed after running all lines for these plots (5 plots in 1)
# Fitting both models to the data (Central)
central_data_4$Predicted_NB <- predict(nbreg_central, type = "response")/1000
central_data_4$Predicted_pois <- predict(poisreg_central, type = "response")/1000

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_central <- ggplot(central_data_4, aes(x = Year, y = Crime_number_each_year_in_thousands)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_pois, color = "Poisson")) +
  geom_line(aes(y = Predicted_NB, color = "NB"), linetype = "dashed") +
  labs(title = "Actual vs Fitted Crime Counts (Central)",
       y = "Crime Count (in thousands)") +
  scale_color_manual(values = c("Poisson" = "blue", "NB" = "red"), name = "Regression Method") +
  theme_minimal()  


# Fitting both models to the data (North)
north_data_4$Predicted_NB <- predict(nbreg_north, type = "response")/1000
north_data_4$Predicted_pois <- predict(poisreg_north, type = "response")/1000

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_north <-ggplot(north_data_4, aes(x = Year, y = Crime_number_each_year_in_thousands)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_pois, color = "Poisson")) +
  geom_line(aes(y = Predicted_NB, color = "NB"), linetype = "dashed") +
  labs(title = "Actual vs Fitted Crime Counts (North)",
       y = "Crime Count (in thousands)") +
  scale_color_manual(values = c("Poisson" = "blue", "NB" = "red"), name = "Regression Method") +
  theme_minimal()  


# Fitting both models to the data (South)
south_data_4$Predicted_NB <- predict(nbreg_south, type = "response")/1000
south_data_4$Predicted_pois <- predict(poisreg_south, type = "response")/1000

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_south <-ggplot(south_data_4, aes(x = Year, y = Crime_number_each_year_in_thousands)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_pois, color = "Poisson")) +
  geom_line(aes(y = Predicted_NB, color = "NB"), linetype = "dashed") +
  labs(title = "Actual vs Fitted Crime Counts (South)",
       y = "Crime Count (in thousands)") +
  scale_color_manual(values = c("Poisson" = "blue", "NB" = "red"), name = "Regression Method") +
  theme_minimal() 


# Fitting both models to the data (East)
east_data_4$Predicted_NB <- predict(nbreg_east, type = "response")/1000
east_data_4$Predicted_pois <- predict(poisreg_east, type = "response")/1000

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_east <-ggplot(east_data_4, aes(x = Year, y = Crime_number_each_year_in_thousands)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_pois, color = "Poisson")) +
  geom_line(aes(y = Predicted_NB, color = "NB"), linetype = "dashed") +
  labs(title = "Actual vs Fitted Crime Counts (East)",
       y = "Crime Count (in thousands)") +
  scale_color_manual(values = c("Poisson" = "blue", "NB" = "red"), name = "Regression Method") +
  theme_minimal() 


# Fitting both models to the data (West)
west_data_4$Predicted_NB <- predict(nbreg_west, type = "response")/1000
west_data_4$Predicted_pois <- predict(poisreg_west, type = "response")/1000

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_west <-ggplot(west_data_4, aes(x = Year, y = Crime_number_each_year_in_thousands)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_pois, color = "Poisson")) +
  geom_line(aes(y = Predicted_NB, color = "NB"), linetype = "dashed") +
  labs(title = "Actual vs Fitted Crime Counts (West)",
       y = "Crime Count (in thousands)") +
  scale_color_manual(values = c("Poisson" = "blue", "NB" = "red"), name = "Regression Method") +
  theme_minimal()

library(gridExtra)
grid.arrange(fitted_central, fitted_north, fitted_south, fitted_east, fitted_west, nrow = 2)

# Fitting the beta models

# Fitting beta model to the data (Central)
central_data_3$Predicted_Beta <- predict(betareg_central, type = "response")

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_central_beta <- ggplot(central_data_3, aes(x = Year, y = Crime_rate_each_year_scaled)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_Beta)) +
  labs(title = "Actual vs Fitted Crime Counts (Central)",
       y = "Crime Count (in thousands)") +
  theme_minimal()  

# Fitting beta model to the data (North)
north_data_3$Predicted_Beta <- predict(betareg_north, type = "response")

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_north_beta <- ggplot(north_data_3, aes(x = Year, y = Crime_rate_each_year_scaled)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_Beta)) +
  labs(title = "Actual vs Fitted Crime Counts (North)",
       y = "Crime Count (in thousands)") +
  theme_minimal()  

# Fitting beta model to the data (South)
south_data_3$Predicted_Beta <- predict(betareg_south, type = "response")

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_south_beta <- ggplot(south_data_3, aes(x = Year, y = Crime_rate_each_year_scaled)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_Beta)) +
  labs(title = "Actual vs Fitted Crime Counts (South)",
       y = "Crime Count (in thousands)") +
  theme_minimal()  

# Fitting beta model to the data (East)
east_data_3$Predicted_Beta <- predict(betareg_east, type = "response")

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_east_beta <- ggplot(east_data_3, aes(x = Year, y = Crime_rate_each_year_scaled)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_Beta)) +
  labs(title = "Actual vs Fitted Crime Counts (East)",
       y = "Crime Count (in thousands)") +
  theme_minimal()  

# Fitting beta model to the data (West)
west_data_3$Predicted_Beta <- predict(betareg_west, type = "response")

# Plot actual data + Negative Binomial fit + Poisson fit
fitted_west_beta <- ggplot(west_data_3, aes(x = Year, y = Crime_rate_each_year_scaled)) +
  geom_point() + # actual data points
  geom_line(aes(y = Predicted_Beta)) +
  labs(title = "Actual vs Fitted Crime Counts (West)",
       y = "Crime Count (in thousands)") +
  theme_minimal()  

grid.arrange(fitted_central_beta, fitted_north_beta, fitted_south_beta, fitted_east_beta, fitted_west_beta, nrow = 2)


# Residual plot for Negative binomial models

par(mfrow=c(2,3))
plot(nbreg_central$fitted.values, residuals(nbreg_central, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (Central)")
abline(h = 0, col = "red")

plot(nbreg_north$fitted.values, residuals(nbreg_north, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (North)")
abline(h = 0, col = "red")

plot(nbreg_south$fitted.values, residuals(nbreg_south, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (South)")
abline(h = 0, col = "red")

plot(nbreg_east$fitted.values, residuals(nbreg_east, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (East)")
abline(h = 0, col = "red")

plot(nbreg_west$fitted.values, residuals(nbreg_west, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Residuals vs Fitted (West)")
abline(h = 0, col = "red")

par(mfrow=c(1,1)) # Resets layout to having singular plot

# For Research Question 4
################### Impact of the 2008 Financial Crisis #######################
# Dummy variable for before or after 2008
# Assessing the impact of the financial crisis 
Crime_2$Crisis <- ifelse(Crime_2$Year >= 2008, 1, 0)

nbreg_crisis <- glm.nb(Crime_number_each_year ~ Crisis + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = Crime_2)
print(summary(nbreg_crisis))

central_data_2$Crisis <- ifelse(central_data_2$Year >= 2008, 1, 0)
north_data_2$Crisis <- ifelse(north_data_2$Year >= 2008, 1, 0)
south_data_2$Crisis <- ifelse(south_data_2$Year >= 2008, 1, 0)
east_data_2$Crisis <- ifelse(east_data_2$Year >= 2008, 1, 0)
west_data_2$Crisis <- ifelse(west_data_2$Year >= 2008, 1, 0)

nbreg_crisis_central <- glm.nb(Crime_number_each_year ~ Crisis + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = central_data_2)
print(summary(nbreg_crisis_central))

nbreg_crisis_north <- glm.nb(Crime_number_each_year ~ Crisis + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = north_data_2)
print(summary(nbreg_crisis_north))

nbreg_crisis_south <- glm.nb(Crime_number_each_year ~ Crisis + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = south_data_2)
print(summary(nbreg_crisis_south))

nbreg_crisis_east <- glm.nb(Crime_number_each_year ~ Crisis + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = east_data_2)
print(summary(nbreg_crisis_east))

nbreg_crisis_west <- glm.nb(Crime_number_each_year ~ Crisis + Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = west_data_2)
print(summary(nbreg_crisis_west))


# Interaction between Jobseekers and Crisis
nbreg_JSA_crisis_central <- glm.nb(Crime_number_each_year ~ Crisis*Jobseekers_Claimants_Percentage + Population_in_millions + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = central_data_2)
print(summary(nbreg_JSA_crisis_central))

nbreg_JSA_crisis_north <- glm.nb(Crime_number_each_year ~ Crisis*Jobseekers_Claimants_Percentage + Population_in_millions + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = north_data_2)
print(summary(nbreg_JSA_crisis_north))

nbreg_JSA_crisis_south <- glm.nb(Crime_number_each_year ~ Crisis*Jobseekers_Claimants_Percentage + Population_in_millions + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = south_data_2)
print(summary(nbreg_JSA_crisis_south))

nbreg_JSA_crisis_east <- glm.nb(Crime_number_each_year ~ Crisis*Jobseekers_Claimants_Percentage + Population_in_millions + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = east_data_2)
print(summary(nbreg_JSA_crisis_east))

nbreg_JSA_crisis_west <- glm.nb(Crime_number_each_year ~ Crisis*Jobseekers_Claimants_Percentage + Population_in_millions + Median_House_Price_in_thousands + Average_Income_in_thousands + Job_Density, data = west_data_2)
print(summary(nbreg_JSA_crisis_west))

# For Research Question 5
######################## Interaction model ###################################

nbreg_incomedensity_central <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands*Job_Density, data = central_data_2)
print(summary(nbreg_incomedensity_central))

nbreg_incomedensity_north <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands*Job_Density, data = north_data_2)
print(summary(nbreg_incomedensity_north))

nbreg_incomedensity_south <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands*Job_Density, data = south_data_2)
print(summary(nbreg_incomedensity_south))

nbreg_incomedensity_east <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands*Job_Density, data = east_data_2)
print(summary(nbreg_incomedensity_east))

nbreg_incomedensity_west <- glm.nb(Crime_number_each_year ~ Population_in_millions + Jobseekers_Claimants_Percentage + Median_House_Price_in_thousands + Average_Income_in_thousands*Job_Density, data = west_data_2)
print(summary(nbreg_incomedensity_west))

AIC(nbreg_central,nbreg_incomedensity_central)
AIC(nbreg_north,nbreg_incomedensity_north)
AIC(nbreg_south,nbreg_incomedensity_south)
AIC(nbreg_east,nbreg_incomedensity_east)
AIC(nbreg_west,nbreg_incomedensity_west)


############ Overdispersion methods ###############
library(MASS)

# Performing tests to see which model is better out of the 2 regressions

# AIC test
AIC(poisreg_central,nbreg_central)
AIC(poisreg_north,nbreg_north)
AIC(poisreg_south,nbreg_south)
AIC(poisreg_east,nbreg_east)
AIC(poisreg_west,nbreg_west)

# Dispersion statistic
# Values greater than 2 suggest overdispersion
dispersion_statistic_central <- sum(residuals(poisreg_central, type = "deviance")^2)/poisreg_central$df.residual
dispersion_statistic_central 
dispersion_statistic_north <- sum(residuals(poisreg_north, type = "deviance")^2)/poisreg_north$df.residual
dispersion_statistic_north
dispersion_statistic_south <- sum(residuals(poisreg_south, type = "deviance")^2)/poisreg_south$df.residual
dispersion_statistic_south
dispersion_statistic_east <- sum(residuals(poisreg_east, type = "deviance")^2)/poisreg_east$df.residual
dispersion_statistic_east
dispersion_statistic_west <- sum(residuals(poisreg_west, type = "deviance")^2)/poisreg_west$df.residual
dispersion_statistic_west


# Cameron & Trivedi dispersion test
# p-value < 0.05 suggest overdispersion
library(AER)
dispersiontest(poisreg_central) 
dispersiontest(poisreg_north)
dispersiontest(poisreg_south)
dispersiontest(poisreg_east)
dispersiontest(poisreg_west)

# Checking dispersion parameter
# If theta is not 0 then there is overdispersion
summary(nbreg_central) 
summary(nbreg_north) 
summary(nbreg_south) 
summary(nbreg_east) 
summary(nbreg_west) 

# All of the above tests suggest negative binomial is seen to be better due to overdispersion.

##### Box Plots ##############

library(ggplot2)

ggplot(Crime_4, aes(x = Region, y = Crime_number_each_year_in_thousands)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Crime Rate Distribution by Region",
       x = "Region",
       y = "Crime Rate")



########################### Descriptive Analysis ##############################

# Chloropleth Map

library(sf)
library(ggplot2)
library(dplyr)
library(tmap)
library(gridExtra)
library(grid)


crime_data <- read.csv("Files for R code/Files for Choropleth Map + Boroughs/met-police-recorded-offences-rates-ward - chloropleth map.csv")
shapefile_path <- "Files for R code/Files for Choropleth Map + Boroughs/London_Boroughs/London_Boroughs.shp"
london_map <- st_read(shapefile_path)

crime_data$Crime2001 <- as.numeric(gsub(",", "", crime_data$Crime2001)) # Remove the comma from the crime values
crime_data$Crime2013 <- as.numeric(gsub(",", "", crime_data$Crime2013)) # Remove the comma from the crime values

crime_2001_map <- crime_data[c(1:32),c(1:2)]
crime_2013_map <- crime_data[c(1:32),c(4:5)]

combined_range <- c(9000, 81000)  # min of mins, max of maxs
# Create consistent breaks (adjust number as needed)
breaks <- seq(9000, 81000, by = 24000)

# Need to rename specific boroughs to have the same format as my data frame
london_map <- london_map %>%
  mutate(BOROUGH = case_when(
    BOROUGH == "Richmond upon Thames" ~ "RichmondUponThames",
    BOROUGH == "Kensington & Chelsea" ~ "KensingtonChelsea",
    BOROUGH == "Kingston upon Thames" ~ "KingstonUponThames",
    BOROUGH == "Barking & Dagenham"   ~ "Barking and Dagenham",
    BOROUGH == "Hammersmith & Fulham" ~ "HammersmithFulham",
    BOROUGH == "Tower Hamlets"        ~ "TowerHamlets",
    TRUE ~ BOROUGH  # Keep all other names unchanged
  ))

# Creating 1st Choropleth map for crimes in 2001
london_map1 <- london_map %>%
  filter(BOROUGH != "City of London")
  
london_map1 <- london_map1 %>%       # Arrange the borough column in a-z order
  arrange(BOROUGH) 


names(london_map1)


colnames(crime_2001_map)[1] <- "BOROUGH"  # Ensuring the name matches exactly for both files

# Merge datasets
london_map1 <- london_map1 %>%
  left_join(crime_2001_map, by = "BOROUGH")

ggplot(london_map1) +
  geom_sf(aes(fill = crime_2001_map$Crime2001), color = "black") +
  geom_sf_text(aes(label = BOROUGH), size = 2.5, color = "white") +  # Add borough names
  scale_fill_viridis_c(option = "rocket", name = "No. of crimes",direction = -1,
                       limits = combined_range,
                       breaks = breaks) +  # Customize colour and add legend name
  theme_minimal()

# Making sure that the crime value column is part of the london map data, with all 32 boroughs.
names(london_map1)
names(crime_2001_map)
nrow(london_map1)
nrow(crime_2001_map)


# Compute the center of the borough in the map
london_map1$centroid <- st_centroid(london_map1$geometry)

# Extract X, Y coordinates of centroids
centroids <- st_coordinates(london_map1$centroid)
london_map1$x <- centroids[,1]
london_map1$y <- centroids[,2]# Plot with labels at centroids

ggplot(london_map1) +
  geom_sf(aes(fill = crime_2001_map$Crime2001), color = "black") +
  geom_text(aes(x = x, y = y, label = BOROUGH), size = 3, color = "white") +  
  scale_fill_viridis_c(option = "rocket", name = "No. of crimes",direction = -1,
                       limits = combined_range,
                       breaks = breaks) +  # Customize colour and add legend name
  theme_minimal()


# Step 1: Create a Borough ID column (1 to 32)
london_map1$Borough_ID <- 1:nrow(london_map1)  # Assign 1–32


# Step 3: Plot the Map with Numbers Instead of Borough Names (using above centroids)
map_plot1 <- ggplot(london_map1) +
  geom_sf(aes(fill = crime_2001_map$Crime2001), color = "black") +  
  geom_text(aes(x = x, y = y, label = Borough_ID), size = 3.5, color = "blue") + # Show numbers on the map
  scale_fill_viridis_c(option = "rocket", name = "No. of crimes",direction = -1,
                       limits = combined_range,
                       breaks = breaks) +  # Customize colour and add legend name
  labs(title = "Choropleth map for crime in 2001 (by borough)") +
  theme(plot.title = element_text(size = 20, face = "bold")) 

# Step 4: Create a Borough Legend
borough_legend <- data.frame(
  Borough_ID = london_map1$Borough_ID,
  Borough_Name = london_map1$BOROUGH
)

colnames(borough_legend) <- c("Number","Borough")

# Convert legend into a table
legend_table <-tableGrob(borough_legend,
               rows = NULL,         
               theme = ttheme_default(
              core = list(fg_params = list(cex = 0.65)),
              colhead = list(fg_params = list(cex = 0.6)),
               )
)  
# Have the plot on the left and the legend table on the right
grid.arrange(map_plot1, legend_table, ncol=2, widths = c(5,1.2))


# Choropleth Map 2
# Creating 2nd Choropleth map for crimes in 2013
london_map2 <- london_map %>%
  filter(BOROUGH != "City of London")

london_map2 <- london_map2 %>%       # Arrange the borough column in a-z order
  arrange(BOROUGH) 

colnames(crime_2013_map)[1] <- "BOROUGH"  # Ensure the name matches exactly

# Merge datasets
london_map2 <- london_map2 %>%
  left_join(crime_2013_map, by = "BOROUGH")

ggplot(london_map2) +
  geom_sf(aes(fill = crime_2013_map$Crime2013), color = "black") +
  geom_sf_text(aes(label = BOROUGH), size = 2.5, color = "white") +  # Add borough names
  scale_fill_viridis_c(option = "rocket", name = "No. of crimes",direction = -1,
                       limits = combined_range,
                       breaks = breaks) +  # Customize colour and add legend name
  theme_minimal()

# Making sure that the crime value column is part of the london map data, with all 32 boroughs.
names(london_map2)
names(crime_2001_map)
nrow(london_map2)
nrow(crime_data)


# Compute borough centroids
london_map2$centroid <- st_centroid(london_map2$geometry)

# Extract X, Y coordinates of centroids
centroids <- st_coordinates(london_map2$centroid)
london_map2$x <- centroids[,1]
london_map2$y <- centroids[,2]# Plot with labels at centroids

ggplot(london_map2) +
  geom_sf(aes(fill = crime_2013_map$Crime2013), color = "black") +
  geom_text(aes(x = x, y = y, label = BOROUGH), size = 3, color = "white") +  
  scale_fill_viridis_c(option = "rocket", name = "No. of crimes",direction = -1,
                       limits = combined_range,
                       breaks = breaks) +  # Customize colour and add legend name
  theme_minimal()

# Step 1: Create a Borough ID column (1 to 32)
london_map2$Borough_ID <- 1:nrow(london_map2)  # Assign 1–32


# Step 3: Plot the Map with Numbers Instead of Borough Names (using above centroids)
map_plot2 <- ggplot(london_map2) +
  geom_sf(aes(fill = crime_2013_map$Crime2013), color = "black") +  
  geom_text(aes(x = x, y = y, label = Borough_ID), size = 3.5, color = "blue") + # Show numbers on the map
  scale_fill_viridis_c(option = "rocket", name = "No. of crimes",direction = -1,
                       limits = combined_range,
                       breaks = breaks) +   # Customize colour and add legend name
  labs(title = "Choropleth map for crime in 2013 (by borough)") +
  theme(plot.title = element_text(size = 20, face = "bold")) 

# Step 4: Create a Borough Legend
borough_legend <- data.frame(
  Borough_ID = london_map2$Borough_ID,
  Borough_Name = london_map2$BOROUGH
)

colnames(borough_legend) <- c("Number","Borough")

# Convert legend into a table
legend_table <-tableGrob(borough_legend,
                         rows = NULL,         
                         theme = ttheme_default(
                           core = list(fg_params = list(cex = 0.65)),
                           colhead = list(fg_params = list(cex = 0.6)),
                         )
)  
# Have the plot on the left and the legend table on the right
grid.arrange(map_plot2, legend_table, ncol=2, widths = c(5,1.2))

# Both choropleth maps side by side (With 1 legend to save space)
grid.arrange(map_plot1, map_plot2 + theme(legend.position = "none"), legend_table, ncol=3, widths = c(4.8,4.1,1.2))


# Interactive plot for the trend of crime per region
# Load libraries
library(ggplot2)
library(plotly)

# For Research Question 1
# Create ggplot line graph
p <- ggplot(Crime_4, aes(x = Year, y = Crime_number_each_year_in_thousands, group = Region, color = Region)) +
  geom_line(size = 1) +  # Line for each region
  geom_point(aes(text = paste("Year:", Year, "<br>Crime Rate:", round(Crime_number_each_year_in_thousands, 1))), size = 3) +  
  labs(
    title = "Crime trend over the years",
    x = "Year",
    y = "No. of crimes (per 100,000)"
  ) +
  theme_minimal()

# Convert to interactive plot
interactive_plot <- ggplotly(p, tooltip = "text")
interactive_plot


######### Testing IF JSA claimants changed due to the financial crisis #######

install.packages("patchwork")
library(patchwork)

# Create Crime Rate Plot
p1 <- ggplot(Crime_4, aes(x = Year, y = Crime_number_each_year_in_thousands, color = Region)) +
  geom_line(size = 1) +
  facet_wrap(~ Region) +
  labs(y = "Crime Number in thousands", x = "Year") +
  theme_minimal()

# Create JSA Claimants Plot
p2 <- ggplot(Crime_4, aes(x = Year, y = Jobseekers_Claimants_Percentage, color = Region)) +
  geom_line(size = 1, linetype = "dashed") +
  facet_wrap(~ Region) +
  labs(y = "Jobseekers Claimants in thousand", x = "Year") +
  theme_minimal()

# Combine the two plots vertically
p1 / p2  # Uses patchwork to stack them


########################## Singular Borough Data ##############################


Crime_borough<- read.csv("Files for R code/Files for Choropleth Map + Boroughs/Singular Borough Data.csv"")

colnames(Crime_borough) <- c('Borough','Crime_number_each_year','Year','Population','Jobseekers_Claimants_Percentage','Median_House_Price','Average_Income','Job_Density')

# Plotting graphs for the various variables changing overtime, for each borough

# Graphs of crime rate trend over the years, for all 32 boroughs
ggplot(Crime_borough, aes(x = Year, y = Crime_number_each_year, group = Borough, color = Borough)) +
geom_line() +
facet_wrap(~ Borough, scales = "free_y") +  # One mini-plot per borough
theme_minimal() +
theme(legend.position = "none") +  # Remove messy legend
labs(title = "Crime Number Trends in Each Borough (2001–2013)",
     x = "Year", 
     y = "Crime Number")


# Graphs of population trend over the years, for all 32 boroughs
ggplot(Crime_borough, aes(x = Year, y = Population, group = Borough, color = Borough)) +
  geom_line() +
  facet_wrap(~ Borough, scales = "free_y") +  # One mini-plot per borough
  theme_minimal() +
  theme(legend.position = "none") +  # Remove messy legend
  labs(title = "Population Trends in Each Borough (2001–2013)",
       x = "Year", 
       y = "Population")

# Graphs of Jobseekers claimants rate trend over the years, for all 32 boroughs
ggplot(Crime_borough, aes(x = Year, y = Jobseekers_Claimants_Percentage, group = Borough, color = Borough)) +
  geom_line() +
  facet_wrap(~ Borough, scales = "free_y") +  # One mini-plot per borough
  theme_minimal() +
  theme(legend.position = "none") +  # Remove messy legend
  labs(title = "Jobseekers Claimant Percentage in Each Borough (2001–2013)",
       x = "Year", 
       y = "Jobseekers Claimant Percentage")

# Graphs of Median House Price trend over the years, for all 32 boroughs
ggplot(Crime_borough, aes(x = Year, y = Median_House_Price, group = Borough, color = Borough)) +
  geom_line() +
  facet_wrap(~ Borough, scales = "free_y") +  # One mini-plot per borough
  theme_minimal() +
  theme(legend.position = "none") +  # Remove messy legend
  labs(title = "Median House Price Trends in Each Borough (2001–2013)",
       x = "Year", 
       y = "Median House Price")  

# Graphs of Average Income trend over the years, for all 32 boroughs
ggplot(Crime_borough, aes(x = Year, y = Average_Income, group = Borough, color = Borough)) +
  geom_line() +
  facet_wrap(~ Borough, scales = "free_y") +  # One mini-plot per borough
  theme_minimal() +
  theme(legend.position = "none") +  # Remove messy legend
  labs(title = "Average Income Trends in Each Borough (2001–2013)",
       x = "Year", 
       y = "Average Income")  

# Graphs of Job Density trend over the years, for all 32 boroughs
ggplot(Crime_borough, aes(x = Year, y = Job_Density, group = Borough, color = Borough)) +
  geom_line() +
  facet_wrap(~ Borough, scales = "free_y") +  # One mini-plot per borough
  theme_minimal() +
  theme(legend.position = "none") +  # Remove messy legend
  labs(title = "Job Density Trends in Each Borough (2001–2013)",
       x = "Year", 
       y = "Job Density")  

