# 📊 London Crime Regression Analysis

## Final Year Dissertation | BSc Mathematics for Data Science

An analysis of crime trends across London from 2001–2013, investigating the relationship between crime and socio-economic factors using statistical regression models in R.

---

## 📌 Project Overview

This project investigates the spatial and temporal patterns of crime across five regions of London between 2001 and 2013.

The analysis examines whether socio-economic factors can help explain differences in crime levels between regions and over time.

The project combines multiple datasets from the London Datastore, followed by data preparation, exploratory analysis, statistical modelling and geographic visualisation.

The main statistical models used were:

- Poisson Regression
- Negative Binomial Regression
- Beta Regression

A key finding was the presence of **overdispersion** in the crime count data. As a result, Negative Binomial regression provided a more appropriate model than Poisson regression for the analysis.

---

## 🎯 Research Objectives

The project aimed to investigate:

- How crime levels changed across London between 2001 and 2013
- How crime trends differed between the five London regions
- The relationship between crime and socio-economic factors
- Which predictors were statistically significant within each region
- The suitability of different regression approaches for modelling crime
- Whether major events, including the 2008 financial crisis, may have influenced crime trends

---

## 📊 Data

The final dataset was created by combining five data sources obtained from the London Datastore.

### Variables

| Variable | Description |
|----------|-------------|
| Crime | Total number of recorded crimes |
| Population | Population size |
| Average Income | Average income |
| Job Density | Job density |
| JSA | Percentage of jobseeker's allowance claimants |
| Median House Price | Median house price |
| Year | Year of observation |
| Region | London region |

The original crime dataset contained crime counts for individual wards across London's boroughs. The different crime categories were aggregated to produce total crime counts for the analysis.

---

## 🔎 Analysis Workflow

The project followed the following workflow:

**Raw Data → Data Preparation → Exploratory Analysis → Statistical Modelling → Model Diagnostics → Geographic Analysis → Conclusions**

### 1. Data Preparation

Multiple datasets were cleaned and combined to produce the final dataset used for regression analysis.

### 2. Exploratory Data Analysis

Crime trends were analysed over time and compared across the five London regions.

Correlation analysis and visualisations were also used to investigate relationships between crime and the socio-economic predictors.

### 3. Regression Modelling

Several regression approaches were considered:

**Poisson Regression**

Used initially to model the crime count response variable.

**Negative Binomial Regression**

Used after identifying overdispersion in the crime data. This provided a more appropriate model for the count data.

**Beta Regression**

A separate Beta regression approach was also investigated using transformed data.

### 4. Model Diagnostics

A range of diagnostic techniques were used, including:

- Overdispersion testing
- Variance Inflation Factor (VIF)
- Q-Q plots
- Residual analysis
- Model comparison
- AIC

### 5. Geographic Analysis

Choropleth maps were created to visualise the geographic distribution of crime across London's boroughs.

---

## 📈 Key Findings

### Crime Trends

Crime levels generally decreased across the five London regions between 2001 and 2013, although individual regions experienced periods of significant fluctuation.

The analysis also identified differences in crime patterns between regions, suggesting that local factors and external events may have contributed to variations over time.

### Regression Models

The crime data exhibited **overdispersion**, meaning that the variance of the crime counts was greater than their mean.

As Poisson regression assumes equidispersion, Negative Binomial regression was used to account for this additional variability.

The Negative Binomial models provided a more reliable fit and more appropriate statistical inference for the crime count data.

### Socio-economic Factors

The regression analysis showed that the significance and influence of socio-economic predictors varied between London regions.

Some predictors were statistically significant in certain regions while having less evidence of an effect in others.

This highlights the importance of considering regional differences when analysing crime.

### Geographic Patterns

Choropleth maps were used to visualise differences in crime levels across London's boroughs and provide a spatial perspective alongside the statistical analysis.

---

## 🗺️ Visualisations

The project includes several visualisations, including:

- Crime trends across London regions
- Correlation analysis
- Regression model diagnostics
- Model comparison
- Choropleth maps of London boroughs

Selected visualisations are available in the [`Figures`](Figures/) folder.

---

## 🛠️ Tools & Technologies

### Programming

- **R**

### Data Analysis

- Data cleaning and preparation
- Exploratory Data Analysis
- Statistical modelling
- Regression analysis
- Model diagnostics

### Visualisation

- `ggplot2`
- Choropleth mapping
- Correlation visualisation

### Statistical Modelling

- Poisson Regression
- Negative Binomial Regression
- Beta Regression

### Other R Packages

- `dplyr`
- `MASS`
- `betareg`
- `car`
- `sf`

---

## 📁 Repository Structure

```text
london-crime-regression-analysis
│
├── Dissertation_Report.pdf
│
├── data
│   ├── raw
│   │   ├── crime_population.csv
│   │   ├── income.csv
│   │   ├── job_density.csv
│   │   ├── jsa.csv
│   │   └── median_house_prices.csv
│   │
│   └── processed
│       └── crime_data_processed.csv
│
├── scripts
│   └── crime_analysis.R
│
├── maps
│   ├── [map data]
│   └── london_boroughs
│
└── figures
    └── [analysis visualisations]
