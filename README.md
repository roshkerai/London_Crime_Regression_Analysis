# 📊 London Crime Regression Analysis

**Final Year Dissertation** analysing the relationship between socio-economic factors and crime across London regions using R and statistical regression models.

## Overview

This repository contains my final year dissertation, which investigates the relationship between socio-economic factors and crime across five London regions between **2001 and 2013**.

The project combines multiple public datasets, performs extensive data cleaning and exploratory analysis, and applies statistical regression models to understand which factors are associated with crime levels across London.

The analysis was completed using **R** as part of my BSc Mathematics for Data Science.

---

## Research Questions

This project aimed to answer the following questions:

- How did crime levels change across London between 2001 and 2013?
- Which socio-economic factors have the strongest relationship with crime?
- How do these relationships vary across different London regions?
- Which statistical model provides the best fit for modelling crime counts?
- Did the 2008 financial crisis appear to influence crime trends?

---

## Dataset

The analysis combines multiple datasets into a single processed dataset.

### Raw datasets

- Crime data
- Population
- Average income
- Job density
- Jobseeker's Allowance (JSA)
- Median house prices

These datasets were cleaned and merged before statistical analysis.

---

## Repository Structure

```
london-crime-regression-analysis
│
├── Dissertation_Report.pdf
├── README.md
│
├── data
│   ├── raw
│   └── processed
│
├── scripts
│   └── crime_analysis.R
│
├── maps
│   └── london_boroughs
│
└── figures
```

---

## Methodology

The project followed the workflow below:

1. Data collection
2. Data cleaning and preparation
3. Dataset integration
4. Exploratory Data Analysis (EDA)
5. Statistical modelling
6. Model diagnostics
7. Geographic visualisation
8. Interpretation of results

---

## Statistical Methods

The following techniques were applied throughout the project:

- Exploratory Data Analysis (EDA)
- Correlation analysis
- Multiple Linear Regression
- Poisson Regression
- Negative Binomial Regression
- Beta Regression
- Model comparison using AIC
- Variance Inflation Factor (VIF)
- Residual diagnostics
- Overdispersion testing
- Choropleth mapping

---

## Technologies Used

- R
- ggplot2
- dplyr
- MASS
- betareg
- sf
- car
- corrplot
- tidyverse

---

## Key Findings

Some of the main findings include:

- Crime trends differed across London's regions between 2001 and 2013.
- Socio-economic indicators showed measurable relationships with crime levels.
- Negative Binomial regression outperformed Poisson regression due to overdispersion in the crime count data.
- Geographic visualisations highlighted regional differences in crime distribution.
- The project demonstrated the importance of selecting an appropriate statistical model for count data.

---

## Visualisations

Example outputs from the project include:

- Crime trends over time
- Choropleth maps of London boroughs
- Correlation matrices
- Regression diagnostics
- Model comparison plots

*(Images can be found in the `figures` folder.)*

---

## How to Reproduce the Analysis

1. Clone this repository.
2. Open `scripts/crime_analysis.R` in RStudio.
3. Install the required R packages.
4. Update any file paths if necessary.
5. Run the script from top to bottom.

---

## Dissertation

The complete dissertation can be found here:

📄 **Dissertation_Report.pdf**

---

## Author

**Roshan Kerai**
