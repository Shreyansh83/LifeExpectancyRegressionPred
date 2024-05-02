# Analysis of the Effects of Economic Growth on Health Outcomes

## Project Overview
This repository contains the R code and findings from my coursework titled "The Effects of Economic Growth on Health." The study investigates the relationship between economic indicators like GDP per capita and various health indicators such as life expectancy, infant mortality rate (IMR), and maternal mortality rate (MMR) across different countries from 2000 to 2017.

## Repository Structure
- `Analysis.R`: R script containing all the statistical analyses and data visualization code.
- `Report.pdf`: Comprehensive report detailing the project's background, methodology, results, and conclusions.
- `Data/`: Folder containing the CSV files used in the analysis.

## Findings
This project highlights the significant relationship between a country's economic growth and its health outcomes. Key findings include:
- A direct correlation between GDP per capita and improvements in life expectancy.
- The impact of economic status on health metrics like IMR and MMR, varying significantly across different income levels of countries.

## Tools Used
- **R**: For all statistical analysis and data visualizations.
- **ggplot2**: Used for creating advanced visual representations of our data.
- **Tableau**: Used for additional visual insights.

## Visualization Samples
Here are a few visualizations derived from our analysis:
![GDP vs Life Expectancy](/Images/life_gdp.png)  
*Figure 1: GDP per capita vs. Life Expectancy over time.*

## How to Run the Code
To run this analysis, you will need R and RStudio installed. Clone this repository, navigate to the directory containing `Analysis.R`, and open the script in RStudio. Make sure to replace your current folder in setwd() function. Ensure all necessary libraries are installed by running:
```R
install.packages(c("ggplot2", "dplyr", "tidyr","gridExtra","corrgram","plm","lme4","directlabels","ggthemes","directlabels","psych","lmtest"))
