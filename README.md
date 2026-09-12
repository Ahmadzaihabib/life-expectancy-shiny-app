# Healthcare Workforce and Life Expectancy: A Global Policy Simulation Dashboard

🔗 [Live app](https://ahmadzaihabib.shinyapps.io/healthcare-life-expectancy-dashboard/)

An interactive R Shiny dashboard exploring how physician density, GDP per capita, and health expenditure per capita relate to life expectancy across countries and over time.

## What it does

This app fits a multiple regression model:

life_expectancy ~ physicians_per_1000 + log(gdp_per_capita) + log(health_exp_per_capita)

and lets users run a "what-if" policy simulation:

- Adjust sliders for physicians per 1,000 people, GDP per capita, and health expenditure per capita to see the model's predicted life expectancy update instantly.
- Select and compare multiple countries to see how life expectancy and physician density have trended over time.
- View the underlying regression coefficients in a model summary table.

## Technologies used

- R / Shiny — app framework
- tidyverse — data wrangling
- plotly — interactive charts
- broom — tidy regression output

## Data

- life_expectancy.csv, gdp.csv, health_exp.csv, physicians (1).csv — raw World Bank source data
- data_cleaning_and_analysis.R — script that cleans, reshapes, and merges the raw data into health_data.csv
- health_data.csv — final cleaned dataset used directly by the app

## Running it locally

1. Clone this repo:
   git clone https://github.com/Ahmadzaihabib/life-expectancy-shiny-app.git
2. Open Ahmadzai_ShinyApp.R in RStudio.
3. Install required packages if you don't have them:
   install.packages(c("shiny", "tidyverse", "plotly", "broom"))
4. Click Run App.

## Author

Habib Gul Ahmadzai — created for Global Data Analytics coursework.
