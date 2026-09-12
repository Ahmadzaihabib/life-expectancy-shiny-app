
# Healthcare Workforce and Life Expectancy:
# A Global Panel Data Analysis with Interactive Policy Simulation
#
# Author: Habib Gul Ahmadzai
# Course: Global Data Analytics
# Date: 4/23/2026



# Loading the libraries
library(tidyverse)

# For this analysis, I have 4 different datasets from the world Bank data. In this step, I will use one 
# to clean and reshape it and will combine this process for the other three datasets used.

# Loading the life_expectancy dataset
life <- read_csv("life_expectancy.csv", skip = 4)
head(life)

# Now lets clean the coulmns
life <- life %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`
  )

# Now removing the unnecessary columns
life <- life %>%
  select(-`Indicator Name`, -`Indicator Code`)

# Converting from wide --> Long
life_long <- life %>%
pivot_longer(
  cols = -c(country, country_code),
  names_to = "year",
  values_to = "life_expectancy"
)


# Clean year and values

life_long <- life_long %>%
  mutate(
    year = as.numeric(year),
    life_expectancy = as.numeric(life_expectancy)
  )


# Filtering useful years for my own anlysis
life_long <- life_long %>%
  filter(year >= 2000, year <= 2022)

# Removing all the missing values
life_long <- life_long %>%
  drop_na(life_expectancy)

# Now lets check our final results
head(life_long)


#Now in tbis step, I would be doing the same process for all the other three datasets combined



# Physicians per 1,000

physicians <- read_csv("physicians.csv", skip = 4, show_col_types = FALSE)

physicians <- physicians %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`
  )

physicians <- physicians %>%
  select(-`Indicator Name`, -`Indicator Code`)

physicians_long <- physicians %>%
  pivot_longer(
    cols = -c(country, country_code),
    names_to = "year",
    values_to = "physicians_per_1000"
  )

physicians_long <- physicians_long %>%
  mutate(
    year = as.numeric(year),
    physicians_per_1000 = as.numeric(physicians_per_1000)
  )

physicians_long <- physicians_long %>%
  filter(year >= 2000, year <= 2022)

physicians_long <- physicians_long %>%
  drop_na(physicians_per_1000)



# GDP per capita

gdp <- read_csv("gdp.csv", skip = 4, show_col_types = FALSE)

gdp <- gdp %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`
  )

gdp <- gdp %>%
  select(-`Indicator Name`, -`Indicator Code`)

gdp_long <- gdp %>%
  pivot_longer(
    cols = -c(country, country_code),
    names_to = "year",
    values_to = "gdp_per_capita"
  )

gdp_long <- gdp_long %>%
  mutate(
    year = as.numeric(year),
    gdp_per_capita = as.numeric(gdp_per_capita)
  )

gdp_long <- gdp_long %>%
  filter(year >= 2000, year <= 2022)

gdp_long <- gdp_long %>%
  drop_na(gdp_per_capita)



# Health expenditure per capita

health_exp <- read_csv("health_exp.csv", skip = 4, show_col_types = FALSE)

health_exp <- health_exp %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`
  )

health_exp <- health_exp %>%
  select(-`Indicator Name`, -`Indicator Code`)

health_exp_long <- health_exp %>%
  pivot_longer(
    cols = -c(country, country_code),
    names_to = "year",
    values_to = "health_exp_per_capita"
  )

health_exp_long <- health_exp_long %>%
  mutate(
    year = as.numeric(year),
    health_exp_per_capita = as.numeric(health_exp_per_capita)
  )

health_exp_long <- health_exp_long %>%
  filter(year >= 2000, year <= 2022)

health_exp_long <- health_exp_long %>%
  drop_na(health_exp_per_capita)


# Converting year to numeric values

life_long <- life_long %>%
  mutate(year = as.numeric(year))

physicians_long <- physicians_long %>%
  mutate(year = as.numeric(year))

gdp_long <- gdp_long %>%
  mutate(year = as.numeric(year))

health_exp_long <- health_exp_long %>%
  mutate(year = as.numeric(year))


# Merge all 4 datasets
#
health_data <- life_long %>%
  inner_join(physicians_long, by = c("country", "country_code", "year")) %>%
  inner_join(gdp_long, by = c("country", "country_code", "year")) %>%
  inner_join(health_exp_long, by = c("country", "country_code", "year"))

# 
# Removing aggregate groups
# 
health_data <- health_data %>%
  filter(!country %in% c(
    "World", "High income", "Low income", "Middle income",
    "Lower middle income", "Upper middle income",
    "OECD members", "European Union",
    "East Asia & Pacific", "Europe & Central Asia",
    "Latin America & Caribbean", "Middle East & North Africa",
    "North America", "South Asia", "Sub-Saharan Africa"
  ))

# 
# Check final data
# 
dim(health_data)
head(health_data)
glimpse(health_data)


# Saving final file

write_csv(health_data, "health_data.csv")




# -------------------------
# Statistical Model
# -------------------------
health_data <- health_data %>%
  mutate(
    log_gdp_per_capita = log(gdp_per_capita),
    log_health_exp_per_capita = log(health_exp_per_capita)
  )

# Running the regression
model <- lm(
  life_expectancy ~ physicians_per_1000 + log_gdp_per_capita + log_health_exp_per_capita,
  data = health_data
)

summary(model)




