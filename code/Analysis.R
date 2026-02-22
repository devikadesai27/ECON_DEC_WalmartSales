# load library
library(tidyverse)
library(fixest)
library(lubridate)

# Load Walmart_Sales.csv file
walmart_sales <- read_csv("./raw_data/Walmart_Sales.csv")

# Data Cleaning
# Check for missing values
colSums(is.na(walmart_sales))

# Check for duplicate rows
sum(duplicated(walmart_sales))

# Check data types
str(walmart_sales)

# Convert Date column into proper Date format
# Use dmy because format is YYYY-MM-DD
walmart_sales <- walmart_sales %>%
  mutate(Date = dmy(Date))

# Create month and year variables 
# Month fixed effects control for seasonality (temperature and sales both vary by season)
# Create time variables for seasonality and time trends
# season captures broad seasonal patterns (Winter/Spring/Summer/Fall)
# year captures overall changes across years (e.g., economy-wide trends)
walmart_sales <- walmart_sales %>%
  mutate(
    month = month(Date),
    year  = year(Date),
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )

# Exploratory visualization
# Plot Temperature and Weekly sales.
ggplot(walmart_sales, 
       aes(x = Temperature, y= Weekly_Sales))+
  geom_point(alpha=0.2)+
  geom_smooth()

# Key variables for regression
# create a squared term to allow for non linearity 
# Log-transform sales so coefficients can be interpreted as percentage changes and to reduce heteroskedasticity.
walmart_sales <- walmart_sales %>%
  mutate(
    Temp_sq = Temperature^2,
    log_sales = log(Weekly_Sales)
  )

# add boxplot
ggplot(walmart_sales, aes(x = season, y = log_sales)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Log Weekly Sales by Season",
       x = "Season",
       y = "Log Weekly Sales") +
  theme_minimal()

# Panel fixed effects regression
# controls: Holiday_Flag, Fuel_Price,CPI, Unemployment
# Fixed effects:
# Store FE: controls for time-invariant differences across stores (size, location, demographics)
# Month FE: controls for detailed monthly seasonality that affects both temperature and sales
# Year FE: controls for time trends
# Clustered standard errors at the store level account for serial correlation within stores over time.
model1 <- feols(log_sales ~ Temperature + Temp_sq +
                  Holiday_Flag + Fuel_Price + CPI +Unemployment 
                | Store + month + year, 
                data = walmart_sales,
                vcov = ~ Store)


# Display regression table
etable(model1)

# plot for heteroskedasticity
walmart_sales <- walmart_sales %>%
  mutate(
    fitted = fitted(model1),
    residuals= resid(model1)
  )

ggplot(walmart_sales, aes(fitted, residuals)) +
  geom_point(alpha =0.3, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed",   color = "red")+
  labs(title= "Residual vs Fitted", x= "Fitted", 
       y= "Residuals") +
  theme_minimal()

# Model 2: Excludes Unemployment to test robustness.
# If temperature coefficients remain similar, results are robust
# Include store, month, year fixed effects.
# Clustered standard errors at store level
model2 <- feols(log_sales ~ Temperature + Temp_sq +
                  Holiday_Flag + Fuel_Price + CPI
                | Store + month + year,
                data= walmart_sales,
                vcov = ~ Store)

# Display regression table for model 2
etable(model2)
