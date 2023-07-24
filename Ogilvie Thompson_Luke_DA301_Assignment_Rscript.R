## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# Install and import libraries.
# Installed previously on my device, remove # if required
# install.packages('tidyverse')
# install.packages("reshape")
library(tidyverse)
library(skimr)
library(DataExplorer)
library(reshape)
library(dplyr)

# Check working directory
getwd()

# Change working directory if required. Please update directory as applicable.
setwd(dir='C:/Users/logil/Dropbox/DACA/C3/Assignment/R Data')

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=TRUE)

# Alternatively, remove # below to chose file directly
# sales <- read.csv(file.choose(), header=T)

## Print the data frame to check it read properly.
head(sales)

# (Optional) View the data frame to have a reference copy.
# View(sales)

# Check column names
colnames(sales)
# All column names appear logical so do not need changing.

## Check the data frame for missing values.
sum(is.na(sales))
# Result is 2, so there are missing values.

## Check all columns for missing values as they might be used later.
sum(is.na(sales$Ranking))
sum(is.na(sales$Product))
sum(is.na(sales$Platform))
sum(is.na(sales$Year))
# Year has two missing values, but will still check other columns just in case.
sum(is.na(sales$Genre))
sum(is.na(sales$Publisher))
sum(is.na(sales$NA_Sales))
sum(is.na(sales$EU_Sales))
sum(is.na(sales$Global_Sales))
# Only missing values in Year column
# Initial subset planned does not use Year column,
# so will not adjust/remove the missing values at this stage as the values
# elsewhere in the rows will still contribute when not considering year.
# If Year is used for analysis this will need to be considered at that point.


## Check structure of the data frame.
str(sales)
# Columns all have acceptable data types

# Note: all Year values should be integers (other than NA values)
# However, cannot coerce from double to integer so will if all values round
# to themselves which would show it to be an integer
sales$Year == round(sales$Year)

# All appear to return TRUE apart from NA values. Create a vector to check this.
Year_int <- sales$Year == round(sales$Year)

# Count number of TRUE results in this vector
sum(Year_int, na.rm = TRUE)
# This results in 350, so 350 Year values are integers.
# The remaining two are the two NA values.

# (Optional) Remove Year_int from environment to free up space and keep clean.
rm(Year_int)

### Sense check remaining columns

## Check spellings of values in Platform column.
unique(sales$Platform)
# All 22 platform names known apart from "2600" and "GEN" which aren't known.

# Check rows with Platform "2600".
filter(sales,
       Platform == "2600")
# This shows 1 result: Platform "2600" with Publisher "Atari".
# From checking online: The Atari 2600 was a platform from the 1970s.
# As such, will Keep row.

# Check rows with Platform "GEN".
filter(sales,
       Platform == "GEN")
# This shows 1 result: Platform "GEN" with Publisher "Sega".
# From checking online: The Sega Genesis was a platform from the 1980s.
# As such, will Keep row.

## Check spellings of values in Genre column.
unique(sales$Genre)
# 12 genres shown, all of which seem to be reasonable categories.
# Consider using Genre as a Factor/Aggregate for analysis.

## Check spellings of values in Publisher column.
unique(sales$Publisher)
# 24 publishers shown, all of which seem to be reasonable.

## Check all sales are positive values
sales_values <-  select(sales, NA_Sales, EU_Sales, Global_Sales)
summary(sales_values)
# Min of each is 0, so all sales values are positive.
# Remove sales_values
rm(sales_values)

### Check for duplicates.
sales[duplicated(sales)]
# This shows 0 columns indicating there are no duplicates.
# As two values are missing in the Year column,
# check for duplicates when Year is removed.
sales_no_year <- select(sales, -Year)
sales_no_year[duplicated(sales_no_year)]
# Even without considering the year column, no duplicate values are identified
# (Optional) Remove sales_no_year from environment.
rm(sales_no_year)


## Add a ROW_Sales column
# Add a column to show sales from the rest of the world as ROW_Sales.
# Round the calculation to avoid scientific notation.
sales_full <- mutate(sales, ROW_Sales
                     =round((Global_Sales - (NA_Sales + EU_Sales)), 2))

# Check the new data frame.
head(sales_full)

# Place ROW_Sales before Global_Sales.
sales_full <- sales_full %>% relocate(ROW_Sales, .before = Global_Sales)

# Check the new data frame before saving it
head(sales_full)

### Export the cleaned data as a csv file for future use.
write_csv (sales_full, file='turtle_sales_clean.csv')

# (Optional, add/remove # below) Remove old data frames from environment.
rm(sales, sales_full)



### Reload the data as a new data frame.
sales_clean <- read.csv('turtle_sales_clean.csv', header=TRUE)

# Check sales_clean.
head(sales_clean)

# (Optional, add/remove # below) Create a report for a summary of data set.
# DataExplorer::create_report(sales_clean)



### Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns Ranking, Year, Genre, Publisher. 
sales1 <- select(sales_clean, -Ranking, -Year, -Genre, -Publisher)

# (Optional) View sales1 for a reference copy of data set being worked with
# View(sales1)

# View sales1 as_tibble and glimpse
as_tibble(sales1)

glimpse(sales1)

# Show dimensions of sales1
dim(sales1)

# Summarise the subset
summary(sales1)

# (Optional as takes some time to run)Create a quick summary of overall data set.
# DataExplorer::create_report(sales1)



### Explore the data to get a sense of it

## Check number of products
length(unique(sales1$Product))
# 175 different products

## Check number of platforms
length(unique(sales1$Platform))
# 22 different platforms

# Show all Platforms
unique(sales1$Platform)



################################################################################

# 2. Review plots to determine insights into the data set.

### If at any point, an error about invalid graphics state occurs. Run this:
dev.off()

## 2a) Scatterplots
# Create scatterplots.

# NA vs EU Sales
ggplot(sales1, aes(NA_Sales, EU_Sales)) +
  geom_point() +
  geom_smooth() +
  xlab("NA Sales(£m)") +
  ylab("EU Sales (£m)") +
  ggtitle("NA and EU Sales") +
  theme(plot.title = element_text(hjust = 0.5))

# NA vs Global Sales
ggplot(sales1, aes(NA_Sales, Global_Sales)) +
  geom_point() +
  geom_smooth() +
  xlab("NA Sales(£m)") +
  ylab("Global Sales (£m)") +
  ggtitle("NA and Global Sales") +
  theme(plot.title = element_text(hjust = 0.5))

# EU vs Global Sales
ggplot(sales1, aes(EU_Sales, Global_Sales)) +
  geom_point() +
  geom_smooth() +
  xlab("EU Sales(£m)") +
  ylab("Global Sales (£m)") +
  ggtitle("EU and Global Sales") +
  theme(plot.title = element_text(hjust = 0.5))

# All regions vs Global sales
ggplot(sales1, aes(NA_Sales, Global_Sales, colour = "NA")) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(EU_Sales, Global_Sales, colour = "EU")) + 
  geom_smooth(aes(EU_Sales, Global_Sales, colour = "EU")) +
  geom_point(aes(ROW_Sales, Global_Sales, colour = "ROW")) + 
  geom_smooth(aes(ROW_Sales, Global_Sales, colour = "ROW")) +
  xlab("Region Sales (£m)") +
  ylab("Global Sales (£m)") +
  ggtitle("All Regions and Global Sales") +
  theme(plot.title = element_text(hjust = 0.5))

## 2b) Histograms
# Create histograms.

# NA sales histogram
ggplot(sales1, aes(NA_Sales)) +
  geom_histogram(bins=10, color="black", fill = "red") +
  xlab("NA Sales(£m)") +
  ylab("Frequency") +
  ggtitle("NA Sales distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# EU sales histogram
ggplot(sales1, aes(EU_Sales)) +
  geom_histogram(bins=10, color="black", fill = "blue") +
  xlab("EU Sales(£m)") +
  ylab("Frequency") +
  ggtitle("EU Sales distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# ROW sales histogram
ggplot(sales1, aes(ROW_Sales)) +
  geom_histogram(bins=10, color="black", fill = "green") +
  xlab("ROW Sales(£m)") +
  ylab("Frequency") +
  ggtitle("ROW Sales distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Global sales histogram
ggplot(sales1, aes(Global_Sales)) +
  geom_histogram(bins=10, color="black") +
  xlab("Global Sales(£m)") +
  ylab("Frequency") +
  ggtitle("Global Sales distribution") +
  theme(plot.title = element_text(hjust = 0.5))


## 2c) Boxplots

# Create a single boxplot for all regions by using melt.
sales1_values <- select(sales1, -Platform)

sales1_long <- melt(sales1_values, id = "Product")
head(sales1_long)

ggplot(sales1_long, aes(x = variable, y = value, colour = variable)) +
  geom_boxplot() +
  xlab("Market") +
  ylab("Sales (£m)") +
  ggtitle("Comparison of Markets") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

ggplot(sales1_long, aes(x = variable, y = value, colour = variable)) +
  geom_violin() +
  xlab("Market") +
  ylab("Sales (£m)") +
  ggtitle("Comparison of Markets") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

###############################################################################

# 3. Observations and insights

# Sales data was imported and cleaned in RStudio and validated in
# a similar fashion to the review data.

# Scatterplots, histograms, boxplots, and violin plots were used to explore 
# the data and compare the NA, EU and Global sales. Before and after being 
# grouped by Product, the NA, EU and Global sales data demonstrated a 
# significant level of skewness with a large number of outliers at the 
# upper end of the sales data. These outliers are considered relevant to 
# the analysis as they represent the best selling games of all time.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

## 1. Load and explore the data

## View data frame created in Week 4.
head(sales_clean)

## Check output: Determine the min, max, and mean values.
# Min values
apply(sales_clean[,c(7, 8, 9, 10)],
      2,
      min,
      na.rm=TRUE)

# Max values
apply(sales_clean[,c(7, 8, 9, 10)],
      2,
      max,
      na.rm=TRUE)

# Mean values rounded to 2d.p.
round((apply(sales_clean[,c(7, 8, 9, 10)],
             2,
             mean,
             na.rm=TRUE)), 2)



## View the descriptive statistics.
summary(sales_clean)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

sales_prod_sum <- sales_clean %>% group_by(Product) %>%
  summarise(EU_Sales_sum=sum(EU_Sales),
            NA_Sales_sum=sum(NA_Sales),
            ROW_Sales_sum=sum(ROW_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')


# Output of dplyr is not data frame so requires turning back into one for melt. 
sales_prod_sum <- as.data.frame(sales_prod_sum)


# View the data frame.
head(sales_prod_sum)

# Explore the data frame.
summary(sales_prod_sum)


## 2b) Determine which plot is the best to compare game sales.
# From earlier analysis, side by side boxplot is easiest to compare

# However, will also check scatter plot to compare with non-grouped data.
# All regions vs Global sales
ggplot(sales_prod_sum, aes(NA_Sales_sum, Global_Sales_sum, colour = "NA")) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(EU_Sales_sum, Global_Sales_sum, colour = "EU")) + 
  geom_smooth(aes(EU_Sales_sum, Global_Sales_sum, colour = "EU")) +
  geom_point(aes(ROW_Sales_sum, Global_Sales_sum, colour = "ROW")) + 
  geom_smooth(aes(ROW_Sales_sum, Global_Sales_sum, colour = "ROW")) +
  xlab("Region Sales (£m)") +
  ylab("Global Sales grouped by Product (£m)") +
  ggtitle("All Regions and Global Sales when grouped by Product") +
  theme(plot.title = element_text(hjust = 0.5))

# Create combined boxplot.

sales_sum_long <- melt(sales_prod_sum, id = "Product")
head(sales_sum_long)

ggplot(sales_sum_long, aes(x = variable, y = value, colour = variable)) +
  geom_boxplot() +
  xlab("Market") +
  ylab("Sales grouped by Product (£m)") +
  ggtitle("Comparison of Markets when grouped by Product") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Explore violin plot
ggplot(sales_sum_long, aes(x = variable, y = value, colour = variable)) +
  geom_violin() +
  xlab("Market") +
  ylab("Sales grouped by Product (£m)") +
  ggtitle("Comparison of Markets when grouped by Product") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Check boxplot and violin 
ggplot(sales_sum_long, aes(x = variable, y = value, colour = variable)) +
  geom_boxplot() +
  geom_violin() +
  xlab("Market") +
  ylab("Sales grouped by Product (£m)") +
  ggtitle("Comparison of Markets when grouped by Product") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
# Considered messy and not of value

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_prod_sum$NA_Sales_sum,
       main = "NA Sales Q-Q Plot",
       col='blue')

qqline(sales_prod_sum$NA_Sales_sum,
       col='red',
       lwd=2) 

qqnorm(sales_prod_sum$EU_Sales_sum,
       main = "EU Sales Q-Q Plot",
       col='blue')

qqline(sales_prod_sum$EU_Sales_sum,
       col='red',
       lwd=2) 

qqnorm(sales_prod_sum$Global_Sales_sum,
       main = "Global Sales Q-Q Plot",
       col='blue')

qqline(sales_prod_sum$Global_Sales_sum,
       col='red',
       lwd=2) 
# All four Q-Q plots show the variables do not follow normal distributions
# They all appear to be exponential distributions

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

library (moments)

# Perform Shapiro-Wilk test.

shapiro.test(sales_prod_sum$NA_Sales_sum)
shapiro.test(sales_prod_sum$EU_Sales_sum)
shapiro.test(sales_prod_sum$Global_Sales_sum)
# All have very small p-values, so none are normally distributed.
# This confirms the indication from the histograms and boxplots from module 4.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sales_prod_sum$NA_Sales_sum) 
# 3.05

kurtosis(sales_prod_sum$NA_Sales_sum)
# 15.60

skewness(sales_prod_sum$EU_Sales_sum) 
# 2.89

kurtosis(sales_prod_sum$EU_Sales_sum)
# 16.23

skewness(sales_prod_sum$Global_Sales_sum) 
# 3.07

kurtosis(sales_prod_sum$Global_Sales_sum)
# 17.79

# All three metrics have a positive skewness, indicating that all three 
# distributions are right-skewed.

# All three metrics have a fairly high kurtosis of at least 15, indicating 
# that all three distributions are a lot more heavy-tailed compared to the 
# normal distribution.



## 3d) Determine correlation
# Determine correlation.
round (cor(sales_prod_sum), 2)
# Highest level of correlation between NA and Global Sales with 0.92
# Lowest level of correlation between ROW and other sales

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# The most useful plots above are considered to be:
# - Scatterplot showing all three markets against Global sales grouped by Product
# - Combined boxplot showing NA, EU, ROW and Global sales grouped by Product
# - Q-Q plots show NA, EU, ROW and Global appear to have exponential distributions


###############################################################################

# 5. Observations and insights

# Through grouping by Product it reduced the number of outliers and 
# the total skewness of the data.

# Q-Q plots and Shapiro-wilk, skewness and kurtosis tests all supported 
# that the sales figures were not normally distributed.
# NA, EU, ROW and Global appear to have exponential distributions.

# NA sales had the highest correlation with Global sales



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
head(sales_prod_sum)


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns

# NA_Sales_sum had the highest correlation with Global_Sales_sum
# Create a linear regression model on the original data.
model1 <- lm(Global_Sales_sum~NA_Sales_sum,
             data=sales_prod_sum)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)
# Residuals:
#
#   Min         1Q        Median    3Q        Max 
#   -15.3417    -1.8198   -0.5933   1.4322    11.9345 
# 
# Coefficients:
#                 Estimate    Std. Error  t value   Pr(>|t|)    
#   (Intercept)   2.45768     0.36961     6.649     3.71e-10 ***
#   NA_Sales_sum  1.63469     0.05435     30.079    < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.266 on 173 degrees of freedom
# Multiple R-squared:  0.8395,	Adjusted R-squared:  0.8385 
# F-statistic: 904.7 on 1 and 173 DF,  p-value: < 2.2e-16


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(model1$residuals)

# Plot the relationship with base R graphics.
plot(sales_prod_sum$NA_Sales_sum, sales_prod_sum$Global_Sales_sum)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1))



###########

### Check EU_Sales_sum with Global_Sales_sum
# Create a linear regression model on the original data.
model2 <- lm(Global_Sales_sum~EU_Sales_sum,
             data=sales_prod_sum)

# View the model.
model2

# View more outputs for the model - the full regression table.
summary(model2)
# Residuals:
#    Min         1Q        Median    3Q        Max 
#    -10.5583    -1.7530   -0.5371   0.9586    24.8556 
# 
# Coefficients:
#                 Estimate    Std. Error  t value   Pr(>|t|)    
#   (Intercept)   3.3343      0.4787      6.965     6.57e-11 ***
#   EU_Sales_sum  2.2369      0.1060      21.099    < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.313 on 173 degrees of freedom
# Multiple R-squared:  0.7201,	Adjusted R-squared:  0.7185 
# F-statistic: 445.2 on 1 and 173 DF,  p-value: < 2.2e-16


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(model2$residuals)

# Plot the relationship with base R graphics.
plot(sales_prod_sum$EU_Sales_sum, sales_prod_sum$Global_Sales_sum)
coefficients(model2)

# Add line-of-best-fit.
abline(coefficients(model2))


##########

### Look at EU_Sales_sum and NA_Sales_sum 
model3 <- lm(EU_Sales_sum~NA_Sales_sum,
             data=sales_prod_sum)

# View the model.
model3

# View more outputs for the model - the full regression table.
summary(model3)
# Residuals:
#   Min       1Q        Median      3Q      Max 
#   -9.9391   -1.1930   -0.4267     0.7023  9.6102 
# 
# Coefficients:
#                 Estimate    Std. Error  t value   Pr(>|t|)    
#   (Intercept)   1.17946     0.27433     4.299     2.85e-05 ***
#   NA_Sales_sum  0.42028     0.04034     10.419    < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.424 on 173 degrees of freedom
# Multiple R-squared:  0.3856,	Adjusted R-squared:  0.382 
# F-statistic: 108.6 on 1 and 173 DF,  p-value: < 2.2e-16


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(model3$residuals)

# Plot the relationship with base R graphics.
plot(sales_prod_sum$NA_Sales_sum, sales_prod_sum$EU_Sales_sum)
coefficients(model3)

# Add line-of-best-fit.
abline(coefficients(model3))



###############################################################################

# Install the psych package.
# install.packages('psych')

# Import the psych package.
library(psych)

# Check data frame to be used
head(sales_prod_sum)

# Remove product column
sales2 <- select(sales_prod_sum, -Product)

# 
head(sales2)

corPlot(sales2, cex=1)



# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.

# Create a new object and 
# specify the lm function and the variables.
modela = lm(Global_Sales_sum~NA_Sales_sum+EU_Sales_sum, data=sales2)

# Print the summary statistics.
summary(modela)

#   Residuals:
#   Min         1Q          Median      3Q        Max 
#   -3.4156     -1.0112     -0.3344     0.6516    6.6163 
# 
#   Coefficients:
#                   Estimate Std.   Error     t value   Pr(>|t|)    
#   (Intercept)     1.04242         0.17736   5.877     2.11e-08 ***
#   NA_Sales_sum    1.13040         0.03162   35.745    < 2e-16 ***
#   EU_Sales_sum    1.19992         0.04672   25.682    < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.49 on 172 degrees of freedom
# Multiple R-squared:  0.9668,	Adjusted R-squared:  0.9664 
# F-statistic:  2504 on 2 and 172 DF,  p-value: < 2.2e-16

###############################################################################

## Input values for predicting Global_Sales
# 1 - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
# 2 - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
# 3 - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
# 4 - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
# 5 - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

# Create data frame for input values.
input_values <- data.frame (NA_Sales_sum = c(34.02, 3.93, 2.73, 2.26, 22.08),
                          EU_Sales_sum = c(23.80, 1.56, 0.65, 0.97, 0.52))

# (Optional) Alternatively load data frame in from client with further inputs.
# input_values <- read.csv(file.choose(), header=T)

# Check data frame.
input_values

# Check structure of data frame.
str(input_values)

# 4. Predictions based on given values.
# Compare with observed values for a number of records.

# Create a new object and specify the predict function.
predictTest = predict(modela, newdata=input_values,
                      interval='confidence')

# Print the object.
round(predictTest, 2)
#     fit     lwr     upr
# 1   68.06   66.43   69.68
# 2   7.36    7.10    7.61
# 3   4.91    4.61    5.20
# 4   4.76    4.48    5.04
# 5   26.63   25.37   27.88

predictions <- cbind(input_values, round(predictTest, 2))

# Check the new data frame.
predictions

# Add a column for range of predicted Global Sales
predictions <- mutate(predictions, Predicted_Global_Sales_range
                     =round((upr - lwr), 2))


# Adjust the column names of the inputs for end user clarity.
predictions <- predictions %>%
   rename_at('NA_Sales_sum', ~'Input NA Sales (£m)')

predictions <- predictions %>%
   rename_at('EU_Sales_sum', ~'Input EU Sales (£m)')


# Adjust the column names of the predictions for end user clarity.
predictions <- predictions %>% 
  rename_at('fit', ~'Predicted Global Sales (£m)')

predictions <- predictions %>%
  rename_at('lwr', ~'Predicted Global Sales lower-bound (£m)')

predictions <- predictions %>%
  rename_at('upr', ~'Predicted Global Sales upper-bound (£m)')

predictions <- predictions %>% 
  rename_at('Predicted_Global_Sales_range', ~'Predicted Global Sales range (£m)')


# View the final predictions table
View(predictions)

### Export the predictions as a csv file for future use.
write_csv (predictions, file='Predictions.csv')


## Check for multicollinearity
# install.packages("car")
library(car)


# calculate the VIF for each predictor variable in the model
vif(modela)

#   NA_Sales_sum    EU_Sales_sum 
#   1.627488        1.627488

## Both VIF values are <5 and therefore not significantly correlated enough
# to be problematic in terms of multicollinearity

###############################################################################

# 5. Observations and insights

# NA and EU sales could individually be used to predict Global sales to some degree. 

# A multiple linear regression model using NA and EU sales can predict Global
# sales with a high degree of accuracy. NA and EU sales did not 
# exhibit multicollinearity.

# However NA sales + EU sales equal a large part of Global sales, so this model
# is of limited use.



###############################################################################
###############################################################################




