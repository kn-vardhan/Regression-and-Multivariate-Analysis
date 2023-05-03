# load necessary packages for regression
library(car)
library(lmtest)
library(corrplot)
library(readr)
library(MASS)


# Load data
data <- read_delim("dataset.csv", delim = ",")

# remove 1st column
data <- data[, -1]

# head of the data
head(data)

# change column names 
names(data) <- c("Income", "Commute", "Literacy","JobGrowth", "Physicians",	"RapeRate", "Restaurants", "Housing", "MedianAge", "HouseholdIncome")

# summary of the data
summary(data)

# correlation matrix and plot
corr_matrix <- cor(data)
corrplot(corr_matrix, method = "color")

# histogram plot of dependent variable - Housing
hist(data$Housing, main="Histogram Plot of Dependent Variable", 
     xlab = "Dependent Variable - Housing", ylab = "Frequency")


# fit linear regression model on the dataset
model <- lm(Housing ~ ., data = data)

# summary of the model
summary(model)

# Checking the correctness of the assumptions
# 1. Linearity
# 2. Independence of errors
# 3. Homoscedasticity
# 4. Normality of Errors
# 5. Multicollinearity

# 1. Linearity
plot(model, which=1)

# 2. Independence of errors (Auto correlation)
# Performing Durbin-Watson test for Auto correlation
# Durbin-Watson statistic close to 2 implies no auto correlation
dwtest(model)


# 3. Homoscedasticity
# Performing Breusch-Pagan test for Homoscedasticity
# Null hypothesis: Homoscedasticity
# Alternate hypothesis: Heteroscedasticity
bptest(model)

# 4. Normality of errors
# Performing Shapiro-Wilk test for Normality of errors
# Null hypothesis: Normality of errors
# Alternate hypothesis: Non-normality of errors
# Q-Q plot
plot(model, which=2)
shapiro.test(resid(model))


# correction for normality
# Log transformation
model_log <- lm(log(Housing) ~ ., data = data)

# updated model summary
summary(model_log)

# 5. Multicollinearity
# Performing Variance Inflation Factor (VIF) test for Multicollinearity
# VIF > 10 implies Multicollinearity
vif(model_log)

# Income has high VIF value
# Removing Income from the model
new_data <- data[, -1]
model_log <- lm(log(Housing) ~ ., data = new_data)

#check VIF
vif(model_log)

# updated model summary
summary(model_log)

# checking all the assumptions once again on the updated final model
# 1. Linearity
plot(model_log, which=1)

# 2. Independence of errors (Auto correlation)
dwtest(model_log)

# 3. Homoscedasticity
bptest(model_log)

# 4. Normality of errors
shapiro.test(resid(model_log))

# 5. Multicollinearity
vif(model_log)

# All the assumptions are satisfied

# check for outliers
# Cook's distance
plot(model_log, which=4)

