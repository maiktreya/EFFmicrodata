
########################################## RIF METHOD  LIBRARY DINEQ ################################################

###  Recentered Influence Function EXAMPLE ###
library(dineq)

# Generate income and education data
set.seed(123)
data <- data.frame(income = rnorm(1000, mean = 50, sd = 10), education = rnorm(1000, mean = 3, sd = 1))

# Calculate the RIF for the Gini coefficient
data$RIF_income <- rif(data$income)

# Run regression analysis using the calculated RIF as the dependent variable
model <- lm(RIF_income ~ education, data = data)

# View model summary
summary(model)
