# Problem 3
# 3a: 3 is the correct answer. When GPA is high enough (>= 3.5), males make more on average.
# 3b: $137,100
# 3c: False, the coefficient does not mean the interaction does not have effect. We would need further testing to determine thise.

# Problem 4
# 4a: We would expect the training RSS to be lower with the cubic regression case because the cubic model is more flexible and will fit closer to the data. This could result in overfitting of the data when tested.
# 4b: We would expect the test RSS to be lower for the linear regression case because the linear regression case will not overfit to the variance in the training data and model the actual relationship more closely.
# 4c: We would expect the training RSS to be lower as the degree of our model increases because a model with a higher degree can adjust for non-linearity. This means we would expect training RSS to be lower for the cubic regression model.
# 4d: To answer this question with 100% certainty, we would need to know the degree of the non-linear relationship between X and Y because the linear regression model has the chance to have a lower RSS than the cubic regression. But for the sake of the argument, I will assume that the training data is large enough and representative enough for the cubic regression model to more accurately model the non-linear relationship between X and Y. This will result in the cubic regression model having a lower test RSS than the linear regression model because it was able to model the non-linearity between X and Y, while the linear regression model could not. Technically, if the relationship was just barely more than non-linear, then the linear regression model would have a lower RSS than the cubic regression model.

# Problem 8

setwd("C:/Users/codyb/Documents/Rstudio")

eightA <- function() {
  # Load and clean the Auto dataset
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  
  # Fit the linear model
  lm_fit <- lm(mpg ~ horsepower, data = Auto)
  
  # Print the summary of the linear model
  print(summary(lm_fit))
  
  # Make predictions for horsepower = 98 with confidence interval
  conf_interval <- predict(lm_fit, data.frame(horsepower = 98), interval = "confidence")
  cat("Confidence Interval for horsepower = 98:\n")
  print(conf_interval)
  
  pred_interval <- predict(lm_fit, data.frame(horsepower = 98), interval = "prediction")
  cat("Prediction Interval for horsepower = 98:\n")
  print(pred_interval)
}
# 8a:
# i: Yes
# ii: Strong p-value: (e-16)
# iii: Negative
# iv: 24.46708, confidence: 23.97308 24.96108, prediction: 14.8094 34.12476

eightB <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  lm_fit <- lm(mpg ~ horsepower, data = Auto)
  plot(Auto$horsepower, Auto$mpg, 
       xlab = "Horsepower", 
       ylab = "MPG", 
       main = "MPG vs Horsepower", 
       pch = 20, 
       col = "blue")
  abline(lm_fit, col = "red", lwd = 2)
}
# 8b: Done

eightC <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  lm_fit <- lm(mpg ~ horsepower, data = Auto)
  par(mfrow = c(2, 2))
  plot(lm_fit)
  par(mfrow = c(1, 1))
}
# 8c: There is a slight non-linear relationship between the two.

nineA <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  pairs(Auto[,-9], main = "Scatterplot Matrix for Auto Data", col = "blue")
}
# 9a: Done

nineB <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  cor_matrix <- cor(Auto[,-9])
  print(cor_matrix)
}
# 9b: Done

nineC <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  lm_fit <- lm(mpg ~ . - name, data = Auto)
  summary(lm_fit)
}
# 9c:
# i: Yes
# ii: displacement, weight, year, and origin
# iii: There is a linear relationship between year and mpg of 0.75.

nineD <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  par(mfrow = c(2, 2))
  lm_fit <- lm(mpg ~ . - name, data = Auto)
  plot(lm_fit)
  par(mfrow = c(1, 1))
}
# 9d: There seems to be a large outlier on Residuals vs Leverage.

nineE <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  lm_interaction <- lm(mpg ~ horsepower * weight + year * displacement + cylinders * weight + acceleration * horsepower, data = Auto)
  
  summary(lm_interaction)
}
# 9e: 

nineF <- function() {
  Auto <- read.csv("Chapter 3/data/Auto.csv", header = TRUE, na.strings="?")
  Auto <- na.omit(Auto)
  lm_transformed1 <- lm(mpg ~ log(horsepower) + sqrt(weight) + year, data = Auto)
  cat("Model with log(horsepower) and sqrt(weight):\n")
  print(summary(lm_transformed1))
  
  lm_transformed2 <- lm(mpg ~ I(horsepower^2) + I(weight^2) + year, data = Auto)
  cat("\nModel with squared terms (horsepower^2 and weight^2):\n")
  print(summary(lm_transformed2))
}

# Problem 10
tenA <- function() {
  library(ISLR)
  
  data("Carseats")
  
  lm_fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
  
  print(summary(lm_fit))
}
# 10a: Done

# 10b:
# - The coefficient for Price indicates that as Price increases by $1, Sales decrease by the amount of the coefficient.
# - There is not a significant relationship between sales and ubran/non-urban area.
# - There is a significant relationship between sales and US/non-US. Sales increase in the US by 1.2x if price and environment are the same.

# 10c: 13.04 + (-0.0545)X1 + (-0.0219)X2 + (1.201)X3

# 10d: We can reject the null hypothesis for Price and USYes.

tenE <- function() {
  library(ISLR)
  data("Carseats")
  lm_fit_reduced <- lm(Sales ~ Price + US, data = Carseats)

  print(summary(lm_fit_reduced))
}
# 10e: Done

tenF <- function() {
  # Plot the models to show how well they fit
  library(ISLR)
  data("Carseats")
  
  # Full model from tenA()
  lm_fit_full <- lm(Sales ~ Price + Urban + US, data = Carseats)
  
  # Reduced model from tenE()
  lm_fit_reduced <- lm(Sales ~ Price + US, data = Carseats)
  
  # Predicted values for both models
  predicted_full <- predict(lm_fit_full)
  predicted_reduced <- predict(lm_fit_reduced)
  
  # Actual Sales values
  actual_sales <- Carseats$Sales
  
  # Plotting Actual vs Predicted Sales for both models
  par(mfrow = c(1, 2))
  
  # Plot for the full model
  plot(actual_sales, predicted_full, 
       xlab = "Actual Sales", ylab = "Predicted Sales",
       main = "Full Model: Actual vs Predicted",
       pch = 20, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  
  # Plot for the reduced model
  plot(actual_sales, predicted_reduced, 
       xlab = "Actual Sales", ylab = "Predicted Sales",
       main = "Reduced Model: Actual vs Predicted",
       pch = 20, col = "green")
  abline(0, 1, col = "red", lwd = 2)
  
  par(mfrow = c(1, 1))
}

# 10f: There seems to be a trend found in the data through the model but the R-squared and error values indicate that there is too much variability from data point to data point to be extremely confident in a prediction.

tenG <- function() {
  library(ISLR)
  data("Carseats")
  lm_fit_reduced <- lm(Sales ~ Price + US, data = Carseats)
  conf_intervals <- confint(lm_fit_reduced, level = 0.95)
  
  print(conf_intervals)
}

# 10g: Done

tenH <- function() {
  library(ISLR)
  data("Carseats")
  lm_fit_reduced <- lm(Sales ~ Price + US, data = Carseats)
  
  par(mfrow = c(2, 2))
  plot(lm_fit_reduced)
  par(mfrow = c(1, 1))
}

# 10h: There does not appear to be evidence of outliers although one residual has very high leverage.

# Problem 11

elevenA <- function() {
  set.seed(1)
  
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)
  
  lm_fit <- lm(y ~ x + 0)
  
  print(summary(lm_fit))
  
  coef_estimate <- coef(summary(lm_fit))[1, "Estimate"]
  std_error <- coef(summary(lm_fit))[1, "Std. Error"]
  t_value <- coef(summary(lm_fit))[1, "t value"]
  p_value <- coef(summary(lm_fit))[1, "Pr(>|t|)"]
  
  cat("Coefficient Estimate (β̂):", round(coef_estimate, 3), "\n")
  cat("Standard Error:", round(std_error, 3), "\n")
  cat("t-Statistic:", round(t_value, 2), "\n")
  cat("p-Value:", format.pval(p_value, digits = 3), "\n")
  
  cat("\nComment:\n")
  cat("The estimated coefficient β̂ is", round(coef_estimate, 3), "with a standard error of", round(std_error, 3), ".\n")
  cat("The t-statistic is", round(t_value, 2), "and the p-value is", format.pval(p_value, digits = 3), ", which is much less than 0.05.\n")
  cat("Therefore, we reject the null hypothesis H0: β = 0. There is strong evidence that β is not equal to zero.\n")
}
# 11a: The estimated coefficient is significant, and we can reject the null hypothesis that β = 0.

elevenB <- function() {
  set.seed(1)
  
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)
  
  lm_fit <- lm(x ~ y + 0)
  
  print(summary(lm_fit))
  
  coef_estimate <- coef(summary(lm_fit))[1, "Estimate"]
  std_error <- coef(summary(lm_fit))[1, "Std. Error"]
  t_value <- coef(summary(lm_fit))[1, "t value"]
  p_value <- coef(summary(lm_fit))[1, "Pr(>|t|)"]
  
  cat("Coefficient Estimate (β̂):", round(coef_estimate, 3), "\n")
  cat("Standard Error:", round(std_error, 3), "\n")
  cat("t-Statistic:", round(t_value, 2), "\n")
  cat("p-Value:", format.pval(p_value, digits = 3), "\n")
  
  cat("\nComment:\n")
  cat("The estimated coefficient β̂ is", round(coef_estimate, 3), "with a standard error of", round(std_error, 3), ".\n")
  cat("The t-statistic is", round(t_value, 2), "and the p-value is", format.pval(p_value, digits = 3), ", which is much less than 0.05.\n")
  cat("Therefore, we reject the null hypothesis H0: β = 0. There is strong evidence that β is not equal to zero.\n")
}

# 11b: The estimated coefficient is significant, and we can reject the null hypothesis that β = 0.

elevenC <- function() {
  set.seed(1)
  
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)
  
  lm_fit_yx <- lm(y ~ x + 0)
  beta_yx <- coef(lm_fit_yx)[1]
  
  lm_fit_xy <- lm(x ~ y + 0)
  beta_xy <- coef(lm_fit_xy)[1]
  
  cat("Coefficient from regression of y onto x (β̂_yx):", round(beta_yx, 3), "\n")
  cat("Coefficient from regression of x onto y (β̂_xy):", round(beta_xy, 3), "\n")
  cat("Reciprocal of β̂_yx:", round(1 / beta_yx, 3), "\n")
  cat("Reciprocal of β̂_xy:", round(1 / beta_xy, 3), "\n")
  
  if (abs(beta_xy - 1 / beta_yx) < 1e-6 && abs(beta_yx - 1 / beta_xy) < 1e-6) {
    cat("\nThe coefficients are reciprocals of each other.\n")
  } else {
    cat("\nThe coefficients are not reciprocals of each other.\n")
  }
}

# 11c: The relationship between a and b is not exact because Beta in x = (Beta)y should be 0.5 and y = (Beta)x should be 2 (which happened).
# They are almost reciprocals or each other, but not close enough.

elevenD <- function() {
  set.seed(1)
  
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)

  lm_fit_yx <- lm(y ~ x + 0)

  t_stat_lm <- summary(lm_fit_yx)$coefficients[1, "t value"]

  n <- length(x)
  
  sum_x_y <- sum(x * y)
  sum_x_sq <- sum(x^2)
  sum_y_sq <- sum(y^2)
  
  numerator <- sqrt(n - 1) * sum_x_y
  
  denominator <- sqrt((sum_x_sq * sum_y_sq) - (sum_x_y)^2)
  
  t_stat_formula <- numerator / denominator
  
  cat("T-statistic from model summary:", round(t_stat_lm, 3), "\n")
  cat("T-statistic from the provided formula:", round(t_stat_formula, 3), "\n")
  
  # Check if they match
  if (abs(t_stat_lm - t_stat_formula) < 1e-6) {
    cat("\nThe t-statistics match.\n")
  } else {
    cat("\nThe t-statistics do not match.\n")
  }
}

# 11d: Done

# 11e: The numbers are the same from the calculations/models and switching them around in the formulas to calculate t-stat does nothing.

elevenF <- function() {
  # Set seed for reproducibility
  set.seed(1)
  
  # Generate predictor x and response y
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)
  
  # Regression of y onto x (with intercept)
  lm_fit_yx <- lm(y ~ x)
  
  # Regression of x onto y (with intercept)
  lm_fit_xy <- lm(x ~ y)
  
  # Extract t-statistics for the slope (β1) from both models
  t_stat_yx <- summary(lm_fit_yx)$coefficients[2, "t value"]
  t_stat_xy <- summary(lm_fit_xy)$coefficients[2, "t value"]
  
  # Print the t-statistics for comparison
  cat("T-statistic for regression of y onto x (with intercept):", round(t_stat_yx, 3), "\n")
  cat("T-statistic for regression of x onto y (with intercept):", round(t_stat_xy, 3), "\n")
  
  # Check if they match
  if (abs(t_stat_yx - t_stat_xy) < 1e-6) {
    cat("\nThe t-statistics for both regressions match.\n")
  } else {
    cat("\nThe t-statistics for both regressions do not match.\n")
  }
}

# 11f: Done

# Problem 12

# 12a: The circumstance where the coefficient for x onto y is the same as y onto x when there is no intercept, will be when the data has a slope of 1.

twelveB <- function() {
  set.seed(123)
  
  x <- rnorm(100, mean = 10, sd = 2)
  y <- 3 * x + rnorm(100, mean = 0, sd = 5)
  
  lm_yx <- lm(y ~ x + 0)
  beta_yx <- coef(lm_yx)[1]
  
  lm_xy <- lm(x ~ y + 0)
  beta_xy <- coef(lm_xy)[1]
  
  cat("Coefficient for regression of Y onto X (without intercept):", round(beta_yx, 3), "\n")
  cat("Coefficient for regression of X onto Y (without intercept):", round(beta_xy, 3), "\n")
  
  if (abs(beta_yx - beta_xy) > 1e-6) {
    cat("The coefficients are different.\n")
  } else {
    cat("The coefficients are the same.\n")
  }
}

# 12b: Done

twelveC <- function() {
  set.seed(123)
  
  x <- rnorm(100)
  y <- x
  
  lm_yx <- lm(y ~ x + 0)
  beta_yx <- coef(lm_yx)[1]
  
  lm_xy <- lm(x ~ y + 0)
  beta_xy <- coef(lm_xy)[1]
  
  cat("Coefficient for regression of Y onto X (without intercept):", round(beta_yx, 3), "\n")
  cat("Coefficient for regression of X onto Y (without intercept):", round(beta_xy, 3), "\n")
  
  if (abs(beta_yx - beta_xy) < 1e-6) {
    cat("The coefficients are the same.\n")
  } else {
    cat("The coefficients are different.\n")
  }
}

# 12c: Done

# Problem 13

thirteenA <- function() {
  set.seed(1)
  x <- rnorm(100, mean = 0, sd = 1)
  
  cat("First few values of x:\n")
  print(head(x))
  
  return(x)
}

# 13a: Done

thirteenB <- function() {
  set.seed(2)
  eps <- rnorm(100, mean = 0, sd = 0.5)
  
  cat("First few values of eps:\n")
  print(head(eps))
  
  return(eps)
}

# 13b: Done

thirteenC <- function(x, eps) {
  y <- -1 + 0.5 * x + eps
  
  cat("Length of y:", length(y), "\n")
  cat("First few values of y:\n")
  print(head(y))
  
  cat("\nIn this linear model:\n")
  cat("β0 (intercept) = -1\n")
  cat("β1 (slope) = 0.5\n")
  
  return(y)
}

# 13c: Done

thirteenD <- function(x, y) {
  plot(x, y, 
       xlab = "X", 
       ylab = "Y", 
       main = "Scatterplot of X vs Y", 
       pch = 19, col = "blue")
  
  grid()
}

# 13d: Done

thirteenE <- function(x, y) {
  lm_fit <- lm(y ~ x)
  
  cat("Summary of the linear model:\n")
  print(summary(lm_fit))
  
  beta_hat_0 <- coef(lm_fit)[1]
  beta_hat_1 <- coef(lm_fit)[2]
  
  cat("\nObtained coefficients:\n")
  cat("Intercept (β̂0):", round(beta_hat_0, 3), "\n")
  cat("Slope (β̂1):", round(beta_hat_1, 3), "\n")
  
  cat("\nTrue values:\n")
  cat("Intercept (β0): -1\n")
  cat("Slope (β1): 0.5\n")
  
  return(lm_fit)
}

# 13e: Done

thirteenF <- function(x, y, lm_fit) {
  plot(x, y, 
       xlab = "X", 
       ylab = "Y", 
       main = "X vs Y with Least Squares and Population Regression Lines", 
       pch = 19, col = "blue")
  
  abline(lm_fit, col = "red", lwd = 2)
  
  abline(a = -1, b = 0.5, col = "green", lwd = 2, lty = 2)
  
  legend("topleft", 
         legend = c("Least Squares Line", "Population Regression Line"), 
         col = c("red", "green"), 
         lwd = 2, lty = c(1, 2))
}

# 13f: Done

thirteenG <- function(x, y) {
  lm_linear <- lm(y ~ x)
  
  lm_poly <- lm(y ~ x + I(x^2))
  
  cat("Summary of the polynomial regression model:\n")
  print(summary(lm_poly))
  
  anova_results <- anova(lm_linear, lm_poly)
  cat("\nANOVA comparison between linear and polynomial models:\n")
  print(anova_results)
  
  if (anova_results[2, "Pr(>F)"] < 0.05) {
    cat("\nThere is significant evidence that the quadratic term improves the model fit.\n")
  } else {
    cat("\nThere is no significant evidence that the quadratic term improves the model fit.\n")
  }
  
  return(lm_poly)
}

# 13g: The quadratic models reduces RSS slightly and is slightly significant in coeff table.

thirteenH <- function() {
  set.seed(1)
  x <- rnorm(100, mean = 0, sd = 1)
  
  set.seed(2)
  eps <- rnorm(100, mean = 0, sd = 0.1)
  
  y <- -1 + 0.5 * x + eps
  
  thirteenD(x, y)
  lm_fit <- thirteenE(x, y)
  thirteenF(x, y, lm_fit)
  
  return(lm_fit)
}

# 13h: Done

thirteenI <- function() {
  set.seed(1)
  x <- rnorm(100, mean = 0, sd = 1)
  
  set.seed(2)
  eps <- rnorm(100, mean = 0, sd = 1)
  
  y <- -1 + 0.5 * x + eps
  
  thirteenD(x, y)
  lm_fit <- thirteenE(x, y)
  thirteenF(x, y, lm_fit)
  
  return(lm_fit)
}

# 13i: Done

thirteenJ <- function() {
  set.seed(1)
  x <- rnorm(100, mean = 0, sd = 1)
  
  set.seed(2)
  eps <- rnorm(100, mean = 0, sd = 0.5)
  
  y <- -1 + 0.5 * x + eps
  lm_fit_original <- thirteenE(x, y)
  cat("Confidence intervals for the original data:\n")
  print(confint(lm_fit_original))
  
  set.seed(1)
  x <- rnorm(100, mean = 0, sd = 1)
  
  set.seed(2)
  eps <- rnorm(100, mean = 0, sd = 0.1)
  
  y <- -1 + 0.5 * x + eps
  lm_fit_less_noise <- thirteenE(x, y)
  cat("\nConfidence intervals for the less noisy data:\n")
  print(confint(lm_fit_less_noise))
  
  set.seed(1)
  x <- rnorm(100, mean = 0, sd = 1)
  
  set.seed(2)
  eps <- rnorm(100, mean = 0, sd = 1)
  
  y <- -1 + 0.5 * x + eps
  lm_fit_more_noise <- thirteenE(x, y)
  cat("\nConfidence intervals for the noisier data:\n")
  print(confint(lm_fit_more_noise))
}