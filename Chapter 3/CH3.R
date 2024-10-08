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

setwd("C:/Users/codyb/Documents/RStudio")

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
  par(mfrow = c(2, 2))
  plot(lm_fit)
  par(mfrow = c(1, 1))
}
# 8c: There is a slight non-linear relationship between the two.

nineA <- function() {
  pairs(Auto[,-9], main = "Scatterplot Matrix for Auto Data", col = "blue")
}
# 9a: Done

nineB <- function() {
  cor_matrix <- cor(Auto[,-9])
  print(cor_matrix)
}
# 9b: Done

nineC <- function() {
  lm_fit <- lm(mpg ~ . - name, data = Auto)
  summary(lm_fit)
}
# 9c:
# i: Yes
# ii: displacement, weight, year, and origin
# iii: There is a linear relationship between year and mpg of 0.75.

nineD <- function() {
  par(mfrow = c(2, 2))
  lm_fit <- lm(mpg ~ . - name, data = Auto)
  plot(lm_fit)
  par(mfrow = c(1, 1))
}
# 9d: There seems to be a large outlier on Residuals vs Leverage.

nineE <- function() {
  lm_interaction <- lm(mpg ~ horsepower * weight + year * displacement + cylinders * weight + acceleration * horsepower, data = Auto)
  
  summary(lm_interaction)
}
# 9e: 

nineF <- function() {
  lm_transformed1 <- lm(mpg ~ log(horsepower) + sqrt(weight) + year, data = Auto)
  cat("Model with log(horsepower) and sqrt(weight):\n")
  print(summary(lm_transformed1))
  
  lm_transformed2 <- lm(mpg ~ I(horsepower^2) + I(weight^2) + year, data = Auto)
  cat("\nModel with squared terms (horsepower^2 and weight^2):\n")
  print(summary(lm_transformed2))
}

# Problem 10
