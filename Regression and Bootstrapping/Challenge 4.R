##############
# QUESTION 4 #  
##############

# Boostrap function for calculating r^2
r_squared <- function(true_y, predicted_y) {
  iterations <- 200000
  storage <- rep(NA, iterations)
  for (i in 1:iterations){
    indices <- sample(1:length(true_y), length(true_y), replace = T)
    new_true_y <- true_y[indices]
    new_pred_y <- predicted_y[indices]
    rss <- sum((new_true_y - new_pred_y)**2)
    tss <- sum((new_true_y - mean(new_true_y))**2)
    storage[i] <- (1 - rss/tss)
  }
  return(mean(storage))
}

# Retrieving and cleaning data
afterschool <- read.csv(url("https://tinyurl.com/y2prc9xq"))
afterschool_na.math <- afterschool[!is.na(afterschool$MATH_SCORE), ]
lm.afterschool <- lm(MATH_SCORE ~ TREATMENT, data = afterschool_na.math)

# R squared by using function
r_squared(afterschool_na.math$MATH_SCORE, lm.afterschool$fitted.values)
# R squared by using bootstrap
summary(lm(MATH_SCORE ~ TREATMENT , data = afterschool))$r.squared
