##############
# QUESTION 3 #  
##############
afterschool <- read.csv(url("https://tinyurl.com/y2prc9xq"))
head(afterschool)

# Regression model
lm.afterschool <- lm(MATH_SCORE ~ TREATMENT, data = afterschool)
summary(lm.afterschool)
summary(lm.afterschool)$coef[2]

# Declare bootstrapping function
iterations <- 10000
storage <- rep(NA, iterations)
for (i in 1:iterations) {
  temp_lm = lm(MATH_SCORE ~ TREATMENT, data = afterschool[sample(1:nrow(afterschool), nrow(afterschool), replace = T),])
  storage[i] <- temp_lm$coefficients[2]
}

# plot the histogram and relevant statistics
hist(storage, lwd = 2,
     main = "The histogram of the observed effect",
     ylim = c(0, 1700),
     xlab = "Weight",
     ylab = "Frequency")
abline(v = mean(storage),
       col = rgb(1,0,0,0.3),
       lwd = 5)
abline(v = quantile(storage, 0.025),
       col = "blue",
       lwd = 4)
abline(v = quantile(storage, 0.975),
       col = "blue",
       lwd = 4)
abline(v = summary(lm.afterschool)$coef[2],
       col = rgb(0,1,0,0.3),
       lwd = 2)

# Find simulated confidence intervals and compare with regression standard error
mean(storage)
quantile(storage, c(0.025, 0.975))
summary(lm.afterschool)$coef[2]
confint(lm.afterschool, level = 0.95)
