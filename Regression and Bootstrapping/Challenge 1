##############
# QUESTION 1 #
##############

set.seed(1234)
# Independent and dependent variables 
x <- rnorm(998)
y = -2*x + 60 + rnorm(998, 1, 2)

# Data frame with 998 entries and regressiion results
df_998 <- data.frame(x,y)
lm.1_998 <- lm(y ~ x, data = df_998)
summary(lm.1_998)

# Outliers
df_1000 <- rbind(df_998, c(6, 100), c(12, 600))

# Regression results with outliers
lm.1_1000 <- lm(y ~ x, data = df_1000)
summary(lm.1_1000)

# Generating plot
plot(df_1000,
     xlim = c(0, 5),
     main = "Regression line for both 998 points and 1000 points",
     xlab = "Years of weekly exposure to toxic chemicals (x)",
     ylab = "Life expectancy (y)")
abline(a = coef(lm.1_998)[1],
       b = coef(lm.1_998)[2],
       col = "blue")
abline(a = coef(lm.1_1000)[1],
       b = coef(lm.1_1000)[2],
       col = "red")
