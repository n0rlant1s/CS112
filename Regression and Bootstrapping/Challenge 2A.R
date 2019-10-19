library(Matching)
library('arm')
data(lalonde)

lalonde$age_squared <- lalonde$age^2

# only TREATED units
lalonde.treat <- lalonde[which(lalonde$treat == 1), ]
max(lalonde.treat$age)

# Fit the regression model
lm.lalonde <- lm(re78 ~ age + age_squared + educ + treat + treat * age + re74 + re75, data = lalonde)
summary(lm.lalonde)

# Simulate
set.seed(123)
iterations <- 10000
sim.lalonde <- sim(lm.lalonde, n.sims = iterations)

# Checking coefficients to make sure the order of analysis is right
sim.lalonde@coef[1,]

# Predict re78 for every unit of age for every set of coefficients holding at the medians
simulated.ys_mean <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Finding means for given quantities
mean_educ <- mean(lalonde.treat$educ)
mean_re74 <- mean(lalonde.treat$re74)
mean_re75 <- mean(lalonde.treat$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age^2, mean_educ, 1, mean_re74, mean_re75, 1 * age)
  for (i in 1:iterations) {
    simulated.ys_mean[i, age + 1 - min(lalonde$age)] <- sum(Xs*sim.lalonde@coef[i,])
  }
}

conf.intervals_mean_t <- apply(simulated.ys_mean, 2, quantile, probs = c(0.025, 0.975))
table_mean_treat_t <- t(data.frame(conf.intervals_mean_t))
colnames(table_mean_treat_t) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_mean_treat_t <- data.frame(table_mean_treat_t, mean_educ, mean_re74, mean_re75)
rownames(table_mean_treat_t) <- min(lalonde$age):max(lalonde$age)
View(table_mean_treat_t)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(0,17000), 
     main = "Re78 (Earnings) by Age With Predictors Held at The Means from Treatment Group", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean_t[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean_t[2, age - min(lalonde$age) + 1],
    lwd = 2)
}
