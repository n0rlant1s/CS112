library(Matching)
library('arm')
data(lalonde)

lalonde$age_squared <- lalonde$age^2

# only TREATED units
lalonde.control <- lalonde[which(lalonde$treat == 0), ]

# Fit the regression model
lm.lalonde <- lm(re78 ~ age + age_squared + educ + treat + treat * age + re74 + re75, data = lalonde)
summary(lm.lalonde)

# Simulate
set.seed(123)
iterations <- 10000
sim.lalonde_c <- sim(lm.lalonde, n.sims = iterations)

# Checking coefficients to make sure the order of analysis is right
sim.lalonde_c@coef[1,]

# Predict re78 for every unit of age for every set of coefficients holding at the medians
simulated.ys_mean_c <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Finding means for given quantities
mean_educ <- mean(lalonde.control$educ)
mean_re74 <- mean(lalonde.control$re74)
mean_re75 <- mean(lalonde.control$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age^2, mean_educ, 0, mean_re74, mean_re75, 0 * age)
  for (i in 1:iterations) {
    simulated.ys_mean_c[i, age + 1 - min(lalonde$age)] <- sum(Xs*sim.lalonde_c@coef[i,])
  }
}

conf.intervals_mean_c <- apply(simulated.ys_mean_c, 2, quantile, probs = c(0.025, 0.975))
table_mean_treat_c <- t(data.frame(conf.intervals_mean_c))
colnames(table_mean_treat_c) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_mean_treat_c <- data.frame(table_mean_treat_c, mean_educ, mean_re74, mean_re75)
rownames(table_mean_treat_c) <- min(lalonde$age):max(lalonde$age)
View(table_mean_treat_c)


plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-4000,12000), 
     main = "Re78 by Age With Predictors Held at The Means (Control Unit)", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean_c[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean_c[2, age - min(lalonde$age) + 1],
    lwd = 2)
}
