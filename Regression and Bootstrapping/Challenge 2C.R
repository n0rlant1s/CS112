library(Matching)
library('arm')
data(lalonde)

lalonde$age_squared <- lalonde$age^2

# both treated and control
lalonde.control <- lalonde[which(lalonde$treat == 0), ]
lalonde.treat <- lalonde[which(lalonde$treat == 1), ]

# Fit the regression model
lm.lalonde <- lm(re78 ~ age + age_squared + educ + treat + treat * age + re74 + re75, data = lalonde)
summary(lm.lalonde)

# Simulate
set.seed(123)
iterations <- 10000
sim.lalonde <- sim(lm.lalonde, n.sims = iterations)

sim.lalonde@coef[1,]

# Predict re78 for every unit of age for every set of coefficients holding at the medians
treat.effect <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

mean.educ_control <- mean(lalonde.control$educ)
mean.re74_control <- mean(lalonde.control$re74)
mean.re75_control <- mean(lalonde.control$re75)

mean.educ_treated <- mean(lalonde.treat$educ)
mean.re74_treated <- mean(lalonde.treat$re74)
mean.re75_treated <- mean(lalonde.treat$re75)

for (a in min(lalonde$age):max(lalonde$age)){
  X.treat <- c(1, a, a^2, mean.educ_treated, 1, mean.re74_treated, mean.re75_treated, 1*a)
  X.control <- c(1, a, a^2, mean.educ_control, 0, mean.re74_control, mean.re75_control, 0*a)
  for(i in 1:iterations) {
    treat.effect[i, a + 1 - min(lalonde$age)] <- sum(X.treat*sim.lalonde@coef[i, ]) - 
      sum(X.control*sim.lalonde@coef[i, ])
  }
}

conf.intervals_mean_te <- apply(treat.effect, 2, quantile, probs = c(0.025, 0.975))
table_mean_treat_te <- t(data.frame(conf.intervals_mean_te))
colnames(table_mean_treat_te) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_mean_treat_te <- data.frame(table_mean_treat_te, mean.educ_treated - mean.educ_control, 
                               mean.re74_treated - mean.re74_control, 
                               mean.re75_treated - mean.re75_control)
rownames(table_mean_treat_te) <- min(lalonde$age):max(lalonde$age)
View(table_mean_treat_te)


plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-2000,12000), 
     main = "Re78 by Age With Predictors Held at The Means (Treatment Effect)", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean_te[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean_te[2, age - min(lalonde$age) + 1],
    lwd = 2)
}
