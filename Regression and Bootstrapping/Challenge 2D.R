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

# Predict re78 for every unit of age for every set of coefficients holding at the medians
treat.effect <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Medians for control variables
median.educ_control <- median(lalonde.control$educ)
median.re74_control <- median(lalonde.control$re74)
median.re75_control <- median(lalonde.control$re75)

# Medians for treated variables
median.educ_treated <- median(lalonde.treat$educ)
median.re74_treated <- median(lalonde.treat$re74)
median.re75_treated <- median(lalonde.treat$re75)

for (a in min(lalonde$age):max(lalonde$age)) {
  X.treat <- c(1, a, a^2, median.educ_treated, 1, median.re74_treated, median.re75_treated, 1*a)
  X.control <- c(1, a, a^2, median.educ_control, 0, median.re74_control, median.re75_control, 0*a)
  for(i in 1:iterations) {
    treat.effect[i, a + 1 - min(lalonde$age)] <- (sum(X.treat*sim.lalonde@coef[i, ]) + rnorm(1, 0, sim.lalonde@sigma[i])) - 
      (sum(X.control*sim.lalonde@coef[i, ]) + rnorm(1, 0, sim.lalonde@sigma[i])) 
  }
}

# Confidence intervals and tables
conf.intervals_median_te <- apply(treat.effect, 2, quantile, probs = c(0.025, 0.975))
table_median_treat_te <- t(data.frame(conf.intervals_median_te))
colnames(table_median_treat_te) <- c("Median PI Lower Bound", "Median PI Upper Bound")
table_median_treat_te <- data.frame(table_median_treat_te, median.educ_treated - median.educ_control, 
                                  median.re74_treated - median.re74_control, 
                                  median.re75_treated - median.re75_control)
rownames(table_median_treat_te) <- min(lalonde$age):max(lalonde$age)
table_median_treat_te
View(table_median_treat_te)


plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-20000, 30000), 
     main = "Re78 by Age With Predictors Held at The Medians", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_median_te[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_median_te[2, age - min(lalonde$age) + 1],
    lwd = 2)
}
