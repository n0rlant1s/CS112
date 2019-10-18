##############
# QUESTION 5 #  
##############
library(boot)

nsw <- read.csv(url("https://tinyurl.com/yx8tqf3k")) 
set.seed(12345)
head(nsw)

# Sample for test set
test_set <- sample(1:length(nsw$age), 2000, replace = FALSE) 

# Test set data
test_set_data <- nsw[test_set, ]

# Training set data
training_set_data <- nsw[-test_set, ]

# Checking for accurate length
length(test_set_data$age)
length(training_set_data$age)
length(nsw$age)

# MODEL 1 LOOCV
glm.fit_1 <- glm(treat ~ age + education + black + hispanic + nodegree + married, data = training_set_data)
summary(glm.fit_1)
cv.error_1 = cv.glm(training_set_data, glm.fit_1)
names(cv.error_1)
round(cv.error_1$delta, 8)

# MODEL 1 TEST-SET ERROR
test_MSE_1 <- mean((test_set_data$treat - predict.glm(glm.fit_1, test_set_data))^2)
test_MSE_1

# MODEL 2 LOOCV
glm.fit_2 <- glm(treat ~ age + education + black + married, data = training_set_data)
summary(glm.fit_2)
cv.error_2 = cv.glm(training_set_data, glm.fit_2)
names(cv.error_2)
round(cv.error_2$delta, 8)

# MODEL 2 TEST-SET ERROR
test_MSE_2 <- mean((test_set_data$treat - predict.glm(glm.fit_2, test_set_data))^2)
test_MSE_2

# MODEL 3 LOOCV
glm.fit_3 <- glm(treat ~ age + education, data = training_set_data)
summary(glm.fit_3)
cv.error_3 = cv.glm(training_set_data, glm.fit_3)
names(cv.error_3)
round(cv.error_3$delta, 8)

# MODEL 3 TEST-SET ERROR
test_MSE_3 <- mean((test_set_data$treat - predict.glm(glm.fit_3, test_set_data))^2)
test_MSE_3

# MODEL 4 LOOCV
glm.fit_4 <- glm(treat ~ age + education + hispanic, data = training_set_data)
summary(glm.fit_4)
cv.error_4 = cv.glm(training_set_data, glm.fit_4)
names(cv.error_4)
round(cv.error_4$delta, 8)

# MODEL 4 TEST-SET ERROR
test_MSE_4 <- mean((test_set_data$treat - predict.glm(glm.fit_4, test_set_data))^2)
test_MSE_4

# MODEL 5 LOOCV
glm.fit_5 <- glm(treat ~ age + education + hispanic + married, data = training_set_data)
summary(glm.fit_5)
cv.error_5 = cv.glm(training_set_data, glm.fit_5)
names(cv.error_5)
round(cv.error_5$delta, 8)

# MODEL 5 TEST-SET ERROR
test_MSE_5 <- mean((test_set_data$treat - predict.glm(glm.fit_5, test_set_data))^2)
test_MSE_5

