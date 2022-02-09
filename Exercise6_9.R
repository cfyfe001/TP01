

####################
### Exercise 6-9 ###
####################


remove(list=ls())












## Load necessary libraries
library(ISLR)
library(glmnet)


## Set seed 
set.seed(123)

## Understand 'College' data set
dim(College)
  # 777 rows
  # 18 columns


summary(College)
str(College)

## Question 1:   Split the data set into a training set and a test set ##

  # random split
train <- sample(c(TRUE, FALSE), nrow(College), replace = TRUE)
test <- (!train)


  # Train and testing data
train_college <- College[train, ]
test_college <- College[test, ]



## Question 2:   Fit a linear model using least squares on the training set
## and report the test error obtained.

linear_model <- lm(Apps ~ ., data = train_college)
summary(linear_model)

lin_pred <- predict(linear_model, test_college)
lin_MSE <- mean((lin_pred - test_college$Apps)^2)
lin_MSE



## Question 3:   Fit a ridge regression model on the training set, with ?? chosen
## by cross-validation. Report the test error obtained.

train_matrix <- model.matrix(Apps ~ ., data = train_college)
test_matrix <- model.matrix(Apps ~ ., data = test_college)

grid <- 10^seq(10, -2, length=100)
    # grid is used to find lambda in cross validation; our search range

  # Alpha = 0 is for RIDGE; Alpha = 1 is for LASSO
ridge <- glmnet(train_matrix, train_college$Apps, alpha=0, lambda=grid, thresh = 1e-12)
    # thresh is the limit at which the coordinate descent stops

cv_ridge <- cv.glmnet(train_matrix, train_college[, 'Apps'], alpha=0)
plot(cv_ridge)

best_lam <- cv_ridge$lambda.min
best_lam

ridge_pred <- predict(ridge, newx = test_matrix, s = best_lam)

ridge_MSE <- mean((ridge_pred - test_college[, 'Apps'])^2)
ridge_MSE



## Question 4:   Fit a lasso model on the training set, with ?? chosen by 
## crossvalidation. Report the test error obtained, along with the number of 
## non-zero coefficient estimates.

lasso <- glmnet(train_matrix, train_college$Apps, alpha = 1, lambda = grid, thresh = 1e-12)

cv_lasso <- cv.glmnet(train_matrix, train_college[, 'Apps'], alpha=1)

best_lam <- cv_lasso$lambda.min
best_lam

lasso_pred <- predict(lasso, newx = test_matrix, s = best_lam)

lasso_MSE <- mean((lasso_pred - test_college[, 'Apps'])^2)
lasso_MSE



x <- model.matrix(Apps~., College)[, -2]
y <- College$Apps
out<-glmnet(x,y, alpha=1, lambda=grid)
lasso.coef<-predict(out, type="coefficients", s=best_lam)
print(lasso.coef)



## Question 5:   COMPARE METHODS
lin_MSE; ridge_MSE; lasso_MSE
  
