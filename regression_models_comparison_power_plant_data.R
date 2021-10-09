# Comparison of regression models in R

# Importing the dataset
dataset = read.csv('Data.csv')

# Splitting the dataset into training and test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#multiple linear regression

lin_reg = lm(formula = PE ~ .,
             data = dataset)
y_pred = predict(object = lin_reg, newdata = test_set)
summary(lin_reg)

# Adjusted R-squared for multiple linear regression:  0.9287

# polynomial regression
dataset = read.csv('Data.csv')
library(caTools)
set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

dataset$AT2 = dataset$AT^2
dataset$V2 = dataset$V^2
dataset$AP2 = dataset$AP^2
dataset$RH2 = dataset$RH^2

dataset$AT3 = dataset$AT^3
dataset$V3 = dataset$V^3
dataset$AP3 = dataset$AP^3
dataset$RH3 = dataset$RH^3

poly_reg = lm(formula = PE ~ .,
              data = dataset)
y_pred = predict(object = poly_reg, newdata = test_set)
summary(poly_reg)

# Adjusted R-squared for multiple linear regression:  0.9382

# SVR

dataset = read.csv('Data.csv')
library(caTools)
set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# install.packages('e1071')
library(e1071)
svr = svm(formula = PE ~ ., 
          data = dataset,
          type = 'eps-regression',
          kernel = 'radial')
y_pred = predict(object = svr, newdata = test_set)
summary(svr)


# Decision tree regression
dataset = read.csv('Data.csv')
library(caTools)
set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#install.packages('rpart')
library(rpart)

dt_regressor = rpart(formula = PE ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))
y_pred = predict(object = dt_regressor, newdata = test_set)
summary(dt_regressor)




