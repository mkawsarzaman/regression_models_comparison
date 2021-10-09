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

lin_reg = lm(formula = PE ~ .,
             data = dataset)
y_pred = predict(object = lin_reg, newdata = test_set)
summary(lin_reg)

# Adjusted R-squared for multiple linear regression:  0.9287

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
summary(poly_reg)

# Adjusted R-squared for multiple linear regression:  0.9382