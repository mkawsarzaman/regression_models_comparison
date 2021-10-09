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

poly_reg = lm()