## Import packages
library(ggplot2)
library(repr)
library(dplyr)
library(caret)
options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions

auto_prices = read.csv('Auto_Prices_Preped.csv')
names(auto_prices)

set.seed(1955)
## Randomly sample cases to create independent training and test data
partition = createDataPartition(auto_prices[,'fuel.type'], times = 1, p = 0.75, list = FALSE)
training = auto_prices[partition,] # Create the training sample
dim(training)
test = auto_prices[-partition,] # Create the test sample
dim(test)

#Scale Numeric Features
num_cols = c('curb.weight', 'horsepower', 'city.mpg')
preProcValues <- preProcess(training[,num_cols], method = c("center", "scale"))

training[,num_cols] = predict(preProcValues, training[,num_cols])
test[,num_cols] = predict(preProcValues, test[,num_cols])
head(training[,num_cols])

## define and fit the linear regression model
lin_mod = lm(log_price ~ curb.weight + horsepower + city.mpg + fuel.type + aspiration +
               body.style + drive.wheels + num.of.cylinders, data = training)

summary(lin_mod)$coefficients

#Evaluate the model
print_metrics = function(lin_mod, df, score, label){
  resids = df[,label] - score
  resids2 = resids**2
  N = length(score)
  r2 = as.character(round(summary(lin_mod)$r.squared, 4))
  adj_r2 = as.character(round(summary(lin_mod)$adj.r.squared, 4))
  cat(paste('Mean Square Error      = ', as.character(round(sum(resids2)/N, 4)), '\n'))
  cat(paste('Root Mean Square Error = ', as.character(round(sqrt(sum(resids2)/N), 4)), '\n'))
  cat(paste('Mean Absolute Error    = ', as.character(round(sum(abs(resids))/N, 4)), '\n'))
  cat(paste('Median Absolute Error  = ', as.character(round(median(abs(resids)), 4)), '\n'))
  cat(paste('R^2                    = ', r2, '\n'))
  cat(paste('Adjusted R^2           = ', adj_r2, '\n'))
}

score = predict(lin_mod, newdata = test)
print_metrics(lin_mod, test, score, label = 'log_price')      

hist_resids = function(df, score, label, bins = 10){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  df$resids = df[,label] - score
  bw = (max(df$resids) - min(df$resids))/(bins + 1)
  ggplot(df, aes(resids)) + 
    geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
    geom_density(aes(y=..density..), color = 'blue') +
    xlab('Residual value') + ggtitle('Histogram of residuals')
}

hist_resids(test, score, label = 'log_price')   

resids_qq = function(df, score, label){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  df$resids = df[,label] - score
  ggplot() + 
    geom_qq(data = df, aes(sample = resids)) + 
    ylab('Quantiles of residuals') + xlab('Quantiles of standard Normal') +
    ggtitle('QQ plot of residual values')
}

resids_qq(test, score, label = 'log_price') 

resid_plot = function(df, score, label){
  df$score = score
  df$resids = df[,label] - score
  ggplot(df, aes(score, resids)) + 
    geom_point() + 
    ggtitle('Residuals vs. Predicted Values') +
    xlab('Predicted values') + ylab('Residuals')
}

resid_plot(test, score, label = 'log_price')

score_untransform = exp(score)
resid_plot(test, score_untransform, label = 'price')