# This section checks the dataset for autocorrelation

# Testing the model for autocorrelation using the Durbin-Watson test
durbinWatsonTest(results8)


## Since the p-value is greater than 0.05, we cannot reject the null
## hypothesis (which states that the variables have NO autocorrelation).
## We can conclude, therefore, that the variables are NOT autocorrelated.