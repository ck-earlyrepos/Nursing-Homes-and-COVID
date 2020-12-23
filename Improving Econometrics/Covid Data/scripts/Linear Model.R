######
# This section creates a linear model from the data
# The model:
# Deaths = β1 + β2Nursing + β3Elderly + β4Mask + β5Density + β6Test + β7Lockdown



# Clearing the console again
cat("\014")

# The model in code:
results <- lm(data = covidData, `Death Rate` ~ Nursing +
                Elderly + `% Wearing Mask` +
                `Population Density (p/mi^2)`+
                `Total Tests per 100k` +
                `Has Strict Lockdown`)


print(summary(results), digits = 10)



# This gives me the same number as the model does in the "Residual standard error" field
standardErrorofEstimate <- function(mod = results){
  SSE <- sum(residuals.lm(mod)^2)
  N <- 44
  sigma <- sqrt(SSE/N)
  return(sigma)
}
standardErrorofEstimate()


# This gives us the standardized coefficients
QuantPsyc::lm.beta(results)





# nursing squared
nursingsq <- covidData$Nursing^2
testssq <- covidData$`Total Tests per 100k`^2
masksq <- covidData$`% Wearing Mask`^2
elderlysq <- covidData$Elderly^2
densitysq <- covidData$`Population Density (p/mi^2)`^2

# Let's try this with some quadratic data
results2 <- lm(data = covidData, `Death Rate` ~ nursingsq +
                elderlysq + masksq +
                densitysq +
                testssq +
                `Has Strict Lockdown`)
summary(results2)
summary(results)
gtsummary::tbl_regression(results2)










# nursing squared
nursingsq2 <- minusDC$Nursing^2
testssq2 <- minusDC$`Total Tests per 100k`^2
masksq2 <- minusDC$`% Wearing Mask`^2
elderlysq2 <- minusDC$Elderly^2
densitysq2 <- minusDC$`Population Density (p/mi^2)`^2


results3 <- lm(data = minusDC, `Death Rate` ~ nursingsq2 +
                 elderlysq2 + masksq2 +
                 densitysq2 +
                 testssq2 +
                 `Has Strict Lockdown`)
summary(results3)