######
# This section checks the correlation of the variables

# Clearing the console again
cat("\014")


# Finding the correlation of all non-categorical/dummy, numeric variables
correlation <- as.data.frame(cor(covidData[,c(-1 ,-5, -8)]))
print(correlation, digits = 2)

# Renaming the variables the way they are in my official model in the document
colnames(correlation) <- c("Mask", "Density", "Tests", "Elderly", "Nursing")
rownames(correlation) <- c("Mask", "Density", "Tests", "Elderly", "Nursing")


# Visualizing correlation between the variables (corrplot takes a matrix as input, I think)
corrplot(corr = as.matrix(correlation),
         method = "pie",
         type = "upper",
         tl.srt = 0,
         outline = "black",
         tl.pos = "d",
         tl.col = "black",
         tl.cex = 1.5,
         cl.cex = 1.5,
         diag = T)


## Bigger problems:         Population density and masks, testing and population density
## Questionable problems:   tests and masks, nursing and masks, elderly and density, elderly and testing



### I think dropping density will improve the model--there is no transformation that can be made, either





























# linear model of masks against density
summary(lm(formula = covidData$`Population Density (p/mi^2)` ~ covidData$`% Wearing Mask`))


# linear model of masks against nursing
summary(lm(formula = covidData$Nursing ~ covidData$`% Wearing Mask`))

# linear model of density against tests
summary(lm(formula = covidData$`Total Tests per 100k` ~ covidData$`Population Density (p/mi^2)`))










death <- covidData$`Death Rate`
nursing <- covidData$Nursing
elderly <- covidData$Elderly
mask <- covidData$`% Wearing Mask`
tests <- covidData$`Total Tests per 100k`
lockdown <- covidData$`Has Strict Lockdown`
density <- covidData$`Population Density (p/mi^2)`

results <- lm(death ~ nursing +
                elderly + mask +
                density +
                tests +
                lockdown)

# The variance proportion table (also provides eigenvalues in column 1)
print(olsrr::ols_eigen_cindex(results)[, -c(1, 3, 9)], digits = 3)

# The same variance proportion table, but with the perturb package
perturb::colldiag(results)


# The VIF and Tolerance collinearity statistics
print(olsrr::ols_vif_tol(model = results), digits = 4)


print(olsrr::ols_vif_tol(model = lm(formula = mtcars$mpg ~ mtcars$cyl)), digits = 4)













### After removing the population density variable
results4 <- lm(death ~ nursing +
                 elderly +
                 mask +
                 tests +
                 lockdown)
summary(results4)

# Gives us the standardized coefficients
QuantPsyc::lm.beta(results4)






### After adding the urban density variable
results5 <- lm(death ~ nursing +
                 tests +
                 elderly +
                 mask +
                 urban$Urban.Density +
                 lockdown)
summary(results5)

summary(lm(death ~ urban$Urban.Density))

summary(results4)



# The variance proportion table
print(olsrr::ols_eigen_cindex(results4)[, -c(3, 9)], digits = 3)

# The same variance proportion table, but with the perturb package
perturb::colldiag(results4)


# The VIF and Tolerance collinearity statistics
print(olsrr::ols_vif_tol(model = results4), digits = 4)


c <- data.frame("Mask" = mask, "Urban" = urban$Urban.Density, "Tests" = tests, "Elderly" = elderly, "Nursing" = nursing)
correlation2 <- as.data.frame(cor(as.matrix(c)))
# Another corrplot
corrplot(corr = as.matrix(correlation2),
         method = "number",
         type = "upper",
         tl.srt = 0,
         outline = "black",
         tl.pos = "d",
         tl.col = "black",
         tl.cex = 1.5,
         cl.cex = 1.5,
         diag = T)
cor(c)








# The variance proportion table
print(olsrr::ols_eigen_cindex(results5)[, -c(3, 9)], digits = 3)

# The same variance proportion table, but with the perturb package
perturb::colldiag(results5)


# The VIF and Tolerance collinearity statistics
print(olsrr::ols_vif_tol(model = results5), digits = 4)




































# Variable transformations (lockdown stays the same)
# "Log-Lin" method
nursingLN <- log(nursing)
testsLN <- log(tests)
elderlyLN <- log(elderly)
maskLN <- log(mask)
densityLN <- log(density)

logLin <- lm(death ~ nursingLN +
               testsLN +
               elderlyLN +
               maskLN +
               densityLN +
               lockdown)
summary(logLin)# It did not improve the results


# "Lin-Log" method
deathLN <- abs(log(death))

# Death has values that are zero, so we must replace them with a very small value
deathLN[22] <- 0.00000000000000000000000000000000001
deathLN[47] <- 0.00000000000000000000000000000000001


linLog <- lm(formula = deathLN ~ nursing +
               tests +
               elderly +
               mask +
               density +
               lockdown)
summary(linLog)
QuantPsyc::lm.beta(linLog)
standardErrorofEstimate(linLog)




# "Log-Log" method
logLog <- lm(deathLN ~ nursingLN +
               testsLN +
               elderlyLN +
               maskLN +
               densityLN +
               lockdown)
summary(logLog)








# because I am curious, let's do this with the urban density instead of the state density
urbanLN <- log(urban$Urban.Density)

# "Lin-Log" method
results6 <- lm(deathLN ~ nursing +
             tests +
            elderly +
             mask +
             urban$Urban.Density +
             lockdown)
QuantPsyc::lm.beta(results6)
summary(results6)
# No way, it's even more significant!




# Let's get rid of the elderly variable
# "Lin-Log" method
results7 <- lm(deathLN ~ nursing +
             tests +
             mask +
             urban$Urban.Density +
             lockdown)
QuantPsyc::lm.beta(results7)
summary(results7)