# Before Any Changes ---------------------------------
# This section creates a simple linear model for the regression before variable manipulation

summary(lm(data = covidData, formula = `Death Rate` ~ `% Wearing Mask`)) # The linear model for masks
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ `Population Density (p/mi^2)`)) # The linear model for population density
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ `Total Tests per 100k`))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ `Has Strict Lockdown`))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ `Elderly`))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ `Nursing`))


# This will format the model for you
gtsummary::tbl_regression(lm(data = covidData, formula = `Death Rate` ~ `% Wearing Mask`))












# Again, but with quadratic variables
summary(lm(data = covidData, formula = `Death Rate` ~ masksq)) # The linear model for masks
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ densitysq)) # The linear model for population density
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ testssq))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ `Has Strict Lockdown`))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ elderlysq))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = covidData, formula = `Death Rate` ~ nursingsq))







summary(lm(data = minusDC, formula = `Death Rate` ~ masksq2)) # The linear model for masks
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = minusDC, formula = `Death Rate` ~ densitysq2)) # The linear model for population density
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = minusDC, formula = `Death Rate` ~ testssq2))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = minusDC, formula = `Death Rate` ~ `Has Strict Lockdown`))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = minusDC, formula = `Death Rate` ~ elderlysq2))
cat("\n\n\n\n\n\n\n\n")
summary(lm(data = minusDC, formula = `Death Rate` ~ nursingsq2))

