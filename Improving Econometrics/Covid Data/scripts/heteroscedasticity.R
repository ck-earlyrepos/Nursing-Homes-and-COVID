# This section checks for heteroscedasticity in the data

# Storing the error terms in a variable
error <- residuals.lm(results7)

# Plotting Pearson residuals
residualPlots(results7)


# Squaring the error terms
errorsq <- error^2




# Thank goodness, the data looks like it has homoscedasticity!!!
ggplot(mapping = aes(y = deathLN,x = Normalize(residuals.lm(results7)))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  xlab("Regression Standardized Predicted Values") +
  ylab("Death") +
  ggtitle("Scatterplot\n\nDependent Variable: Death\n") +
  theme(plot.title = element_text(hjust = 0.5))





# Plotting the error terms against the
# dependent variable
heteroDep <- ggplot() +
  geom_point(mapping = aes(x = deathLN, y = errorsq)) +
  geom_hline(yintercept = 0.225) +
  geom_hline(yintercept = -0.01) +
  xlab("Death") +
  ylab("Residual Squared") +
  ggtitle("Examining for Heteroscedasticity") +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting the error terms against the
# predicted values
heteroIndep <- ggplot() +
  geom_point(mapping = aes(x = predict.lm(results7), y = errorsq)) +
  geom_hline(yintercept = 0.225) +
  geom_hline(yintercept = -0.01) +
  xlab("Predicted Value (Xi)") +
  ylab("Residual Squared")


heteroDep / heteroIndep






















###################################################################################################################



# Running linear regressions against the absolute value of the error terms

# Stores the absolute value of the error term
absError <- abs(error)

# The absolute value of the error squared
absErrorsq <- error^2





# The Glejser tests for heteroscedasticity (not used in the paper)
skedastic::glejser(results7)

## Since the p-value is not significant, we FAIL to reject the null hypothesis (which states there is no
## heteroscedasticity), and conclude there is NO heteroscedasticity in the data...








# One of the Glejser tests, done "manually" because
# I need the regression statistics
glej1 <- lm(absError ~ nursing +
             tests +
             mask +
             urban$Urban.Density +
             lockdown)
summary(glej1)
lm.beta(glej1)


# Both Tests and lockdown are nearly heteroscedastic



# Another Gljser test
glej2 <- lm(absErrorsq ~ nursing +
              tests +
              mask +
              urban$Urban.Density +
              lockdown)
summary(glej2)
lm.beta(glej2)













# A variable transformation
tests_inv <- 1 / tests

deathLN_divTest <- deathLN / tests

nursing_divTest <- nursing / tests
mask_divTest <- mask / tests
urban_divTest <- urban$Urban.Density / tests_inv

# Lockdown does not change




# The new regression
results8 <- lm(deathLN_divTest ~ nursing_divTest +
                 tests_inv +
                 mask_divTest +
                 urban_divTest +
                 lockdown)
summary(results8)
lm.beta(results8)



# Storing the error terms in a variable
residualsTransformed <- residuals.lm(results8)
absResidualsTransformed <- abs(residualsTransformed)
residualsTransformedsq <- residualsTransformed^2



# Repeating the Glejser tests with the new model

glej3 <- lm(absResidualsTransformed ~ nursing_divTest +
              tests_inv +
              mask_divTest +
              urban_divTest +
              lockdown)
summary(glej3)
lm.beta(glej3)







glej4 <- lm(residualsTransformedsq ~ nursing_divTest +
              tests_inv +
              mask_divTest +
              urban_divTest +
              lockdown)
summary(glej4)
lm.beta(glej4)



















# Let's remove some variables we no longer need
rm(error)
rm(errorsq)
rm(yplot)
rm(x1plot)
rm(x2plot)
rm(x3plot)
rm(x4plot)
rm(x5plot)
