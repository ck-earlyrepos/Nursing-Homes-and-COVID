######
# This section identifies outliers (defined in this study as 2 standard deviations from the mean)

# Clearing the console again
cat("\014")



# The sample standard deviation for "% Wearing Mask"
maskSD <- sd(covidData$`% Wearing Mask`)

# checking which states are outliers wearing masks
covidData %>%
  filter(`% Wearing Mask` >= (2 * maskSD) + mean(`% Wearing Mask`))

covidData %>%
  filter(`% Wearing Mask` <= (-2 * maskSD) + mean(`% Wearing Mask`))



## Wyoming and South Dakota disproportionately wear fewer masks compared to the rest of the country (the second filter)






# Clearing the console again
cat("\014")

# The sample standard deviation for "Population Density"
densitySD <- sd(covidData$`Population Density (p/mi^2)`)


# Checking which states are outliers with respect to population density
covidData %>%
  filter(`Population Density (p/mi^2)` >= (2 * densitySD) + mean(`Population Density (p/mi^2)`))

covidData %>%
  filter(`Population Density (p/mi^2)` <= (-2 * densitySD) + mean(`Population Density (p/mi^2)`))



## DC is more than 6 standard deviations above the mean (the first filter)







# Clearing the console again
cat("\014")

# The sample standard deviation for testing
testingSD <- sd(covidData$`Total Tests per 100k`)


# Checking which states are outliers with respect to available tests
covidData %>%
  filter(`Total Tests per 100k` >= (2 * testingSD) + mean(`Total Tests per 100k`))

covidData %>%
  filter(`Total Tests per 100k` <= (-2 * testingSD) + mean(`Total Tests per 100k`))



## Alaska, DC, and New York have disproportionately more tests per 100k residents (the first filter)









# Clearing the console again
cat("\014")

# The sample standard deviation for elderly
elderlySD <- sd(covidData$Elderly)


# Checking which states are outliers with respect to the elderly population
covidData %>%
  filter(`Elderly` >= (2 * elderlySD) + mean(`Elderly`))

covidData %>%
  filter(`Elderly` <= (-2 * elderlySD) + mean(`Elderly`))



## Florida and Maine have disproportionately large elderly populations (the first filter)
## DC, Texas, and Utah have disproportionately low elderly populations (the second filter)














# Clearing the console again
cat("\014")


# The sample standard deviation for nursing
nursingSD <- sd(covidData$Nursing)


# Checking which states are outliers with respect to the elderly population
covidData %>%
  filter(`Nursing` >= (2 * nursingSD) + mean(`Nursing`))

covidData %>%
  filter(`Nursing` <= (-2 * nursingSD) + mean(`Nursing`))



## No outliers for nursing home populations














noRestr <- covidData %>%
  filter(`Has Strict Lockdown` == 0)

restr <- covidData %>%
  filter(`Has Strict Lockdown` == 1)

noRestr <- ggplot(data = noRestr) +
  geom_boxplot(mapping = aes(`Death Rate`)) +
  xlab("Death Rate (no strict lockdown)")
restr <- ggplot(data = restr) +
  geom_boxplot(mapping = aes(`Death Rate`)) +
  xlab("Death Rate (strict lockdown)") +
  xlim(c(0, 8))

# plotting everything against the death rate
qplot(covidData$Nursing, covidData$`Death Rate`, xlab = "Nursing", ylab = "Death Rate") +
  qplot(covidData$`Total Tests per 100k`, covidData$`Death Rate`, xlab = "Tests", ylab = "Death Rate") +
  qplot(covidData$Elderly, covidData$`Death Rate`, xlab = "Elderly", ylab = "Death Rate") +
  qplot(covidData$`% Wearing Mask`, covidData$`Death Rate`, xlab = "Mask", ylab = "Death Rate") +
  qplot(covidData$`Population Density (p/mi^2)`, covidData$`Death Rate`, xlab = "Density (Excluding DC)", ylab = "Death Rate", xlim = c(0, 1500)) /
  qplot(covidData$`Population Density (p/mi^2)`, covidData$`Death Rate`, xlab = "Density (Including DC)", ylab = "Death Rate") +
  restr / noRestr














### I am keeping all observations, despite DC being a far outlier in population density
minusDC <- covidData[-8,]

# Let's remove some variables we no longer need
rm(densitySD)
rm(elderlySD)
rm(maskSD)
rm(nursingSD)
rm(testingSD)




