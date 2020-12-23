# This section provides summary tables for all the variables before any variable manipulation

death <- stat.desc(covidData$`Death Rate`)[c(4:6, 8, 9, 13)]
mask <- stat.desc(covidData$`% Wearing Mask`)[c(4:6, 8, 9, 13)]
tests <- stat.desc(covidData$`Total Tests per 100k`)[c(4:6, 8, 9, 13)]
elderly <- stat.desc(covidData$Elderly)[c(4:6, 8, 9, 13)]
nursing <- stat.desc(covidData$Nursing)[c(4:6, 8, 9, 13)]
density <- stat.desc(covidData$`Population Density (p/mi^2)`)[c(4:6, 8, 9, 13)]

print(t(data.frame(death, mask, tests, elderly, nursing, density)), digits = 2) # copy and paste to the Word doc
