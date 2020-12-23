# NEED TO ADDRESS HETEROSCEDASTICITY with data transformation (and you put it in the model revision section)???



# Some information was easier to write in manually,
# but links to all data used are provided
# in both the sources list
# and throughout the script

# Stores the default number-format settings in R
defaultSettings <- options(scipen = 0, digits = 7)

# Changing the options to remove scientific notation
options(scipen = 999)

#####
# Packages used (all available on CRAN)
  # install.packages("dplyr")
  # install.packages("tabulizer")
  # install.packages("janitor")
  # install.packages("rvest")
  # install.packages("corrplot")
  # install.packages("car")
  # install.packages("ggplot2")
  # install.packages("patchwork")
  # install.packages("skedastic")
  # install.packages("pastecs")

library(dplyr)
library(tabulizer)
library(janitor)
library(rvest)
library(corrplot)
library(car)
library(ggplot2)
library(patchwork)
library(skedastic)
library(pastecs)
library(QuantPsyc)
library(olsrr)
library(perturb)


#####
# This section provides a list of all data sources in this project
sources <- list("https://www.washingtonpost.com/business/2020/10/23/pandemic-data-chart-masks/",
                "https://worldpopulationreview.com/state-rankings/state-densities",
                "https://testing.predictcovid.com/",
                "https://coronavirus.dc.gov/data",
                "https://www.census.gov/quickfacts/fact/table/washingtoncitydistrictofcolumbia,DC/PST045219",
                "https://www.washingtonpost.com/graphics/2020/national/states-reopening-coronavirus-map/",
                "https://www.kff.org/other/state-indicator/number-of-nursing-facility-residents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
                "https://www.kff.org/other/state-indicator/distribution-by-age/?dataView=1&currentTimeframe=0&selectedDistributions=65&selectedRows=%7B%22states%22:%7B%22all%22:%7B%7D%7D,%22wrapups%22:%7B%22united-states%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
                "https://covidtracking.com/data/charts/currently-hospitalized-by-state",
                "http://www.newgeography.com/content/005187-america-s-most-urban-states")

#####
# This section gets the percentage of people wearing a facemask
# and infection rates from a pdf

# We will be using the tabulizer package

# The url of the image, which was converted to pdf, is the table found at this address:
### https://www.washingtonpost.com/business/2020/10/23/pandemic-data-chart-masks/

# I right-clicked on and saved the image as a .svg file, then opened the .svg file in Chrome and
# hit Ctrl+P to save it as a pdf.

# Let's read the pdf stored locally on my computer in my working directory
# (this works the same for a pdf online)
path <- "latest.pdf"

# You can also read in the exact location on your device:
# path <- "C:/Users/crkre/OneDrive/Documents/Miscellaneous/Code/R Tutorials/Scraping data/latest.pdf"
# Note: If the file is stored locally, remove the part before C:/Users
# and get rid of the "%20" that is printed in place of the space character
# EX: file:///C:/Users/crkre/OneDrive/Documents/Siena%20College/Fall%202020/Econometrics/Project/Sources/Covid%20Data/nyse%20and%20gold.pdf
#
# Should be printed
# C:/Users/crkre/OneDrive/Documents/Siena College/Fall 2020/Econometrics/Project/Sources/Covid Data/nyse and gold.pdf




# Works like a charm--but everything on a separate page
# was put into a separate list, meaning we need to join them
tab <- extract_tables(path, output = "data.frame", header = F)
print(tab)

# The results from the first page
covidCasesTop <- tab[[1]][1:3]

# The results from the second page
covidCasesBottom <- tab[[2]][1:3]

# Putting the two together
covidCases <- rbind(covidCasesTop, covidCasesBottom)

# You will see that we want to make the first row the
# name of each column
print(covidCases)

# To be fancy, let's make the first row the column name
# with the janitor package
# This will make the first row the column names
covidCases <- row_to_names(covidCases, 1)

# Now our numbers are messed up! The numbers themselves
# aren't particularly useful, but since this is a small
# dataset, let's make a new dataframe to make
# everything pretty anyway
covidData <- data.frame("State" = covidCases$STATE, "% With Symptoms" = covidCases$`% WITH SYMPTOMS`,
                         "% Wearing Mask" = covidCases$`%WEARING MASK`)

# Renaming columns (I know this makes the janitor package code useless)
# I just kept it in because I think it will be useful to look back on

covidData <- covidData %>%
  rename("State" = State, "% With Symptoms" = X..With.Symptoms, "% Wearing Mask" = X..Wearing.Mask)

# Beautiful
covidData


# Don't forget that the pdf read everything in as a String
mean(covidData$`% Wearing Mask`)
### [1] NA
### Warning message:
###   In mean.default(covidData$`% Wearing Mask`) :
###   argument is not numeric or logical: returning NA


# Now let's make the % With Symptoms and % Wearing Mask columns numeric
covidData$`% With Symptoms` <- as.numeric(covidData$`% With Symptoms`)
covidData$`% Wearing Mask` <- as.numeric(covidData$`% Wearing Mask`)


# Now we can perform mathematical operations
mean(covidData$`% With Symptoms`)
covidData$`% With Symptoms` * c(33333, 12, 9999999999)


# Let's remove some variables we no longer need
rm(covidCases)
rm(covidCasesBottom)
rm(covidCasesTop)
rm(tab)
rm(path)



#####
# This section gets the population density by state from a webpage

# Clearing the console
cat("\014")


# I will be using the rvest package
# Saving the url into a variable
url <- "https://worldpopulationreview.com/state-rankings/state-densities"

# Making a variable that reads the html code at the url
webpage <- read_html(url)

# This reads in the table as html code
tableDataHTML <- html_nodes(webpage, "td:nth-child(2) , td:nth-child(1) , .jsx-2816426159:nth-child(2) .jsx-2816426159 , th:nth-child(1)")

# This saves the table as a String vector
tableData <- html_text(tableDataHTML)


# Giving the state and densities an initial value
state <- "Washington DC"
densityPerSqMile <- "11,815"

# Putting the data into their own vectors
# I am hardcoding a bit here...
# I should probably use a lookup table, too
for(i in 6:107){
  if(i %% 2 == 0){
    state <- c(state, tableData[i]) 
  } else {
    densityPerSqMile <- c(densityPerSqMile, tableData[i]) 
  }
}


# Removing the commas in the density vector
densityPerSqMile <- gsub(",", "", densityPerSqMile)

# Making the density vector numeric
densityPerSqMile <- as.numeric(densityPerSqMile)

# Making a dataframe out of the states and their population densities
byState <- data.frame(state, densityPerSqMile)

# I am not interested in Puerto Rico
byState <- byState[-5, ]

# Now, I have to give each state its abbreviation
# (Thank goodness R comes with a function doing just that!)
byState$state <- state.abb[match(byState$state, state.name)]

# I have to add DC back in
byState$state[1] <- "DC"

# Let's put the rows in alphabetical order
# Using the dplyr package's arrange() function
byState <- arrange(byState, state)

# We are now ready to add the population density to the
# covidData dataframe
covidData <- mutate(covidData, "Population Density (p/mi^2)" = byState$densityPerSqMile)

# Let's remove some variables we no longer need
rm(densityPerSqMile)
rm(state)
rm(tableData)
rm(tableDataHTML)
rm(i)
rm(webpage)
rm(byState)
rm(url)





#####
# This section adds testing data to the dataframe

# Clearing the console
cat("\014")

# This data is easier to get from a simple
# copy and paste (link: https://testing.predictcovid.com/)
# Note: This is data as of 10/23/2020


data <- "North Dakota
38665.3
4968.1

South Dakota
28525.1
4403.2

Louisiana
57750.6
3947.8

Florida
30022.4
3947.1

Mississippi
28196.6
3807

Alabama
27200
3742.8

Tennessee
53548.4
3717.9

Arizona
25455.8
3559.6

South Carolina
34922.7
3529.9

Arkansas
43059.7
3518.8

Idaho
22930.3
3510.9

Utah
34911.6
3499.3

Wisconsin
33864.1
3490.8

Georgia
34102.9
3480.3

Iowa
27599.8
3458.2

Nevada
27978.8
3357

Nebraska
29933.3
3279.9

Texas
28779.2
3219.8

Oklahoma
39433.4
2956.9

Rhode Island
39262.5
2864.3

Illinois
55255.2
2862.5

Missouri
23813.3
2722.2

Kansas
21294.8
2634.1

Montana
45411.3
2610.7

North Carolina
38213.8
2596.5

Delaware
36199
2558.7

New Jersey
48745
2533.1

New York
68567.8
2502.5

Indiana
24434.9
2400.2

Minnesota
31994.9
2395.8

Maryland
31292.8
2339.3

California
45609.6
2313.6

Massachusetts
38838.1
2215.6

Kentucky
40434.2
2132.9

Virginia
30066.9
2073.6

New Mexico
52230.2
1888.3

Connecticut
58834.7
1836.8

Wyoming
21321.4
1809.8

Alaska
75190
1790.3

Michigan
45736.2
1739.4

Colorado
21262.8
1712.5

Ohio
35446
1667.5

Pennsylvania
18721.5
1492

Washington
32938.5
1442

West Virginia
38308.6
1153.6

Oregon
20742.7
1052.1

Hawaii
22056.3
1034.8

New Hampshire
24418.8
755.1

Maine
43234.4
458.9

Vermont
29194.7
321.7

"

# Clearing the console again
cat("\014")


# Splits the string up into a list by each newline character
data <- strsplit(data, "\n")



# Pulling the states out of the list and storing them in a variable
state <- data[[1]][sequence(50, from = 1, to = 197, by = 4)]

# Pulling the total tests per 100k out of the list and storing them in a variable
total <- data[[1]][sequence(50, from = 2, to = 198, by = 4)]

# Pulling the positive tests per 100k out of the list and storing them in a variable
positive <- data[[1]][sequence(50, from = 3, to = 199, by = 4)]

# Changing the state name to abbreviation
state <- state.abb[match(state, state.name)]

# Making the total vector numeric
total <- as.numeric(total)

# Making the positive vector numeric
positive <- as.numeric(positive)



# Making a dataframe
testing <- data.frame(state, total, positive)

# Putting in alphabetical order
testing <- arrange(testing, state)




# Need to add DC data
# According to this website: https://coronavirus.dc.gov/data
# Stats for October 23, 2020:

  # Total Overall Number of Tests: 491553
  # Total Number of DC Residents Tested: 248189
  # Total Positives: 16706
  # Total Lives Lost: 642
  # Cleared From Isolation: 13068


# Census Bureau's estimation for DC population in July 2019: 705749
# link: https://www.census.gov/quickfacts/fact/table/washingtoncitydistrictofcolumbia,DC/PST045219

# Calculating the tests per 100,000
dcPopulation <- 705749

# Calculating the total tests in DC
dcTests <- 491553

# Calculating the positive tests per 100,000
dcPositive <- 16706

# This number looks very comparable to the rest of the data
dcTotalper100000 <- (dcTests / dcPopulation) * 100000

# This number looks very comparable to the rest of the data
dcPositiveper100000 <- (dcPositive / dcPopulation) * 100000


# Adding DC to the dataset
testing <- add_row(testing, .before = 8, state = "DC", total = dcTotalper100000, positive = dcPositiveper100000)



# Adding testing data to the data frame
covidData <- mutate(covidData, "Total Tests per 100k" = testing$total, "Postitive Tests per 100k" = testing$positive)


# Let's remove some variables we no longer need
rm(dcPopulation)
rm(dcPositive)
rm(dcPositiveper100000)
rm(dcTotalper100000)
rm(dcTests)
rm(testing)
rm(state)
rm(positive)
rm(total)
rm(data)







#####
# This section adds a dummy variable that determines the severity of state government lockdowns
# I aggregate Washington Post ratings on a scale of "No restrictions", "Minor Restrictions",
# "Moderate Restrictions", and "Major Restrictions" into two groups
#
# "No Restrictions" and "Minor Restrictions" are 0s
# "Moderate Restrictions" and "Major Restrictions" are 1s
#
# Note: the last time this data was updated was on Spetember 11th, 2020
# link: https://www.washingtonpost.com/graphics/2020/national/states-reopening-coronavirus-map/


# Clearing the console again
cat("\014")

url <- "https://www.washingtonpost.com/graphics/2020/national/states-reopening-coronavirus-map/"

# Making a variable that reads the html code at the url
webpage <- read_html(url)

# This reads in the table as html code
restrictionsDataHTML <- html_nodes(webpage, ".highlight")

# This saves the table as a String vector
restrictions <- html_text(restrictionsDataHTML)

# Removing US Virgin Islands and Puerto Rico
restrictions <- restrictions[-c(40, 46)]


# Making a vector containing the names of states
state <- state.name

# To add DC to the vector, let's first make a dataframe
data <- data.frame(state)

# Adding DC to the dataframe
data <- add_row(data, .before = 8, state = "DC")

# Making a dataframe to store the restriction data
data <- mutate(data, "All" = restrictions, "Minor" = restrictions, "Moderate" = restrictions, "Major" = restrictions)


# Turning the all column into a dummy variable for hasStrictLockdown
data$All <- gsub("No restrictions ", "0", data$All)
data$All <- gsub("Minor restrictions ", "0", data$All)
data$All <- gsub("Moderate restrictions ", "1", data$All)
data$All <- gsub("Major restrictions ", "0", data$All)
data$All <- gsub("vary by region", "", data$All)



# Converting the Strings to logical values to make a
# categorical variable (I think I'll use the dummy
# variable for my model, though)
    # Minor restrictions
data$Minor <- gsub("Minor restrictions ", "1", data$Minor)
data$Minor <- gsub("No restrictions " , "0", data$Minor)
data$Minor <- gsub("Moderate restrictions ", "0", data$Minor)
data$Minor <- gsub("Major restrictions ", "0", data$Minor)
data$Minor <- gsub("vary by region", "", data$Minor)

    # Moderate restrictions
data$Moderate <- gsub("Minor restrictions ", "0", data$Moderate)
data$Moderate <- gsub("No restrictions " , "0", data$Moderate)
data$Moderate <- gsub("Moderate restrictions ", "1", data$Moderate)
data$Moderate <- gsub("Major restrictions ", "0", data$Moderate)
data$Moderate <- gsub("vary by region", "", data$Moderate)

    # Major restrictions
data$Major <- gsub("Minor restrictions ", "0", data$Major)
data$Major <- gsub("No restrictions " , "0", data$Major)
data$Major <- gsub("Moderate restrictions ", "0", data$Major)
data$Major <- gsub("Major restrictions ", "1", data$Major)
data$Major <- gsub("vary by region", "", data$Major)


# Making the Strings numeric
    # Has Strict Lockdown
data$All <- as.numeric(data$All)

    # Minor restrictions
data$Minor <- as.numeric(data$Minor)

    # Moderate restrictions
data$Moderate <- as.numeric(data$Moderate)

    # Major restrictions
data$Major <- as.numeric(data$Major)



# Now, I have to give each state its abbreviation
data$state <- state.abb[match(data$state, state.name)]

# Adding DC back in
data$state[8] <- "DC"

# Rearranging the states
data <- arrange(data, state)

# Adding the lockdown status to the dataset
covidData <- mutate(covidData, "Has Strict Lockdown" = data$All)

# Let's remove some variables we no longer need
rm(restrictions)
rm(state)
rm(url)
rm(webpage)
rm(data)
rm(restrictionsDataHTML)





#####
# This section adds the percent of people ages 65 and older,
# the number of people in certified nursing homes,
# and the 2019 adult population to the dataframe

# Link to find the number of residents in certified nursing homes (2019): https://www.kff.org/other/state-indicator/number-of-nursing-facility-residents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
# Link to find the number of people over the age of 65 by state (2019): https://www.kff.org/other/state-indicator/distribution-by-age/?dataView=1&currentTimeframe=0&selectedDistributions=65&selectedRows=%7B%22states%22:%7B%22all%22:%7B%7D%7D,%22wrapups%22:%7B%22united-states%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D


# Clearing the console again
cat("\014")

# Reading in the number of elderly people by state (2019)
elderlyPop <- read.csv("elderly population.csv", skip = 3)

# Reading in the number of residents in Certified Nursing Facilities
nursingHome <- read.csv("nursing home residents by state.csv", skip = 3)

# Reading in the adult population by state
adults <- read.csv("adult population.csv", skip = 2)


# Finding the total number of adults in each state in 2019
# and storing it into a column
adults <- adults %>%
  rowwise()  %>%
  mutate("Adult Population" = sum(c(Adults.19.25, Adults.26.34, Adults.35.54, Adults.55.64, X65.)))



# Making a new dataframe that is easier to work with
elderly <- data.frame("State" = nursingHome$United.States[1:51],
                      "Percent of Population over 65" = elderlyPop$X0.165[1:51],
                      "Nursing Home Residents" = nursingHome$X1246079[1:51],
                      "Adult Population" = adults$`Adult Population`[2:52])



# Now, I have to give each state its abbreviation
elderly$State <- state.abb[match(elderly$State, state.name)]

# Adding DC back in
elderly$State[9] <- "DC"

# Rearranging the states
elderly <- arrange(elderly, State)

# Adding the percent of population 65-years-old+ to the dataset
covidData <- mutate(covidData, "Elderly" = elderly$Percent.of.Population.over.65 * 100)

# Adding the 2019 adult population to the dataset
covidData <- mutate(covidData, "Adult Population" = elderly$Adult.Population)

# Adding the nursing home residents per 100000
# adults to the dataset
covidData <- mutate(covidData, "Nursing" = ((elderly$Nursing.Home.Residents / covidData$`Adult Population`) * 100))


# Let's remove some variables we no longer need
rm(elderly)
rm(nursingHome)
rm(elderlyPop)
rm(adults)



#####
# This section adds the sum of the number of new deaths from COVID by state from
# October 19th to October 23rd, 2020 (10/19/2020 through 10/23/2020)

# link to source of data (download the csv file from this page): https://covidtracking.com/data/charts/currently-hospitalized-by-state

# Clearing the console again
cat("\014")

data <- read.csv("all-states-history.csv")

# Extracting the column I want and storing it in a vector
# Using the abs() function to remove negative values
newDeaths <- abs(data$deathIncrease[281:560])

# Extracting the state column and storing it in a vector
location <- data$state[281:560]

# Checking the last numbers are correct
tail(newDeaths)
tail(location)


# Making a new dataframe to store the new vectors
deathAndLocation <- data.frame("State" = location, "New Deaths" = newDeaths)

# Calculating the sum of the new deaths
# and rewriting deathAndLocation to make those
# results the new dataframe
deathAndLocation <- deathAndLocation %>%
  group_by(State) %>%
  summarize("New Deaths" = sum(New.Deaths))

# Rearranging the states
deathAndLocation <- arrange(deathAndLocation, State)

# Removing observations that are not the 50 states and DC
deathAndLocation <- deathAndLocation[-4, ]
deathAndLocation <- deathAndLocation[-12, ]
deathAndLocation <- deathAndLocation[-26, ]
deathAndLocation <- deathAndLocation[-40, ]
deathAndLocation <- deathAndLocation[-47, ]

# Adding new deaths per 100000 adults
# to the covidCases dataframe
covidData <- mutate(covidData, "Death Rate" = (deathAndLocation$`New Deaths` / covidData$`Adult Population` * 100000))

# Let's remove some variables we no longer need
rm(data)
rm(deathAndLocation)
rm(location)
rm(newDeaths)

















#####
# This section adds urban population density for each state to the dataframe
#
# Urban Land Area (Square Miles) / Urban Population <-------- as defined by the Census Bureau
#
# Note: This data is from 2010
# link: http://www.newgeography.com/content/005187-america-s-most-urban-states


# Clearing the console again
cat("\014")

url <- "http://www.newgeography.com/content/005187-america-s-most-urban-states"

# Making a variable that reads the html code at the url
webpage <- read_html(url)

# This reads in the table as html code
urbanDataHTML <- html_nodes(webpage, ".excel8 , .excel6 , .excel10:nth-child(4) , .excel10:nth-child(4) , .excel7:nth-child(4)")

# This saves the table as a String vector
urban <- html_text(urbanDataHTML)



state <- ""
ud <- ""
for(i in 1:(length(urban)-1)){
  if(i %%2 == 1){
    # The state name
   state <- c(state, urban[i]) 
  } else {
    # The urban density
    ud <- c(ud, urban[i]) 
  }
}
ud
state

# Removing the extra spaces and
ud <- ud[-c(1, 2)] #also removing the "title"
state <- state[-c(1, 2, 54, 55)]

# Making ud numeric
ud
ud <- gsub(" ", "", ud)
ud <- gsub(",", "", ud)
ud <- substring(text = ud, first = 4)
ud <- as.numeric(ud)
ud <- ud[-52] #removing the national average

# a dataframe containing the urban population densities and state names
# and making the density numeric
urban <- data.frame("State" = state, "Urban Density" = ud)




# Making a vector containing the names of states
state <- state.name

# Changing the state name to abbreviation
urban$State <- state.abb[match(urban$State, state.name)]

# Adding DC back in
urban$State[9] <- "DC"

# Putting in alphabetical order
urban <- arrange(urban, State)

# NOT ADDED TO covidDATA!!!! Remember this















#####
# Removing variables from the dataframe that might be useful for future work
# but are not pertinent to this project
covidData <- select(covidData, -c("% With Symptoms", "Postitive Tests per 100k", "Adult Population"))
