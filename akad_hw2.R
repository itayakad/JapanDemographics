# Setting up the code
japan_demographics <- read.csv("/Users/itayakad/Desktop/Data 101 - HW2/japan_birth.csv")
install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
library(PermutationTestSecond)
# Remove rows where any value is missing
japan_demographics_clean <- na.omit(japan_demographics)

#--EXPLORING THE DATA--
#Average total births by year
avg_births_by_year <- tapply(japan_demographics_clean$birth_total, japan_demographics_clean$year, mean)
#Average birth gender ratio by year
avg_gender_ratio_by_year <- tapply(japan_demographics_clean$birth_gender_ratio, japan_demographics_clean$year, mean)
#Average infant death rate by year
avg_infant_death_rate_by_year <- tapply(japan_demographics_clean$infant_death_rate, japan_demographics_clean$year, mean)
#Subset data for years after 2000
data_post_2000 <- subset(japan_demographics_clean, year > 2000)
#Average total fertility rate for years after 2000
avg_fertility_rate_post_2000 <- mean(data_post_2000$total_fertility_rate)
#Subset data for a specific decade (e.g., 1990s)
data_1990s <- subset(japan_demographics_clean, year >= 1990 & year < 2000)
#Average stillbirth rate in the 1990s
avg_stillbirth_rate_1990s <- mean(data_1990s$stillbirth_rate)

#--STRONG HYPO--
#Null Hypothesis (H0): The mean total population does not significantly differ between 1980 and 2010
#Alternative Hypothesis (H1): The mean total population significantly differs between 1980 and 2010
year <- japan_demographics_clean$year
population_total <- japan_demographics_clean$population_total
p <- Permutation(japan_demographics_clean, 'year', 'population_total', 10000, 1980, 2010)
cat("The p-value of the hypothesis is equal to:",p)

#--CLOSE CALL--
#Null Hypothesis (H0): There is no significant difference in the mean birth rate between the years 1980 and 2010
#Alternative Hypothesis (H1): There is a significant difference in the mean birth rate between the years 1980 and 2010
birth_rate <- japan_demographics_clean$birth_rate
p <- Permutation(japan_demographics_clean, 'year', 'birth_rate', 10000, 1980, 2010)
cat("The p-value of the hypothesis is equal to:",p)

#--FAILED TO REJECT--
#Null Hypothesis (H0): There is no significant difference in the birth gender ratio between the years 1980 and 2010
#Alternative Hypothesis (H1): There is a significant difference in the birth gender ratio between the years 1980 and 2010
birth_gender_ratio <- japan_demographics_clean$birth_gender_ratio
p <- Permutation(japan_demographics_clean, 'year', 'birth_gender_ratio', 10000, 1980, 2010)
cat("The p-value of the hypothesis is equal to:",p)

#--NARROW QUERIES--
#For this, I will be using infant death rate
#Average infant death rate for "clean data" (omits rows w/ NA values, so data is from 1980 to present)
M <- mean(japan_demographics_clean$infant_death_rate)
print(M)
#Average death rate from 1980 to 1990
M0 <- mean(subset(japan_demographics_clean, year >= 1980 & year <= 1990)$infant_death_rate)
print(M0)
#Given eq2 = M < 1/2 * M0, then 2.81 < 1/2 * 5.867 == 2.81 < 2.93 is true, so M satisfies eq2
