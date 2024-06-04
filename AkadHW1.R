#--SETTING UP THE CODE--
japan_demographics <- read.csv("/Users/itayakad/Desktop/Data 101 - HW1/japan_birth.csv")

#--CREATING VARIABLES--
# Average birth rate
mean_birth_rate <- tapply(japan_demographics$birth_rate, japan_demographics$year, mean)
# Average birth gender ratio
mean_gender_ratio <- mean(japan_demographics$birth_gender_ratio, na.rm = TRUE)
# Max birth rate
max_birth_rate <- max(japan_demographics$birth_rate, na.rm = TRUE)
# Min birth rate
min_birth_rate <- min(japan_demographics$birth_rate, na.rm = TRUE)
# Average infant death rate per year
mean_infant_death_rate <- tapply(japan_demographics$infant_death_rate, japan_demographics$year, mean)
# Population growth
population_growth <- max(japan_demographics$population_total) - min(japan_demographics$population_total)
# Average stillbirth rate per year
mean_stillbirth_rate <- tapply(japan_demographics$stillbirth_rate, japan_demographics$year, mean)
# Average mother's age per year
mean_mother_age <- tapply(japan_demographics$mother_age_avg, japan_demographics$year, mean)
# Birth order distribution (firstborn, secondborn, thirdborn)
birth_order_distribution <- table(japan_demographics$firstborn, japan_demographics$secondborn, japan_demographics$thirdborn)
# Average infant death gender ratio
mean_infant_death_gender_ratio <- mean(japan_demographics$infant_death_gender_ratio, na.rm = TRUE)
# Average fertility rate per year
fertility_rate_changes <- tapply(japan_demographics$total_fertility_rate, japan_demographics$year, mean)
# Average father's age
mean_father_age <- mean(japan_demographics$father_age_avg, na.rm = TRUE)
# Converting years to decades for grouping purposes
japan_demographics$decade <- (japan_demographics$year %/% 10) * 10
# Average birth gender ratio by decade
decade_average <- aggregate(birth_gender_ratio ~ decade, data=japan_demographics, FUN=mean, na.rm=TRUE)
# Average mother's age at first birth by decade
decade_age <- tapply(japan_demographics$mother_age_firstborn, japan_demographics$decade, mean, na.rm = TRUE)

#--CREATING PLOTS--
# Average birth gender ratio by decade
barplot(decade_average$birth_gender_ratio, names.arg=decade_average$decade, main="Average Birth Gender Ratio by Decade", xlab="Decade", ylab="Average Gender Ratio", col="blue")
# Total fertility rates
boxplot(japan_demographics$total_fertility_rate, main="Distribution of Total Fertility Rate", ylab="Total Fertility Rate", col="lightblue", notch=FALSE)
# Population growth over time
plot(japan_demographics$year, japan_demographics$population_total, main="Population Growth Over Time", xlab="Year", ylab="Total Population", pch=19, col="green")
# Infant death rate over time
barplot(japan_demographics$infant_death_rate, names.arg=japan_demographics$year, main="Infant Death Rate Over Time", xlab="Year", ylab="Infant Death Rate", col="coral", las=2)
# Mother's age at first birth
boxplot(japan_demographics$mother_age_firstborn, main="Distribution of Mother's Age at First Birth", ylab="Mother's Age", col="lightgreen", notch=FALSE)
# Birth rate vs infant death rate
plot(japan_demographics$birth_rate, japan_demographics$infant_death_rate, main="Birth Rate vs. Infant Death Rate", xlab="Birth Rate", ylab="Infant Death Rate", pch=20, col="purple")
# Birth rate over time
plot(japan_demographics$year, japan_demographics$birth_rate, main="Birth Rate Over Time", xlab="Year", ylab="Birth Rate", pch=19, col="orange", lwd=2)
# Average mother's age at first birth by decade
barplot(decade_age, main="Average Mother's Age at First Birth by Decade", xlab="Decade", ylab="Average Age", col=rainbow(length(decade_age)))

#--PREDICTOR BELOW--
#This predictor attempts to predict high or low birth rate years based on infant death rate and birth total
# Calculate the average birth rate
mean_birth_rate <- mean(japan_demographics$birth_rate, na.rm = TRUE)
# Categorize each year as 'High' or 'Low' birth rate based on being above or below the average
japan_demographics$ActualCategory <- ifelse(japan_demographics$birth_rate > mean_birth_rate, 'High', 'Low')
# Calculate the average infant death rate
mean_infant_death_rate <- mean(japan_demographics$infant_death_rate, na.rm = TRUE)
# Calculate the average total number of births
mean_birth_total <- mean(japan_demographics$birth_total, na.rm = TRUE)
# Initialize predictions with 'Low' and update to 'High' based on infant death rate and birth total
japan_demographics$predicted_birth_rate <- 'Low'
# Update predictions to 'High' for years exceeding the average infant death rate and birth total
japan_demographics$predicted_birth_rate[japan_demographics$infant_death_rate > mean_infant_death_rate & japan_demographics$birth_total > mean_birth_total] <- 'High'
# Compare predictions with actual categories to create a logical vector of correct predictions
correct_predictions <- japan_demographics$predicted_birth_rate == japan_demographics$ActualCategory
# Calculate the proportion of correct predictions to determine model accuracy
accuracy <- mean(correct_predictions, na.rm = TRUE)
# Output the accuracy of the predictor
accuracy
