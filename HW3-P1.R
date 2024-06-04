#--SETTING UP DATA--
data <- read.csv('/Users/itayakad/Desktop/Data 101 - HW3/japan_birth.csv')
# Filter the data set for the years 2000+
filtered_data <- subset(data, year >= 2000)

#B-O STATEMENT
# What are the odds that the fertility rate has increased after 2012?
# Belief: The fertility rate has increased since 2000.
# Observation: The increased fertility rate occurs most commonly in years after 2012.

# Define the threshold for a "high" fertility rate based on the 60th percentile of rates since 2000
threshold <- quantile(filtered_data$total_fertility_rate, 0.6, na.rm = TRUE)
cat("Threshold:", threshold, "\n")

# Calculate the prior probability of having a fertility rate above this threshold in all years since 2000
Prior <- sum(filtered_data$total_fertility_rate > threshold, na.rm = TRUE) / nrow(filtered_data)
cat("Prior probability:", Prior, "\n")

# Convert the prior probability into prior odds
PriorOdds <- round(Prior / (1 - Prior), 2)
cat("Prior odds:", PriorOdds, "\n")

# Calculate the true positive rate
observation_period <- subset(filtered_data, year >= 2012)
TruePositive <- sum(observation_period$total_fertility_rate > threshold, na.rm = TRUE) / nrow(observation_period)
cat("True positive rate:", TruePositive, "\n")

# Calculate the false positive rate
FalsePositive <- sum(filtered_data[filtered_data$year < 2012 & filtered_data$total_fertility_rate > threshold,]$total_fertility_rate > threshold, na.rm = TRUE) / nrow(filtered_data[filtered_data$year < 2012,])
cat("False positive rate:", FalsePositive, "\n")

# Calculate the likelihood ratio
LikelihoodRatio <- round(TruePositive / FalsePositive, 2)
cat("Likelihood ratio:", LikelihoodRatio, "\n")

# Calculate posterior odds
PosteriorOdds <- LikelihoodRatio * PriorOdds
cat("Posterior odds:", PosteriorOdds, "\n")

# Convert the posterior odds back into a probability
Posterior <- PosteriorOdds / (1 + PosteriorOdds)
cat("Posterior probability:", Posterior, "\n")