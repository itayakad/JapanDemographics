#--SETTING UP DATA--
data <- read.csv('/Users/itayakad/Desktop/Data 101 - HW3/japan_birth.csv')
# Filter the dataset for the years 1900 to 1940
filtered_data <- subset(data, year >= 1900 & year <= 1940)

# Categorize Years into Decades
filtered_data$Decade <- cut(filtered_data$year, 
                            breaks = c(1899, 1909, 1919, 1929, 1939, 1941), 
                            labels = c("1900-1909", "1910-1919", "1920-1929", "1930-1939", "1940"), 
                            include.lowest = TRUE)

# Categorize Birth Rates
filtered_data$Birth_Rate_Category <- cut(filtered_data$birth_rate,
                                         breaks = c(-Inf, 30, 31, 32, 33, 34, 35, Inf),
                                         labels = c("Below 30", "30-31", "31-32", "32-33", "33-34", "34-35", "35 or More"),
                                         include.lowest = TRUE)

# Build the contingency table
T <- table(filtered_data$Decade, filtered_data$Birth_Rate_Category)
print(T)

# Analysis for T[i, j] = T[1, 5] = T["1900-1909", "33-34"]
cat("Analysis for observation being birth rates '33-34', belief being in the decade '1900-1909':", "\n")
PriorProb_ij <- sum(T[1,])/sum(T)
PriorOdds_ij <- PriorProb_ij/(1-PriorProb_ij)
cat("Prior Odds of being in the decade '1900-1909':", PriorOdds_ij, "\n")
TruePositive_ij <- T[1, 5] / sum(T[1, ])
FalsePositive_ij <- sum(T[-1, 5]) / sum(T[-1, ])
LikelihoodRatio_ij <- TruePositive_ij / FalsePositive_ij
cat("Likelihood Ratio for being in '1900-1909' when birth rates are '33-34':", LikelihoodRatio_ij, "\n")
PosteriorOdds_ij <- LikelihoodRatio_ij * PriorOdds_ij
Posterior_ij <- PosteriorOdds_ij / (1 + PosteriorOdds_ij)
cat("Posterior Odds of being in '1900-1909' when birth rates are '33-34':", PosteriorOdds_ij, "\n")
cat("Posterior Probability:", Posterior_ij, "\n\n")

# Analysis for T[k, l] = T[4, 1] = T["1930-1939", "Below 30"]
cat("Analysis for observation being birth rates 'Below 30', belief being in the decade '1930-1939':", "\n")
PriorProb_kl <- sum(T[4,])/sum(T)
PriorOdds_kl <- PriorProb_kl/(1-PriorProb_kl)
cat("Prior Odds of being in the decade '1930-1939':", PriorOdds_kl, "\n")
TruePositive_kl <- T[4, 1] / sum(T[4, ])
FalsePositive_kl <- sum(T[-4, 1]) / sum(T[-4, ])
LikelihoodRatio_kl <- TruePositive_kl / FalsePositive_kl
cat("Likelihood Ratio for being in '1930-1939' when birth rates are 'Below 30':", LikelihoodRatio_kl, "\n")
PosteriorOdds_kl <- LikelihoodRatio_kl * PriorOdds_kl
Posterior_kl <- PosteriorOdds_kl / (1 + PosteriorOdds_kl)
cat("Posterior Odds of being in '1930-1939' when birth rates are 'Below 30':", PosteriorOdds_kl, "\n")
cat("Posterior Probability:", Posterior_kl, "\n")


