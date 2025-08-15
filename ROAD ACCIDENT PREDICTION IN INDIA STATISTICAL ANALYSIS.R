# Load required libraries
install.packages("dplyr")
library(dplyr)


# Load the dataset
df <- read.csv("C:\\Users\\Sreelakshmi\\Desktop\\accident_prediction_india.csv")

# View structure
str(df)
summary(df)
# Convert Helmet/Seatbelt to factor if needed
df$Helmet_Seatbelt_Used <- as.factor(df$Helmet_or_Seatbelt)
# OPTIONAL: Clean column names (replace spaces with underscores)
names(df) <- gsub(" ", "_", names(df))
names(df)
# STEP 1: Ensure grouping variable is categorical (factor)
df$Helmet_Seatbelt_Used <- as.factor(df$Helmet_Seatbelt_Used)

# STEP 2: Run T-Test comparing mean fatalities
result <- t.test(Number.of.Fatalities ~ df$Helmet_Seatbelt_Used, data = df)

# STEP 3: Print result
print(result)

# STEP 4: Simple interpretation
if(result$p.value < 0.05) {
  print("Statistically significant: Fatalities differ based on helmet/seatbelt use.")
} else {
  print("Not statistically significant: No clear difference in fatalities based on helmet/seatbelt use.")
}
# Load required packages
library(dplyr)



2#. F-Test
# Compare variance in fatalities by Alcohol involvement
df$Alcohol.Involvement <- as.factor(df$Alcohol.Involvement)

f_test_result <- var.test(Number.of.Fatalities ~ df$Alcohol.Involvement, data = df)
print(f_test_result)

if(f_test_result$p.value < 0.05) {
  print("-Test: Variance in fatalities differs by alcohol involvement.")
} else {
  print("F-Test: No significant difference in variance by alcohol involvement.")
}

3#. Z-Test
# Z-test: Compare sample mean of fatalities to hypothetical population mean (e.g., 5)
sample_mean <- mean(df$Number.of.Fatalities, na.rm = TRUE)
sample_sd <- sd(df$Number.of.Fatalities, na.rm = TRUE)
n <- sum(!is.na(df$Number.of.Fatalities))
pop_mean <- 5
pop_sd <- 2
# Step 3: Calculate Z-score and p-value
z_score <- (sample_mean - pop_mean) / (pop_sd / sqrt(n))
p_value_z <- 2 * (1 - pnorm(abs(z_score)))

# Step 4: Print output
cat("Z-Score:", z_score, "\nP-Value:", p_value_z, "\n")

# Step 5: Interpretation
if(p_value_z < 0.05) {
  print("-Test: Sample mean fatalities significantly differ from population mean.")
} else {
  print("Test: No significant difference from population mean.")
}


4#. ANOVA
# ANOVA: Fatalities across different weather conditions
df$Weather_Condition <- as.factor(df$Weather_Condition)
df$Weather_Condition <- as.factor(df$Weather_Condition)

anova_result <- aov(Number.of.Fatalities ~ df$Weather_Condition, data = df)
summary(anova_result)
if(summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  print("ANOVA: Statistically significant difference in fatalities across weather conditions.")
} else {
  print("ANOVA: No significant difference in fatalities across weather conditions.")
}



5. #Chi-Square Test
# Chi-square test: Helmet use vs Accident severity
df$Accident_Severity <- as.factor(df$Accident.Severity)

chisq_data <- table(df$Helmet_Seatbelt_Used, df$AccidentSeverity)
chisq_result <- chisq.test(chisq_data)
print(chisq_result)

if(chisq_result$p.value < 0.05) {
  print("Chi-Square: Significant association between helmet use and accident severity.")
} else {
  print("Chi-Square: No significant association between helmet use and accident severity.")
} 

