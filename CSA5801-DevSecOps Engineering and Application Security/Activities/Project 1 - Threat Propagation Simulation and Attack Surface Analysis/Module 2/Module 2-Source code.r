############################################################
# STEP 3 : DATA PREPROCESSING (1000 RECORDS)
############################################################

# Install packages (Run only once)
install.packages("readxl")
install.packages("dplyr")

# Load libraries
library(readxl)
library(dplyr)

############################################################
# Read Dataset
############################################################

dataset <- read_excel("Synthetic_Threat_Propagation_Dataset_1000_GMM.xlsx")

############################################################
# Dataset Information
############################################################

head(dataset)

tail(dataset)

dim(dataset)

str(dataset)

summary(dataset)

############################################################
# Check Missing Values
############################################################

missing_values <- colSums(is.na(dataset))

print(missing_values)

############################################################
# Remove Missing Values
############################################################

dataset <- na.omit(dataset)

############################################################
# Check Duplicate Records
############################################################

duplicates <- sum(duplicated(dataset))

print(duplicates)

############################################################
# Remove Duplicate Records
############################################################

dataset <- distinct(dataset)

############################################################
# Convert Categorical Variables
############################################################

categorical_columns <- c(
  "Threat_Type",
  "Threat_Source",
  "Direction",
  "Attack_Vector",
  "Magnitude",
  "Mutation",
  "Transmission_Medium",
  "Boundary",
  "Status"
)

dataset[categorical_columns] <-
  lapply(dataset[categorical_columns], as.factor)

############################################################
# Convert Numeric Variables
############################################################

numeric_columns <- c(
  "Propagation_Speed",
  "Detection_Time_sec",
  "Impact_Score",
  "Propagation_Probability"
)

dataset[numeric_columns] <-
  lapply(dataset[numeric_columns],
         function(x) as.numeric(as.character(x)))

############################################################
# Verify Data Types
############################################################

str(dataset)

############################################################
# Normalize Numeric Variables
############################################################

normalize <- function(x){
  
  (x-min(x))/(max(x)-min(x))
  
}

dataset$Propagation_Speed <-
  normalize(dataset$Propagation_Speed)

dataset$Detection_Time_sec <-
  normalize(dataset$Detection_Time_sec)

dataset$Impact_Score <-
  normalize(dataset$Impact_Score)

dataset$Propagation_Probability <-
  normalize(dataset$Propagation_Probability)

############################################################
# Summary After Normalization
############################################################

summary(dataset)

############################################################
# Outlier Detection
############################################################

boxplot.stats(dataset$Propagation_Speed)$out

boxplot.stats(dataset$Detection_Time_sec)$out

boxplot.stats(dataset$Impact_Score)$out

boxplot.stats(dataset$Propagation_Probability)$out

############################################################
# Save Preprocessed Dataset
############################################################

write.csv(dataset,
          "Preprocessed_Threat_Dataset.csv",
          row.names = FALSE)

############################################################
# Final Output
############################################################

cat("\n======================================\n")
cat(" DATA PREPROCESSING COMPLETED\n")
cat("======================================\n")

cat("\nTotal Records :", nrow(dataset))

cat("\nTotal Columns :", ncol(dataset))

cat("\nMissing Values Removed :", sum(missing_values))

cat("\nDuplicate Records Removed :", duplicates)

cat("\nDataset Saved Successfully\n")

############################################################
# Display Dataset
############################################################

View(dataset)

head(dataset)

summary(dataset)

library(ggplot2)

ggplot(dataset, aes(x = Threat_Type, fill = Threat_Type)) +
  geom_bar() +
  labs(
    title = "Threat Type Distribution",
    x = "Threat Type",
    y = "Number of Threats"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggplot(dataset, aes(x = Attack_Vector, fill = Attack_Vector)) +
  geom_bar() +
  labs(
    title = "Attack Vector Distribution",
    x = "Attack Vector",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggplot(dataset, aes(x = Magnitude, fill = Magnitude)) +
  geom_bar() +
  labs(
    title = "Threat Magnitude Distribution",
    x = "Magnitude",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggplot(dataset,
       aes(x = Impact_Score,
           y = Propagation_Probability)) +
  geom_point(color = "blue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Impact Score vs Propagation Probability",
    x = "Impact Score",
    y = "Propagation Probability"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggplot(dataset, aes(x = Propagation_Speed)) +
  geom_histogram(binwidth = 0.05,
                 fill = "steelblue",
                 color = "black") +
  labs(
    title = "Propagation Speed Distribution",
    x = "Propagation Speed",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
