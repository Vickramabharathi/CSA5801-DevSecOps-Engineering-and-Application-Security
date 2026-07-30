library(readr)
library(dplyr)

# Load dataset
data <- read_csv("cybersecurity_dataset.csv")

set.seed(123)

# Create result column
data$Attack_Result <- "Blocked"

for(i in 1:nrow(data))
{
  p <- data$Probability[i]
  risk <- data$Risk_Score[i]
  
  random <- runif(1)
  
  if(random < p & risk >= 70)
  {
    data$Attack_Result[i] <- "Successful"
  }
  else
  {
    data$Attack_Result[i] <- "Blocked"
  }
}

table(data$Attack_Result)

head(data)

