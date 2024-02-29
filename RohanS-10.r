# Install and load necessary packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

if (!requireNamespace("randomForestExplainer", quietly = TRUE)) {
  install.packages("randomForestExplainer")
}
library(randomForestExplainer)

library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(readr)




# Load the dataset
bank_data <- read_delim("bank.csv", delim = ";",
                        escape_double = FALSE, trim_ws = TRUE)

# Check column names in the dataset
names(bank_data)

# Data Exploration
summary(bank_data)
str(bank_data)
head(bank_data)

# Data Preprocessing
# Convert categorical variables to factors
bank_data <- bank_data %>%
  mutate_if(is.character, as.factor)

# Handle missing values
bank_data <- bank_data %>%
  na.omit()

# Data Visualization
# Bar plot for job categorical variable
barplotjob = ggplot(bank_data, aes(x = job, fill = education)) +
  geom_bar(position = "dodge") +
  ggtitle("Categorical Variable Distribution by Education Level") +
  theme_minimal()

# Box plot for balance distribution by marital status
boxplotbalance = ggplot(bank_data, aes(x = marital, y = balance, fill = marital)) +
  geom_boxplot() +
  ggtitle("Balance Distribution by Marital Status") +
  theme_minimal()

# Model Training and Evaluation
set.seed(123)
# Split the data into training and testing sets
split_index <- createDataPartition(bank_data$y, p = 0.7, list = FALSE)
train_data <- bank_data[split_index, ]
test_data <- bank_data[-split_index, ]

# Train a random forest model
rf_model <- randomForest(y ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, test_data$y)
print(conf_matrix)

# Check the structure of rf_model$importance
str(rf_model$importance)

# If the structure is not as expected, print the entire rf_model object
print(rf_model)

# Extract feature importance using randomForestExplainer
importance <- data.frame(Feature = rownames(rf_model$importance), Importance = rf_model$importance[, 1])

# Sort by importance
importance <- importance[order(importance$Importance, decreasing = TRUE), ]

# Bar plot for feature importance
importancebarplot = ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Feature Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Specify the project folder
project_folder <- "/Users/rohan/Desktop/RohanSinghStats"

# Check if the folder exists, if not, create it
if (!dir.exists(project_folder)) {
  dir.create(project_folder)
}

# Open a connection to a file to save console output
sink(file.path(project_folder, "console_output.txt"))

# Your R code generating console output
cat("Hello, this is console output.\n")
print("More console output.")

# Close the connection to the file
sink()



# Save the bar plot for job categorical variable
ggsave("barplot_job_education.png", plot = barplotjob)

# Save the box plot for balance distribution by marital status
ggsave("boxplot_balance_marital.png", plot = boxplotbalance)

# Save the bar plot for feature importance
ggsave("barplot_feature_importance.png", plot = importancebarplot)
