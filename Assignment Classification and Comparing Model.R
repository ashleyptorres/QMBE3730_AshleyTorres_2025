library(caret)
library(randomForest)
library(dplyr)
library(tidyverse)
library(caTools)

write.csv(loan_default_data_set, "loan_default_data_set.csv", row.names = FALSE) # creating the CSV file
list.files() # Make sure file exists in directory 

# Load dataset
data <- read.csv("loan_default_data_set.csv")
View(loan_default_data_set) # view dataset 

#1.  Do EDA and report you findings.
str(data) # Provides structure of data set       
summary(data) # Provides statistics of the data

#2. Missing Values
colSums(is.na(data))  # Count missing values per column

#Dealing with missing values
data <- data %>% # Using the "dplyr" package 
  mutate( # modify variables in data
    pct_card_over_50_uti = replace(pct_card_over_50_uti, is.na(pct_card_over_50_uti), median(pct_card_over_50_uti, na.rm = TRUE)),# replace missing values with median
    rep_income = replace(rep_income, is.na(rep_income), median(rep_income, na.rm = TRUE)), #replace missing values with median
    rep_education = replace(rep_education, is.na(rep_education), names(sort(table(rep_education), decreasing = TRUE))[1]) # most frequent category will be used to replace missing values
  )

#3. Duplicates
sum(duplicated(data))  # number of duplicate rows shows ZERO
data[duplicated(data), ]  # Displays the rows that are duplicates

#5. Graph Scatter Plot using tot_balance and rep_income
ggplot(data, aes(x = rep_income, y = tot_balance)) +  # x-axis: rep_income, y-axis: tot_balance
  geom_point(color = "blue", alpha = 0.5) +  # color and transparency
  labs(title = "Reported Income vs Total Balance",  # Title of the plot
       x = "Reported Income",  # Label for x-axis
       y = "Total Balance") +  # Label for y-axis
  theme_minimal()  # Using minimal theme can use other 

# 6. Underrepresented education level 
table(data$rep_education) #count of each education level 
prop.table(table(data$rep_education)) # Proportion

# education level with the smallest count
min_education <- names(which.min(table(data$rep_education)))
min_education # chooses other but if we look at the defined ones it is the graduate 

#7. "Def_ind" 

# frequency distribution 
table(data$Def_ind)

# Calculate the proportions of each class (0 and 1)
prop.table(table(data$Def_ind)) # Are imbalanced 

#8. Distribution of "rep_income" 
# Histogram
hist(data$rep_income, main="Histogram of rep_income", xlab="Income", col="lightblue", border="black")

library(e1071)
skew_value <- skewness(data$rep_income)
print(paste("Skewness:", skew_value))

#9. 
# Group by education level and count defaults
default_counts <- table(data$rep_education, data$Def_ind)

# Convert to get default rates
default_rates <- prop.table(default_counts, margin = 1)

# View default rates by education level
default_rates

# Sort the default rates in descending order
sorted_defaults <- sort(default_rates[, "1"], decreasing = TRUE)

# Display the education level with the highest default rate
sorted_defaults



# Split dataset
set.seed(42)
trainIndex <- createDataPartition(data$Def_ind, p=0.8, list=FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Train and evaluate KNN
knn_model <- train(Def_ind ~ ., data=train, method='knn', tuneLength=5) # Fit KNN model
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_knn, test$Def_ind))


# Train and evaluate Decision Tree
dt_model <- train(Def_ind ~ ., data=train, method='rpart') # Fit Decision Tree Model
pred_dt <- predict(dt_model, test)
print(confusionMatrix(pred_dt, test$Def_ind))

## Proceed with evaluation and interpretation of both models.

#ROC/AUC Curve 
prob_dt <- predict(dt_model, test, type = "prob")  #probabilities
roc_curve <- roc(test$Def_ind, prob_dt[,2])  # 2nd column is for "1" (default)
plot(roc_curve, col = "blue", main = "ROC Curve for Decision Tree Model")
auc_value <- auc(roc_curve)
print(auc_value)

#Most Important Features 
importance_dt <- varImp(dt_model)
print(importance_dt)





