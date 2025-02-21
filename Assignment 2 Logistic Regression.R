# Load libraries
library(tidyverse)
library(caTools)

# View dataset
View(admit)
admission <- admit


# Convert 'admit' to a factor (categorical variable)
admission$admit <- as.factor(admission$admit)

# View structure of dataset
str(admission)

# Summary statistics
summary(admission)

### Check for class balance 
table(admission$admit)

class_proportions <- prop.table(table(admission$admit))
class_proportions

# Split the data into training and testing sets.
set.seed(1)

# Split the dataset into training (70%) and testing (30%)
split <- sample.split(admission$admit, SplitRatio = 0.7)

# Create training and testing sets
train_data <- subset(admission, split == TRUE)
test_data <- subset(admission, split == FALSE)

# Check dataset dimensions 
dim(train_data) ## always has to be greater than test_date 
dim(test_data)

# Train the logistic regression model
log_model <- glm(admit ~ gre + gpa + rank, data = train_data, family = binomial)

summary(log_model)


# Predict probabilities on the test dataset
pred_probs <- predict(log_model, test_data, type = "response")
pred_probs
# Convert probabilities to binary predictions (threshold = 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factor for comparison
pred_classes <- as.factor(pred_classes)

# Display predictions
head(pred_probs)
head(pred_classes)

### Print predictions and true y values as dataframe
do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit))

conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Print results
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), 
                      color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admission Status",
       x = "GRE Score",
       y = "Admission Status (0 = Not Admitted, 1 = Admitted)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")



# calculate the skewness 
library(e1071)
skew_value <- skewness(admission$gre)
print(paste("Skewness:", skew_value))

