# WAGES 
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(dplyr)

#Load data
Wages_MF <- read_excel("Downloads/Wages MF.xlsx")

#a) 

# Scatter plot
ggplot(Wages_MF, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.7, size =3, color = "blue") +
  labs(title = "Wage vs Age", x = "Age", y = "Wage") +
  theme_minimal()

# Linear model
linear_model <- lm(Wage ~ Age, data = Wages_MF)

# Add linear line
ggplot(Wages_MF, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.7, size = 3, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "solid") +
  labs(title = "Wage vs Age with Linear Model", x = "Age", y = "Wage") +
  theme_minimal()

# Quadratic model
quadratic_model <- lm(Wage ~ Age + I(Age^2), data = Wages_MF)

# Add quadratic line
ggplot(Wages_MF, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.7, size = 3, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "green", linetype = "dashed") +
  labs(title = "Wage vs Age with Quadratic Model", x = "Age", y = "Wage") +
  theme_minimal()



#b)
#Multiple Regression model with Age and Education(Educ) as IV
multiple_R_Model <- lm(Wage ~ Age + Educ, data = Wages_MF)

summary(multiple_R_Model)



#c) 
# Quadratic model + Education
quadratic_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = Wages_MF)

summary(quadratic_model)


#d) 
# Define data for prediction
new_data <- data.frame(Age = c(30, 50, 70), Educ = 16)

# Quadratic model
predicted_wages <- predict(quadratic_model, new_data)
predicted_wages


#e) 
age <- -coef(quadratic_model)["Age"] / (2 * coef(quadratic_model)["I(Age^2)"])
age



#ANN ARBOR 
AnnArbor <- read_excel("Downloads/AnnArbor.xlsx")

#a) 
# Plot Rent vs Bedrooms
ggplot(AnnArbor, aes(x = Beds, y = Rent)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Rent vs Bedrooms", x = "Number of Bedrooms", y = "Rent") +
  theme_minimal()

# Plot Rent vs Bathrooms
ggplot(AnnArbor, aes(x = Baths, y = Rent)) +
  geom_point(alpha = 0.7, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Rent vs Bathrooms", x = "Number of Bathrooms", y = "Rent") +
  theme_minimal()

# Plot Rent vs Square Footage
ggplot(AnnArbor, aes(x = Sqft, y = Rent)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Rent vs Square Footage", x = "Square Footage", y = "Rent") +
  theme_minimal()

#b) 
# log transformation only to Bedrooms and Bathrooms
AnnArbor$log_Beds <- log(AnnArbor$Beds)
AnnArbor$log_Baths <- log(AnnArbor$Baths)

# Log-Transformed Model 
log_model <- lm(Rent ~ Sqft + log_Beds + log_Baths, data = AnnArbor)
summary(log_model)

# Property with log variables
new_property <- data.frame(
  Sqft = 1600, 
  log_Beds = log(3), 
  log_Baths = log(2)
)

# Predicted Rent
predicted_rent <- predict(log_model, newdata = new_property)
predicted_rent




