# --------------------------------------------
# Educational Performance Modeling Project
# Multiple Linear Regression + ANOVA + Diagnostics
# --------------------------------------------

# Load Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(car)

# Set Working Directory (adjust if needed)
setwd("C:/Users/harsh/OneDrive/Documents/educational-performance-modeling")

# --------------------------------------------
# Load Dataset
# --------------------------------------------
data <- read_csv("data/student_performance.csv")

# View structure
str(data)

# --------------------------------------------
# Full Multiple Linear Regression Model
# --------------------------------------------
model <- lm(FinalScore ~ StudyHours + AttendanceRate + ParticipationScore, data = data)

cat("\n--- Full Model Summary ---\n")
print(summary(model))

cat("\n--- ANOVA Table ---\n")
print(anova(model))

cat("\n--- VIF (Multicollinearity Check) ---\n")
print(vif(model))

# --------------------------------------------
# Correlation Matrix
# --------------------------------------------
cat("\n--- Correlation Matrix ---\n")
print(cor(data[, c("StudyHours", "AttendanceRate", "ParticipationScore")]))

# --------------------------------------------
# Reduced Model (Remove ParticipationScore)
# --------------------------------------------
model_reduced <- lm(FinalScore ~ StudyHours + AttendanceRate, data = data)

cat("\n--- Reduced Model Summary ---\n")
print(summary(model_reduced))

cat("\n--- Reduced Model VIF ---\n")
print(vif(model_reduced))

# --------------------------------------------
# Save Regression Plot
# --------------------------------------------
png("visuals/regression_plot.png", width = 800, height = 600)

ggplot(data, aes(x = StudyHours, y = FinalScore)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Study Hours vs Final Score") +
  theme_minimal()

dev.off()

# --------------------------------------------
# Save Residual Diagnostics
# --------------------------------------------
png("visuals/residual_diagnostics.png", width = 800, height = 600)

par(mfrow = c(2,2))
plot(model_reduced)

dev.off()

cat("\nProject Complete.\n")