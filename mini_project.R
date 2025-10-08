# -----------------------------
# Student Performance Mini Project
# -----------------------------
install.packages(c("tidyverse", "caret", "randomForest", "corrplot", "rpart", "rpart.plot"))
# Load Libraries
library(tidyverse)
library(caret)
library(randomForest)
library(corrplot)
library(rpart)
library(rpart.plot)

# Load Dataset
data <- read.csv("C:/Users/Dharun/Documents/StudentsPerformance.csv")

# View structure
str(data)
summary(data)

# -----------------------------
# 1. Data Cleaning & Preprocessing
# -----------------------------

# Convert categorical columns to factors
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

# Create target variable: Pass if average >= 50, else Fail
data$avg_score <- rowMeans(data[, c("math.score", "reading.score", "writing.score")])
data$pass <- ifelse(data$avg_score >= 50, "Pass", "Fail")
data$pass <- as.factor(data$pass)

# Remove raw average if needed
# keep avg_score for visualization
head(data)

# -----------------------------
# 2. Exploratory Data Analysis (EDA)
# -----------------------------

# Summary statistics
summary(data$avg_score)

# Histogram of average scores
hist(data$avg_score, main="Distribution of Average Scores", col="skyblue", xlab="Average Score")

# Pass/Fail distribution
table(data$pass)
barplot(table(data$pass), col=c("red", "green"), main="Pass vs Fail")

# Boxplot: Math score by gender
boxplot(math.score ~ gender, data=data, main="Math Scores by Gender", col=c("pink","lightblue"))

# Correlation heatmap (numeric only)
num_data <- data %>% select(math.score, reading.score, writing.score, avg_score)
corrplot(cor(num_data), method="color")

# -----------------------------
# 3. Model Building
# -----------------------------

# Train-Test Split
set.seed(123)
trainIndex <- createDataPartition(data$pass, p=0.7, list=FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Logistic Regression
log_model <- glm(pass ~ gender + race.ethnicity + parental.level.of.education +
                   lunch + test.preparation.course,
                 data=train, family=binomial)

log_pred <- predict(log_model, newdata=test, type="response")
log_class <- ifelse(log_pred > 0.5, "Pass", "Fail")
confusionMatrix(as.factor(log_class), test$pass)

# Decision Tree
tree_model <- rpart(pass ~ ., data=train, method="class")
rpart.plot(tree_model)
tree_pred <- predict(tree_model, newdata=test, type="class")
confusionMatrix(tree_pred, test$pass)

# Random Forest
rf_model <- randomForest(pass ~ . - avg_score, data = train, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, newdata=test)
confusionMatrix(rf_pred, test$pass)

# Feature Importance (Random Forest)
importance(rf_model)
varImpPlot(rf_model)

# -----------------------------
# 4. Export Cleaned Data for Power BI
# -----------------------------
write.csv(data, "cleaned_students_performance.csv", row.names=FALSE)


# -----------------------------
# 5. Future Prediction
# -----------------------------

# Create a new hypothetical student record
future_student <- data.frame(
  gender = factor("female", levels = levels(data$gender)),
  race.ethnicity = factor("group B", levels = levels(data$race.ethnicity)),
  parental.level.of.education = factor("bachelor's degree", levels = levels(data$parental.level.of.education)),
  lunch = factor("standard", levels = levels(data$lunch)),
  test.preparation.course = factor("completed", levels = levels(data$test.preparation.course)),
  math.score = 70,
  reading.score = 80,
  writing.score = 78
)

# Predict using the trained Random Forest model
future_student$avg_score <- mean(c(future_student$math.score,
                                   future_student$reading.score,
                                   future_student$writing.score))
future_pred_class <- predict(rf_model, newdata = future_student)

future_pred_prob <- predict(rf_model, newdata = future_student, type = "prob")

cat("Predicted Pass/Fail:", as.character(future_pred_class), "\n")
print("Prediction Probabilities:")
print(future_pred_prob)

# -----------------------------
# Optional: Predict future average score using Regression
# -----------------------------

# Train a linear regression model on current data
score_model <- lm(avg_score ~ gender + race.ethnicity + parental.level.of.education +
                    lunch + test.preparation.course + math.score + reading.score + writing.score,
                  data = data)

# Predict average score for the new student
future_pred_score <- predict(score_model, newdata = future_student)

cat("Predicted Average Score:", round(future_pred_score, 2))

