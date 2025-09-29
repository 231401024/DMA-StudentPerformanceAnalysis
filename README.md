Student Performance Analysis
Project Overview

This project analyzes student performance data to predict whether a student will Pass or Fail based on demographic information, parental education, test preparation, and exam scores. It uses data preprocessing, visualization, and machine learning models to generate actionable insights.

Features

Data Cleaning & Preprocessing: Handles categorical variables and calculates average scores.

Exploratory Data Analysis (EDA): Visualizes score distributions, Pass/Fail counts, and correlations.

Predictive Modeling:

Logistic Regression

Decision Tree

Random Forest

Feature Importance: Identifies key factors affecting student performance.

Export Cleaned Data: Generates CSV file for use in dashboards or further analysis.

Requirements

R (version >= 4.0)

R packages: tidyverse, caret, randomForest, corrplot, rpart, rpart.plot

How to Run

Install required packages:

install.packages(c("tidyverse", "caret", "randomForest", "corrplot", "rpart", "rpart.plot"))


Load the libraries:

library(tidyverse)
library(caret)
library(randomForest)
library(corrplot)
library(rpart)
library(rpart.plot)


Load the dataset:

data <- read.csv("C:/Users/Dharun/Documents/StudentsPerformance.csv")


Run the script to perform EDA, build models, and export cleaned data.

Output

Visualizations: Histograms, boxplots, correlation heatmaps, and model plots.

Model Performance: Confusion matrices for all models.

Cleaned Data: cleaned_students_performance.csv ready for Power BI or further analysis.
