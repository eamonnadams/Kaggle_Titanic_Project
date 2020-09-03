install.packages("DataExplorer")
install.packages("magrittr")
install.packages("lubridate")
install.packages("e1071")
install.packages("rfm")
library(readxl)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(magrittr)
library(lubridate)
library(e1071)
library(rfm)
library(pROC)
getwd()
#Load data
train_data <- read.csv("train.csv")

#Review data
summary(train_data)
introduce(train_data)
plot_missing(train_data)
#Replacing missing data with 0
train_data <- set_missing(train_data,list(0L,"unknown"))
plot_missing(train_data)

train_data <- train_data %>%
  filter(Age > 0 & Embarked %in% c("S","C","Q"))

#Group Ages into frequency
labs <- c(paste(seq(0, 99, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))
train_data <- train_data %>%
  mutate(Age_groups = cut(Age, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE))


##Exploratory data analysis
#Categorical 
plot_bar(train_data)
#Continuous
plot_histogram(train_data)

#Feature Engineering

#model
logistics_model <- glm(Survived ~ Age+ Sex + Pclass + SibSp, 
                       data = train_data, family = "binomial")
test_data <- read.csv("test.csv")
  
logistics_model_prob <- predict(logistics_model, test_data, type = "response")
head(logistics_model_prob,20)
prediction <- ifelse(logistics_model_prob > 0.5, 1,0)
head(prediction,10)

female_gender <- read.csv("gender_submission.csv")
#test of model 
summary(logistics_model)
ROC_2 <- roc(female_gender$Survived, logistics_model_prob)
plot(ROC_2, col = "blue")
auc(ROC_2)
test_data_final <- test_data %>%
  mutate(model_prediction = prediction)

Final_prediction <-merge(test_data_final,female_gender, by = "PassengerId")
Final_prediction <- Final_prediction %>%
  filter(!is.na(Age))
mean(Final_prediction$model_prediction == Final_prediction$Survived)
