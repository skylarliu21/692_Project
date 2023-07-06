library(tidyverse)
library(scales)
library(gridExtra)
library(glmnet)
library(data.table)
library(psych)
library(MASS)

########### Pre-Processing
xtrain <-rio::import("C:\\Users\\Skylar\\OneDrive\\Documents\\692 - Consulting\\Data\\X_train.csv") %>%
  mutate(BestSquatKg = as.numeric(gsub("\\.\\.", "\\.", BestSquatKg)),
         BestSquatKg = if_else(grepl('-', BestSquatKg), BestSquatKg * -1, BestSquatKg))
ytrain <-rio::import("C:\\Users\\Skylar\\OneDrive\\Documents\\692 - Consulting\\Data\\y_train.csv") 
xtest <-rio::import("C:\\Users\\Skylar\\OneDrive\\Documents\\692 - Consulting\\Data\\X_test.csv")
ytest <-rio::import("C:\\Users\\Skylar\\OneDrive\\Documents\\692 - Consulting\\Data\\y_test.csv") %>%
  select(playerId, BestBenchKg) %>%
  mutate(BestBenchKg = if_else(grepl('-', BestBenchKg), BestBenchKg * -1, BestBenchKg))

BenchDataTrain <- xtrain %>%
  left_join(ytrain, by = "playerId") %>%
  mutate(AgeGroup = if_else(Age < 20, "0-19", 
                            if_else(Age >= 20 & Age < 25, "20-24", 
                                    if_else(Age >= 25 & Age < 30, "25-29",
                                            if_else(Age >= 30 & Age < 40, "30-39",
                                                    if_else(Age >= 40 & Age < 50, "40-49",
                                                            if_else(Age >= 50, "50+", ""))))
                            ))) %>%
  filter(!is.na(Age))

BenchDataTest <- xtest %>%
  left_join(ytest, by = "playerId") %>%
  mutate(AgeGroup = if_else(Age < 20, "0-19", 
                            if_else(Age >= 20 & Age < 25, "20-24", 
                                    if_else(Age >= 25 & Age < 30, "25-29",
                                            if_else(Age >= 30 & Age < 40, "30-39",
                                                    if_else(Age >= 40 & Age < 50, "40-49",
                                                            if_else(Age >= 50, "50+", ""))))
                            ))) %>%
  filter(!is.na(Age))

############# Early visualizations
ggplot(data = BenchDataTrain, aes(x = BestDeadliftKg, y = BestBenchKg, color = Sex)) +
  geom_point(size = 1) +
  facet_wrap(facets = vars(AgeGroup)) +
  labs(title = "Best Bench Press Single vs Best Deadlift Single by Age Group and Gender",
       x = "Deadlift Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender")
ggplot(data = BenchDataTest, aes(x = BestDeadliftKg, y = BestBenchKg, color = Sex)) +
  geom_point(size = 1) +
  facet_wrap(facets = vars(AgeGroup)) +
  labs(title = "Best Bench Press Single vs Best Deadlift Single by Age Group and Gender",
       x = "Deadlift Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender")

pairs.panels(BenchDataTrain[,-c(BenchDataTrain$BestBenchKg)])
hist(BenchDataTrain$BestBenchKg)
boxplot(BenchDataTrain$BodyweightKg)
boxplot(BenchDataTrain$BestBenchKg)
boxplot(BenchDataTrain$BestSquatKg)
qqnorm(BenchDataTrain$BestBenchKg)
qqline(BenchDataTrain$BestBenchKg)

model <- lm(BestBenchKg ~ BodyweightKg + BestSquatKg + Sex + AgeGroup, BenchDataTrain)
res <- resid(model)
qqnorm(res)
qqline(res)

par(mfrow = c(2,2))
plot(model)

b <- boxcox(lm(BestBenchKg ~ BodyweightKg + BestSquatKg + Sex + AgeGroup, BenchDataTrain))
lambda <- b$x[which.max(b$y)]
lambda
BenchDataTrain$BodyweightKgtransform <- sqrt(BenchDataTrain$BodyweightKg)
BenchDataTrain$BestSquatKgtransform <- sqrt(BenchDataTrain$BestSquatKg)
BenchDataTrain$BestBenchKgtransform <- sqrt(BenchDataTrain$BestBenchKg)
modeltransform <- lm(BestBenchKgtransform ~ BodyweightKgtransform + BestSquatKgtransform + Sex + AgeGroup, BenchDataTrain)
restransform <- resid(modeltransform)
qqnorm(restransform)
qqline(restransform)

BenchTrainTransform <- subset(BenchDataTrain, select = -c(BodyweightKg, BestSquatKg, BestBenchKg))
pairs.panels(BenchTrainTransform[,-c(BenchTrainTransform$BestBenchKgtransform)])
qqnorm(BenchTrainTransform$BestBenchKgtransform)
qqline(BenchTrainTransform$BestBenchKgtransform)

par(mfrow = c(2,2))
plot(modeltransform)

# Sample too large to perform Shapiro-Wilks test (would tend to reject)

############## Variable selection
BenchDataTrain$Sex <- as.factor(BenchDataTrain$Sex)
BenchDataTrain$AgeGroup <- as.factor(BenchDataTrain$AgeGroup)
BenchDataTrain$Equipment <- as.factor(BenchDataTrain$Equipment)

x_quant <- BenchDataTrain[, -c(2,3,4,9,10)]
x_sex  <- model.matrix(~ Sex - 1, BenchDataTrain)
x_Equip  <- model.matrix(~ Equipment - 1, BenchDataTrain)
x_Age  <- model.matrix(~ AgeGroup - 1, BenchDataTrain)
x <- as.matrix(cbind(x_quant, x_sex, x_Equip, x_Age))
y <- BenchDataTrain$BestBenchKg

lasso_model <- glmnet(x, y, family = "gaussian")
cv_model <- cv.glmnet(x, y, family = "gaussian")
lambda_min <- cv_model$lambda.min
lambda_lse <- cv_model$lambda.1se 
lasso_coeflse <- coef(lasso_model, s = lambda_lse)
lasso_coefmin <- coef(lasso_model, s = lambda_min)
# Keep BodyWeight, Squat, Sex, and AgeGroup
