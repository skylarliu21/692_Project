library(tidyverse)
library(scales)
library(gridExtra)
library(glmnet)
library(data.table)
library(psych)

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
ggplot(data = BenchDataTrain, aes(x = BestSquatKg, y = BestBenchKg, color = Sex)) +
  geom_point(size = .5) +
  facet_wrap(facets = vars(AgeGroup)) +
  labs(title = "Best Bench Press Single vs Best Squat Single by Age Group and Gender",
       x = "Squat Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12),
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1, 'cm')) 

ggplot(data = BenchDataTrain, aes(x = BestDeadliftKg, y = BestBenchKg, color = Sex)) +
  geom_point(size = .5) +
  facet_wrap(facets = vars(AgeGroup)) +
  labs(title = "Best Bench Press Single vs Best Deadlift Single by Age Group and Gender",
       x = "Deadlift Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12),
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1, 'cm')) 

ggplot(data = BenchDataTrain, aes(x = BodyweightKg, y = BestBenchKg, color = Sex)) +
  geom_point(size = .5) +
  facet_wrap(facets = vars(AgeGroup)) +
  labs(title = "Best Bench Press Single vs Body Weight by Age Group and Gender",
       x = "Body Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12),
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1, 'cm')) 

par(cex.main=2.5, cex.lab = 1.5, cex.axis = 1.5 )
boxplot(BenchDataTrain$BodyweightKg,BenchDataTrain$BestSquatKg,BenchDataTrain$BestDeadliftKg,BenchDataTrain$BestBenchKg,
        main = "Boxplots of Numeric Variables",
        xlab = "Numeric Variables",
        ylab = "Frequency",
        names = c("Bodyweight", "Best Squat Max", "Best Deadlift Max", "Best Bench Max"),
        col = c("pink","green","purple","white"))
qqnorm(BenchDataTrain$BestBenchKg,
       main = "Normal Q-Q Plot of Response Variable")
qqline(BenchDataTrain$BestBenchKg)

model <- lm(BestBenchKg ~ BodyweightKg + BestSquatKg + BestDeadliftKg + Sex + AgeGroup, BenchDataTrain)

par(mfrow = c(2,2))
plot(model)

#Outlier detection and deletion
cooksd <- cooks.distance(model)
influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(BenchDataTrain)))])
cleaned_data <- BenchDataTrain[-influential, ]

#pairs.panels(cleaned_data[,-c(cleaned_data$BestBenchKg)])
par(mfrow = c(1,1))
boxplot(cleaned_data$BodyweightKg,cleaned_data$BestSquatKg,cleaned_data$BestDeadliftKg,cleaned_data$BestBenchKg,
        main = "Boxplots of Numeric Variables without Bad Influential Outliers",
        xlab = "Numeric Variables",
        ylab = "Frequency",
        names = c("Bodyweight", "Best Squat Max", "Best Deadlift Max", "Best Bench Max"),
        col = c("pink","green","purple","white"))

model_clean <- lm(BestBenchKg ~ BodyweightKg + BestSquatKg + BestDeadliftKg + Sex + AgeGroup, cleaned_data)

par(mfrow = c(2,2))
plot(model_clean)

#Variable transformation
library(MASS)

b <- boxcox(model_clean)
lambda <- b$x[which.max(b$y)]
lambda
detach("package:MASS", unload=TRUE)
range(b$x[b$y > max(b$y)-qchisq(0.95,1)/2]) #Doesn't contain 0.5, use lambda instead of sqrt

transform_data = cleaned_data %>%
  mutate(BodyweightTransform = (BodyweightKg ^ lambda - 1 ) / lambda,
         SquatTransform = (BestSquatKg ^ lambda - 1 ) / lambda,
         BenchTransform = (BestBenchKg ^ lambda - 1 ) / lambda,
         DeadliftTransform = (BestDeadliftKg ^ lambda - 1 ) / lambda) %>%
  select(playerId, Sex, AgeGroup, BodyweightTransform, SquatTransform, DeadliftTransform, BenchTransform)

model_transform <- lm(BenchTransform ~ BodyweightTransform + SquatTransform + DeadliftTransform + Sex + AgeGroup, transform_data)


par(mfrow = c(2,2))
plot(model_transform)

par(mfrow = c(1,1))
boxplot(transform_data$BodyweightTransform,transform_data$SquatTransform,transform_data$DeadliftTransform,transform_data$BenchTransform,
        main = "Boxplots of Numeric Variables with Boxcox Transformation",
        xlab = "Numeric Variables",
        ylab = "Frequency",
        names = c("Bodyweight", "Best Squat Max", "Best Deadlift Max", "Best Bench Max"),
        col = c("pink","green","purple", "white"))

pairs.panels(train_final, main = "Correlation Matrix of Transformed Data")
qqnorm(transform_data$BenchTransform,
       main = "Normal Q-Q Plot of Response Variable with Boxcox Transformation")
qqline(transform_data$BenchTransform)

# Sample too large to perform Shapiro-Wilks test (would tend to reject)

############## Variable selection
transform_data$Sex <- as.factor(transform_data$Sex)
transform_data$AgeGroup <- as.factor(transform_data$AgeGroup)

x_quant <- transform_data[, -c(2,3,7)]
x_sex  <- model.matrix(~ Sex - 1, transform_data)[,1]
x_Age  <- model.matrix(~ AgeGroup - 1, transform_data)[,1]
x <- as.matrix(cbind(x_quant, x_sex, x_Age))
y <- transform_data$BenchTransform

lasso_model <- glmnet(x, y, family = "gaussian")
cv_model <- cv.glmnet(x, y, family = "gaussian")
lambda_min <- cv_model$lambda.min
lambda_lse <- cv_model$lambda.1se 
lasso_coeflse <- coef(lasso_model, s = lambda_lse)
lasso_coefmin <- coef(lasso_model, s = lambda_min)
# Keep BodyWeight, Squat, Sex, and AgeGroup

train_final <- transform_data %>%
  mutate(Sex = as.character(Sex),
         AgeGroup = as.character(AgeGroup),
         BodyweightKg = BodyweightTransform,
         BestSquatKg = SquatTransform,
         BestBenchKg = BenchTransform) %>%
  select(playerId, Sex, AgeGroup, BodyweightKg, BestSquatKg, BestBenchKg)

test_final <- BenchDataTest %>%
  mutate(BodyweightTransform = (BodyweightKg ^ lambda - 1 ) / lambda,
         SquatTransform = (BestSquatKg ^ lambda - 1 ) / lambda,
         BenchTransform = (BestBenchKg ^ lambda - 1 ) / lambda) %>%
  select(playerId, Sex, AgeGroup, BodyweightTransform, SquatTransform, BenchTransform) %>%
  mutate(BodyweightKg = BodyweightTransform,
         BestSquatKg = SquatTransform,
         BestBenchKg = BenchTransform) %>%
  select(playerId, Sex, AgeGroup, BodyweightKg, BestSquatKg, BestBenchKg)

####### Prediction Intervals
library(Metrics)

final_model <- lm(BestBenchKg ~ BodyweightKg + BestSquatKg + Sex + AgeGroup, train_final)
final_test <- test_final %>%
  select(playerId, Sex, AgeGroup, BodyweightKg, BestSquatKg)
ytest <- test_final %>%
  select(playerId, BestBenchKg)
predictions <- predict(final_model, final_test, interval = "prediction")
predictions <- as.data.frame(predictions)
mse <- mean((predictions$fit - ytest$BestBenchKg)^2) #more sensitive to outliers
rmse <- sqrt(mse)
mae <- mae(ytest$BestBenchKg, predictions$fit) #not sensitive to outliers
r_squared <- 1 - sum((ytest$BestBenchKg - predictions$fit)^2) / sum((ytest$BestBenchKg - mean(ytest$BestBenchKg))^2)
adjusted_r_squared <- 1 - (1 - r_squared) * ((nrow(final_test) - 1) / (nrow(final_test) - 4 - 1))

####### Plot prediction intervals vs Actual
back_trans <- test_final %>%
  mutate(BodyweightTransform = (BodyweightKg * lambda + 1) ^ (1 / lambda),
         SquatTransform = (BestSquatKg * lambda + 1) ^ (1 / lambda),
         BenchTransform = (BestBenchKg * lambda + 1) ^ (1 / lambda))
full_test <- merge(back_trans, predictions, by = "row.names", all.x = TRUE) %>%
  mutate(BodyweightKg = BodyweightTransform,
         BestSquatKg = SquatTransform,
         BestBenchKg = BenchTransform,
         Predict = (fit * lambda + 1) ^ (1 / lambda),
         LowerBound = (lwr * lambda + 1) ^ (1 / lambda),
         UpperBound = (upr * lambda + 1) ^ (1 / lambda)) %>%
  select(playerId, Sex, AgeGroup, BodyweightKg, BestSquatKg, BestBenchKg, Predict, LowerBound, UpperBound)

set.seed(8)
ggplot(data = sample_n(full_test, 1000), mapping = aes(x = BodyweightKg, y = BestBenchKg, color = Sex)) +
  geom_point() +
  geom_line(aes(y = LowerBound)) +
  geom_line(aes(y = UpperBound)) +
  geom_ribbon(aes(ymin=LowerBound,ymax=UpperBound), fill= "grey", alpha=0.4) +
  facet_wrap(facets = vars(AgeGroup)) +
  theme_bw() +
  labs(title = "Actual Values vs. Prediction Intervals for Best Bench Press Weight by Body Weight, Age Group, and Gender",
       x = "Body Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender") +
  theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12),
        legend.title = element_text(size=16, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1, 'cm')) 

set.seed(8)
ggplot(data = sample_n(full_test, 1000), mapping = aes(x = BestSquatKg, y = BestBenchKg, color = Sex)) +
  geom_point() +
  geom_line(aes(y = LowerBound)) +
  geom_line(aes(y = UpperBound)) +
  geom_ribbon(aes(ymin=LowerBound,ymax=UpperBound), fill= "grey", alpha=0.4) +
  facet_wrap(facets = vars(AgeGroup)) +
  theme_bw() +
  labs(title = "Actual Values vs. Prediction Intervals for Best Bench Press Weight by Best Squat Weight, Age Group, and Gender",
       x = "Squat Weight (kg)",
       y = "Bench Press Weight (kg)", 
       color = "Gender") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
      axis.title = element_text(size=14),
      axis.text=element_text(size=12),
      legend.title = element_text(size=15, face = "bold"),
      legend.text = element_text(size=12),
      legend.key.size = unit(1, 'cm')) 
