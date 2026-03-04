library(titanic)

head(titanic_train)

## Drop NA (missing values)
titanic_train <- na.omit(titanic_train)
nrow(titanic_train)

## Split data
set.seed(23)
n <- nrow(titanic_train)
id <- sample(1:n, size = n*0.7) ## 70% train, 30% test
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

## Train model
train_model <- glm(Survived ~ Pclass + Sex + Age, data = train_data, family = "binomial")
summary(train_model)
pred_train <- predict(train_model, type = "response")
train_data$pred <- ifelse(pred_train >= 0.5, 1, 0)
train_data$Survived == train_data$pred
mean(train_data$Survived == train_data$pred)

## Test model
pred_test <- predict(train_model, newdata = test_data, type = "response")
test_data$pred <- ifelse(pred_test >= 0.5, 1, 0)
test_data$Survived == test_data$pred
mean(test_data$Survived == test_data$pred)


## Accuracy
mean(train_data$Survived == train_data$pred)
mean(test_data$Survived == test_data$pred)
