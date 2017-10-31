#Importing library
library(randomForest)

#Loading data
data<-train
valid<-test

#data pre-processing
data$Sex[data$Sex == "female"] <- 0
data$Sex[data$Sex == "male"] <- 1
data$Sex <- as.integer(data$Sex)

data$Child <- 0
data$Child[data$Age < 18] <- 1

data$Embarked[data$Embarked == "C"] <- 1
data$Embarked[data$Embarked == "Q"] <- 2
data$Embarked[data$Embarked == "S"] <- 3
data$Embarked <- as.integer(data$Embarked)

data$Fare <- (data$Fare - mean(data$Fare , na.rm = TRUE))/sd(data$Fare , na.rm = TRUE)

data$famsize <- 0
data$famsize <- data$Parch + data$SibSp + 1

data$Age[is.na(data$Age)] <- mean(data$Age , na.rm = TRUE)

data$Embarked[is.na(data$Embarked)] <- 1

#Training random forest model on data
rf <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+Fare+Embarked+Child+famsize , data=data,importance=TRUE,proximity=TRUE, ntree=20000)
round(importance(rf), 2)
##################################################
#pre-processing test data
test$Sex[test$Sex == "female"] <- 0
test$Sex[test$Sex == "male"] <- 1
test$Sex <- as.integer(test$Sex)

test$Child <- 0
test$Child[test$Age < 18] <- 1

test$Embarked[test$Embarked == "C"] <- 1
test$Embarked[test$Embarked == "Q"] <- 2
test$Embarked[test$Embarked == "S"] <- 3
test$Embarked <- as.integer(test$Embarked)

test$Fare <- (test$Fare - mean(test$Fare , na.rm = TRUE))/sd(test$Fare , na.rm = TRUE)

test$famsize <- 0
test$famsize <- test$Parch + test$SibSp + 1

test$Age[is.na(test$Age)] <- mean(test$Age , na.rm = TRUE)

data$Embarked[is.na(test$Embarked)] <- 1

#Predicting solution
pred <- predict(rf , test)
titan <- data.frame(PassengerId= test$PassengerId)
titan$Survived <- pred
titan$Survived[is.na(titan$Survived)] <- 1
write.csv(titan, file="Titan_sol.csv" , row.names=FALSE)
