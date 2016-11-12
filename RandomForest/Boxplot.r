library(ggplot2)
library(randomForest)

set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)

featuresExtraction <- function(data) {
  featuresSelection <- c("Sex",
                       "Age",
                       "SibSp",
                       "Parch",
                       "Fare")
  features <- data[,featuresSelection]
  features$Sex      <- as.factor(features$Sex)
  features$Age[is.na(features$Age)] <- -1
  features$Fare[is.na(features$Fare)] <- median(features$Fare, na.rm=TRUE)
    
  return(features)
}

rf <- randomForest(featuresExtraction(train), as.factor(train$Survived), ntree=100, importance=TRUE)

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, featuresExtraction(test))
write.csv(submission, file = " RandomForest_Data.csv", row.names=FALSE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

boxplot <- qplot(x=Features, y=Importance, geom=”boxplot”)

ggsave("boxplot_rf.png", boxplot)  
  
