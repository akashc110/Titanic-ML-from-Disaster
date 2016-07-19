rm(list=ls())
library(DMwR)
data_0 <- read.csv("train.csv")
test_0 <- read.csv("test.csv")


knnOutput <- knnImputation(data_0[, !names(data_0) %in% "Survived"])  # perform knn imputation.
train_set_1 <- cbind.data.frame(knnOutput,Survived = data_0$Survived) # knnOutput$PassengerId
test_0 <- knnImputation(test_0[])


#Combine the two data sets to perform similar operations
test_0$Survived <- NA
merge_data <-  rbind(train_set_1,test_0)

#Including name in the model
name <- as.character(merge_data$Name)

title <-sapply(name, FUN =  function(x) {(strsplit(x,split = '[,.]'))[[1]][2]})
title <- sub(' ','',title)
surname <- sapply(name, FUN =  function(x) {(strsplit(x,split = '[,.]'))[[1]][1]})

table(title)
title <- as.vector(title)

title[title %in% c('Capt','Don','Major', 'Sir')] <- 'Sir' 
title[title %in% c('Dona','Lady','the Countess', 'Jonkheer','Mlle','Mme')] <- 'Lady' 
title[title %in% 'Ms'] <- 'Miss'
title <- as.factor(title);

table(title)

merge_data$Title <- title

#Including family size
merge_data$familySize <- NA
merge_data$familySize <- merge_data$SibSp + merge_data$Parch + 1

merge_data$family_set <- paste(as.character(merge_data$familySize), surname, sep = "")
merge_data$family_set[merge_data$familySize <3] <- 'small'
char <- merge_data$family_set


famIssue <- data.frame(table(merge_data$family_set))
famIssue <- famIssue[famIssue$Freq<3,]

char1 <- famIssue$Var1
char[char %in% char1] <- 'small'

merge_data$family_set <- char


########################################################

#Splitting the sets again
merge_data$family_set<- as.factor(merge_data$family_set)
train <-  merge_data[1:nrow(train_set_1),]
test <- merge_data[(nrow(train_set_1) + 1):nrow(merge_data),]

#########################################################

#Random Forest


#library(randomForest)

#fit <- randomForest(as.factor(Survived) ~ Sex + Age + Pclass + SibSp + Parch + Fare +
#                      Title + Embarked + familySize + family_set,
#                      data = train,importance = TRUE, ntree=2500)

#varImpPlot(fit)

#pre <- predict(fit, test)

#submit_3 <- data.frame(PassengerId = test$PassengerId, Survived = pre)
#write.csv(submit_3, file = "mysub5.csv", row.names = FALSE)

######################################################

#Conditional Random Forest
library(party)

set.seed(755)

fit <- cforest(as.factor(Survived) ~ Sex + Age + Pclass + SibSp + Parch + Fare +
                      Title + Embarked + familySize + family_set,
                    data = train,controls = cforest_unbiased(ntree = 2000, mtry=3))

pre <- predict(fit, test, OOB=TRUE, type = "response")

submit_3 <- data.frame(PassengerId = test$PassengerId, Survived = pre)
write.csv(submit_3, file = "mysub7.csv", row.names = FALSE)





