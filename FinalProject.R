#### Final Project Machine Learning #######
library(caret)
##Download test and training data sets #####
setwd("~/Data_Science_Specialization/8_MachineLearning/FinalProject/")
if (!file.exists("data")) {
        dir.create("data")
}
urlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlValid <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urlTrain, destfile = "./data/training.csv")
download.file(urlTest, destfile = "./data/Valid.csv")

list.files("./data")

# Open the files:
data <- read.csv("./data/training.csv")
valid <- read.csv("./data/Valid.csv")

#note classe is the dependent variable we want to predict with a description of
# the classes below

# Six young health participants were asked to perform one set of 10 repetitions
# of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly
# according to the specification (Class A), throwing the elbows to the front
# (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell
# only halfway (Class D) and throwing the hips to the front (Class E).


inTrain <- createDataPartition(y=data$classe,
                               p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
dim(training); dim(testing)

## Pre-processing
# Get rid of empty or almost completely empty columns (and also the non-predictors)
columns <- c(8:11, 37:49,60:68,84:86, 102, 113:124,140,151:160)
training <- training[,columns]
testing <- testing[,columns]
#colnames(training) #note column 53 is the classe.
#Test if there are any missing values we need to impute
sum(complete.cases(training)) - length(training$roll_belt) #no


#models
set.seed(1234)
# fitCon <-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
cv <- trainControl(method="cv", number=3)
# rffit<-train(classe~.,data=training, method="rf", trControl=fitCon, verbose=F)
# modFit <- rffit
modFitRf <- train(classe~.,data=training, method="rf", trControl=cv, verbose=F) #.9956 accuracy
#try glm
set.seed(32343)
# modelFit <- train(classe ~.,data=training, method="glm")
# modelFit <- train(classe ~.,data=training,
#                   preProcess=c("center","scale"),method="glm") #standardize
# #pre-processes with PC as part of the model fitting        
# modelFit <- train(classe ~ .,method="glm",preProcess="pca",data=training, trControl = cv)
# modelFit <- train(classe ~ .,method="rpart",data=training)
# modFit <- train(classe~ .,data=training,method="rf",prox=TRUE)
# modFit <- train(classe ~ ., method="gbm",data=training,verbose=FALSE, trConrol = cv)
# modlda = train(classe ~ .,data=training,method="lda", trControl = cv) ##run
# modFit4 = train(classe ~ ., data=subset(training, select =-c(1:10)),method="nb", trControl = cv) #nb is naive bayes.  Only accuracy of .696
# modFit3 <- train(classe ~.,data=subset(training,select=-c(1:10)),method="rpart", trControl = cv) #only accuracy of around 44%
# 
# 
# modelFit

#predictions
predictions <- predict(modFitRf,newdata=testing)
confusionMatrix(predictions,testing$classe)

# prediction_lda <- predict(modlda, newdata = testing)
# confusionMatrix(prediction_lda, testing$classe) #only 70%

#Next predict the Validation Set
predictions <- predict(modFitRf,newdata=valid)
valid$predictions <- predictions



