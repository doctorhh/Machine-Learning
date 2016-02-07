library(caret)
library(randomForest)
library(rpart)
library(ggplot2)
library(rattle)


# After an inital file load, it has been observed that a lot of variable contained
# NA, blank space and #DIV/0! value.
# Easier and shortest solution to consider blank space, #DIV/0! and NA as NA
train_file <- read.csv(file = 'pml-training.csv',na.strings = c('NA','#DIV/0!',''))
test_file <- read.csv(file = 'pml-testing.csv',na.strings = c('NA','#DIV/0!',''))

#ggplot(train_file) + geom_bar(aes(classe, fill = classe)) + scale_fill_grey(start = 0, end = 1)
ggplot(train_file, aes(classe, ..count..)) + geom_bar()

#Remove column containing NA value (based on the parameter from the read.csv)
train_data <- train_file[,colSums(is.na(train_file)) == 0]
test_data <- test_file[,colSums(is.na(train_file)) == 0]

#data <- data[,-seq(1:7)]
train_data<-train_data[,-(1:7)]
test_data<-test_data[,-(1:7)]

# Partition data set between training and validation 
set.seed(101) 
intrain <- createDataPartition(train_data$classe, p = 0.7, list = FALSE) 
training = train_data[intrain, ] # 70% split 
testing <- train_data[-intrain, ] # 30% split 

# Model Classification Tree
set.seed(202)
# trControl = trainControl(method = "cv", number = 4, allowParallel =TRUE)
# model_fit_Rpart <- train(classe ~.,data = training,method="rpart",trControl=trControl)
# The application of this model yield more accuracy than the train() function at 49%
model_fit_Rpart <- rpart(classe ~ ., data=training, method="class")
model_fit_Rpart
fancyRpartPlot(model_fit_Rpart)

predict_Rpart<-predict(model_fit_Rpart,testing, type="class")
conf_matrix_Rpart<-confusionMatrix(predict_Rpart,testing$classe)
conf_matrix_Rpart

# Model Random Forest
set.seed(303)
#trControl = trainControl(method = "oob") # For Random Forest
model_fit_RF <- randomForest(classe ~ ., training)
model_fit_RF

predict_RF<-predict(model_fit_RF,testing)
conf_matrix_RF<-confusionMatrix(model_test_RF,testing$classe)
conf_matrix_RF

#Test file validation
predict_valid <- predict(model_fit_RF, test_data) 
predict_valid

# code as suggested by Coursera
pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}
pml_write_files(predict_valid)
