library(caret)
library(randomForest)
library(rpart)
library(ggplot2)

# After an inital file load, it has been observed that a lot of variable contained
# NA, blank space and #DIV/0! value.
# Easier and shortest solution to consider blank space, #DIV/0! and NA as NA
train_file <- read.csv(file = 'pml-training.csv',na.strings = c('NA','#DIV/0!',''))
test_file <- read.csv(file = 'pml-testing.csv',na.strings = c('NA','#DIV/0!',''))

#ggplot(train_file) + geom_bar(aes(classe, fill = classe)) + scale_fill_grey(start = 0, end = 1)
ggplot(train_file, aes(classe, ..count..)) + geom_bar()

#Remove column containing NA value (based on the parameter from the read.csv)
#train_file_NA <- apply(train_file,2,function(x) {sum(is.na(x))});
train_data <- train_file[,colSums(is.na(train_file)) == 0]

#data <- data[,-seq(1:7)]
train_data<-train_data[,-(1:7)]

# Partition data set between training and validation 
set.seed(202) 
intrain <- createDataPartition(train_data$classe, p = 0.7, list = FALSE) 
training = train_data[intrain, ] # 70% split 
testing <- train_data[-intrain, ] # 30% split 

