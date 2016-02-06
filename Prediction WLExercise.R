library(caret)

train_data <- read.csv("pml-training.csv", header = TRUE, sep = ",") 
test_data <- read.csv("pml-testing.csv", header = TRUE, sep = ",")