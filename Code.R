library(caret)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)


# Load Data
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
validating <- read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!"))

# Write Function to remove NA Cols
remove_NAcols <- function(data) {
  # Find Cols with NA vlaues
  NAcols_count <- sapply(1:dim(data)[2],function(x) sum(is.na(data[,x])))
  NAcols <- which(NAcols_count > 0)
  
  return(data[,-NAcols])
}

# Remove NA columns
training <- remove_NAcols(training)
validating <- remove_NAcols(validating)

# Remove uninteresting Columns like user name etc.
training <- training[,-c(1:7)]
validating <- validating[,-c(1:7)]

# Transform classe Col to factor
training$classe <- factor(training$classe)

# Set seed and split training data into training and test data
set.seed(1986)
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
training <-training[inTrain,]
testing <- training[-inTrain,]

# Training model with RandomForest
model <- randomForest(classe~., data=training, method='class')
# Predict data
pred <- predict(model,testing,type='class') 

# Plot Data
training %>%
  ggplot(aes(x = roll_belt, y = magnet_dumbbell_y, col = classe)) +
  geom_point(alpha = 0.5) +
  theme_tufte()
  
pred_test <-  predict(model, validating, type='class')
pred_test