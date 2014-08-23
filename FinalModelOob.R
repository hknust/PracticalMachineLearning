#install.packages('doMC')
setwd("~/git/Coursera/DataScience/08_PracticalML/Project/")

library(caret)
library(doMC)

# Remove any variables from the environment
rm(list=ls())

# Data URLs
train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Download files if we don't have  a local copy
if (!file.exists("pml-training.csv")) {
  download.file(url = train.url, destfile = "pml-training.csv") 
}

if (!file.exists("pml-testing.csv")) {
  download.file(url = test.url, destfile = "pml-testing.csv")
}

# Read csv
train.all <- read.csv('pml-training.csv')
test.all <- read.csv('pml-testing.csv')

rem.col.ids <- grep("X|user_name|cvtd_timestamp|raw_timestamp_part_1|raw_timestamp_part_2", names(train.all))
train.clean <- train.all[, -rem.col.ids]

nzv <- nearZeroVar(train.clean[,-155])
train.clean <- train.clean[, -nzv]

# Some columns have mostly NAs and a little more than 400 rows with values
# Remove those columns where the number is less than the threshold of 623 data rows
train.clean <- train.clean[,colSums(is.na(train.clean)) < 19000]

# Set number of available CPU cores to 8
registerDoMC(cores = 8)

# Train the model. Takes 15-20 mins on an i7
model <- train(classe~.,data=train.clean, method="rf", trControl = trainControl(method = "oob"))

# Save the model for later use
#save(model,file="model.RData")

results <- model$results
save(results,file="results.RData")

#load(file = "./modelFinal.RData")

# Remove the columns not used in the training data from the test data
test.clean <- test.all[,which(names(test.all) %in% names(train.clean))]

# Predict the answer for the test data
answers <- predict(model, newdata=test.clean)
save(answers,file="answers.RData")
answers

# Define the function provided to create the answer files and write the answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

setwd("~")