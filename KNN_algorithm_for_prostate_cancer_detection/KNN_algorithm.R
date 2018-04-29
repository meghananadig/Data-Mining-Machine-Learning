                                    

#                                                Assignment 3                                       Meghana B Nadig
#                                  Introduction to Data Mining / Machine Learning                   NUID : 001236144

#1
#2
#3

setwd("C:/Users/Meghana Nadig/Desktop/Knn/") #Using this command, we've imported the 'Prostate_Cancer.csv' data file. This command is used to point to the folder containing the required file. Do keep in mind, that it's a common mistake to use "\" instead of "/" after the setwd command.

prc <- read.csv("C:/Users/Meghana Nadig/Desktop/Knn/prostate_cancer.csv", stringsAsFactors = FALSE) #This command imports the required data set and saves it to the prc data frame.

#stringsAsFactors command helps to convert every character vector to a factor wherever it makes sense.

str(prc) #We use this command to see whether the data is structured or not.

View(prc)

prc <- prc[-1] # #removes the first variable(id) from the data set.

table(prc$diagnosis_result) # it helps us to get the numbers of patients

prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))

View(prc$diagnosis)

round(prop.table(table(prc$diagnosis)) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place( and so it's digits = 1)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }  # Normalization function

prc_n <- as.data.frame(lapply(prc[2:9], normalize)) # Normalizing values in data set

View(prc_n)

summary(prc_n$radius) # summary function to get the mean, median, mode and other statistics

prc_train <- prc_n[1:65,]    # creating training data set
prc_test <- prc_n[66:100,]   # creating test data set

prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1] #This code takes the diagnosis factor in column 1 of the prc data frame and on turn creates prc_train_labels and prc_test_labels data frame.


install.packages('class')
library(class)

View(prc_test_labels)
prc_test_pred <- knn(train = prc_train, test = na.omit(prc_test),cl = prc_train_labels, k=10) # Knn function to classify dataset

View(prc_test_pred)

#install.packages('gmodels')
library(gmodels)
CrossTable(x= na.omit(prc_test_labels), y = prc_test_pred,prop.chisq= FALSE) # check the accuracy of the predicted values in prc_test_pred as to whether they match up with the known values in prc_test_label using CrossTable function.


# There were 8 cases of Benign which are accurately predicted as Benign which constitutes 22.9%. But there were 27 cases of Malignant of which 16 were correctly predicted
# which constitutes 45.7% as Malignant but 11 cases were predicted incorrectly which constitutes to 31.4%. Hence the total accuracy of the model is 68.6% which shows that
# there are chances to improve the model performance.

# Caret Package

install.packages("caret")

library(caret)

setwd("C:/Users/Meghana Nadig/Desktop/Knn/") #Using this command, we've imported the 'Prostate_Cancer.csv' data file. This command is used to point to the folder containing the required file. Do keep in mind, that it's a common mistake to use "\" instead of "/" after the setwd command.

p <- read.csv("C:/Users/Meghana Nadig/Desktop/Knn/prostate_cancer.csv", stringsAsFactors = FALSE) 

str(p)

p <- p[-1]

set.seed(1)

TrainingSet <- createDataPartition(p$diagnosis_result,p =.66, list=FALSE)

TrainingData <- p[TrainingSet,]
Test <- p[-TrainingSet,]
View(TrainingData)
typeof(TrainingSet)

TrainingLabel <- TrainingData[1]
TestLabel <- Test[1]
typet <- train(diagnosis_result ~ ., data = TrainingData, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
of(TrainingLabel)
View(TrainingLabel)

#preprocessing
preProcValues <- preProcess(x = TrainingData,method = c("center", "scale"))

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFi
plot(knnFit)

knnPredict <- predict(knnFit, Test )
View(Test)
View(knnPredict)
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, Test$diagnosis_result)
install.packages('e1071', dependencies=TRUE)


