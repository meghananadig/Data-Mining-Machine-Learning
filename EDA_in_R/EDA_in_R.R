
#                             ASSIGNMENT 1                                            MEGHANA B NADIG
#             INTRODUCTION TO DATA MINING / MACHINE LEARNING (DA 5030)                NUID : 001236144

#1. Locate the data set and load the data into R.
DataAssginment1 <- read.table("C:/Users/Meghana Nadig/Downloads/customertxndata.csv",sep = ",",header = TRUE)
View(DataAssginment1)

#2. Calculate the following summative statistics: total number of cases, mean number of visits, median revenue, maximum and minimum number of transactions, most commonly used operating system. Exclude any cases where there is a missing value.
nrow(DataAssginment1)
summary(DataAssginment1)
summary(na.omit(DataAssginment1))

mean(na.omit(DataAssginment1$Visits))
median(na.omit(DataAssginment1$Revenue))
max(na.omit(DataAssginment1$Transactions))
min(na.omit(DataAssginment1$Transactions))
tail(names(sort(table((na.omit(DataAssginment1$OS))))), 1)

#3. Create a scatterplot of number of visits (x-axis) versus revenue (y-axis). Comment on the correlation between the two variables.
plot(x = DataAssginment1$Visits,y = DataAssginment1$Revenue,main = 'scatter plot',xlab = 'visits',ylab = 'revenue')
pairs(DataAssginment1)
visits.rev <- DataAssginment1[,-2:-3]
View(visits.rev)
cor(visits.rev)

  #The correlation between two variables is not evidently seen when plotted and used with pairs function. When we use the cor function we can see that both the variables have a positive correlation.

#4. Which columns have missing data? How did you recognize them? How would you impute missing values?
colnames(DataAssginment1)[colSums(is.na(DataAssginment1))> 0]

  #We can impute mean, median, mode,0's or majority class depending on the nature of the problem. Best is to use human judgement to make decision.

#5. Impute missing transaction and gender values.
if(DataAssginment1$Revenue != 0){
  DataAssginment1$Transactions[is.na(DataAssginment1$Transactions)] <- median(DataAssginment1$Transactions, na.rm = TRUE)
}
elseif(DataAssginment1$Revenue == 0)
{
  DataAssginment1$Transactions[is.na(DataAssginment1$Transactions)] = 0
}

  # We have imputeted missing transaction values with median of the transaction value. We have added a if else loop to impute a trnasaction equal to 0 when Revenue is 0 else the transaction is imputed with median of the transaction value which is 1
DataAssginment1$Gender[is.na(DataAssginment1$Gender)] <- Male
  # We can impute missing Gender values with unknown, Male or female values. We see that the ratio of femlae : male is 1:14 using summary() function.
  # Hence we can use male to impute missing gender values.

#6. Split the data set into two equally sized data sets where one can be used for training a model and the other for validation. Take every odd numbered case and add them to the training data set and every even numbered case and add them to the validation data set, i.e., row 1, 3, 5, 7, etc. are training data while rows 2, 4, 6, etc. are validation data.
training <- subset(DataAssginment1, as.numeric(rownames(DataAssginment1)) %% 2 == 0) 
validation <- subset(DataAssginment1, as.numeric(rownames(DataAssginment1)) %% 2 != 0) 

#7. Calculate the mean revenue for the training and the validation data sets and compare them. Comment on the difference.
mean(train$Revenue,na.rm = TRUE)       
mean(validation$Revenue,na.rm = TRUE)
mean(na.omit(DataAssginment1$Revenue))

  #We see that the mean of the training and validation data sets is almost consistent with the complete data set.We see a difference of 5 points approx.

#8. For many data mining and machine learning tasks, there are packages in R. Find at least one package that has functions for creating training and validation data subsets and show how to use them.
library(caTools)
DataAssginment1$var = sample.split(iris,SplitRatio=0.7)
training=subset(DataAssginment1, DataAssginment1$var==TRUE)
validation=subset(DataAssginment1, DataAssginment1$var==FALSE) 


