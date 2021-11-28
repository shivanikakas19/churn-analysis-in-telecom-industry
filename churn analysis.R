getwd()
setwd("C:/Users/Shiva/Rprog")


library(plyr)

library(ggplot2)

library(lattice)

library(caret)

library(MASS)

library(party)

library(RColorBrewer)

library(ROCR)

install.packages("ROCR")
library(rpart)

install.packages("rattle")
library(rattle)

library(rpart.plot)

df <-read.csv("C:/Users/Shiva/Rprog/churn.csv") 

head(df)
str(df)

colSums(is.na(df))
#dataset contain no NA values.

length(df$customerID)

## Vizualization ##
#Number of Female and Male
count(df$gender)

#Gender vs Churn

plot(table(df$Churn, df$gender), col=c("red","green"),
     xlab = "churned Yes or No", ylab = "Gender", main = "Gender vs Churn")

#Number of customers having partner
table(df$Partner)

#Partner vs Churn


plot(df$Churn, df$Partner, col=c("red","green"),
     xlab = "Churned Yes or No", ylab = "Partner", main = "Partner vs Churn")

#Summary of tenure
summary(df$tenure)

#Histogram of tenure
hist(df$tenure, col = "purple", xlab = "Tenure of the connection", main = "Distribution of tenure")

 #Maximum tenure of connection is between 0 to 10 months.

table(df$CallService)
plot(table(df$Churn, df$CallService), xlab= "churned Yes or no",
     ylab = "Call service Yes or No", col = c("orange", "yellow"), main = "CallService vs Churn")

#Multipleconnection

table(df$MultipleConnections)
#MultipleConnection vs Churn

plot(table(df$MultipleConnections, df$Churn), col = c("red", "blue"), main = "MultipleConnection vs Churn")

#Type of InternetConnection

table(df$InternetConnection)

#InternetConnection vs Churn
plot(table(df$InternetConnection, df$Churn), 
     xlab = "Type of connection", ylab = "churned Yes or No", 
     main = "Internetconnection vs Churn", col = c("red", "green"))
 #People having fiber optic internetconnection has churned more.

#OnlineSecurity

table(df$OnlineSecurity)

#OnlineSecurity vs Churn

plot(table(df$OnlineSecurity, df$Churn), 
     xlab = "Availability of onine security",ylab = "Churned Yes or No",
     col = c("red","violet"), main = "OnlineSecurity vs Churn")
 #Customers having no online security has churned more.

#OnlineBackup

table(df$OnlineBackup)
#OnlineBackup vs Cjurn

plot(table(df$OnlineBackup, df$Churn), 
     xlab = "Online Backup",ylab = "Churned Yes or No",
     col = c("red","orange"), main = "OnlineBackup vs Churn")

#DeviceProtectionservice

table(df$DeviceProtectionService)
plot(table(df$DeviceProtectionService, df$Churn), 
     xlab = "Device Protection Service",ylab = "Churned Yes or No",
     col = c("red","green"), main = "DeviceProtectionService vs Churn")

#TechnicalHelp

table(df$TechnicalHelp)
#TechnicalHelp vs Churn

plot(table(df$TechnicalHelp, df$Churn),col = c("red","green"), main = "TechnicalHelp vs Churn")

#OnlineTv

table(df$OnlineTV)
#OnlineTv vs Churn

plot(table(df$OnlineTV, df$Churn),col = c("red","green"), main = "OnlineTV vs Churn")

#OnlineMovies

table(df$OnlineMovies)

#OnlineMovies vs Churn

plot(table(df$OnlineMovies, df$Churn),col = c("red","green"), main = "OnlineMovies vs Churn")

#Agreement

table(df$Agreement)
#Agreement vs Churn

plot(table(df$Agreement, df$Churn),col = c("red","green"), main ="Agreement vs Churn")

  #Customers subscribing two year plan has churned very less in comparison to month-to-month and one year.

#BillingMethod

table(df$BillingMethod
      #Billing vs Churn
      
plot(table(df$BillingMethod, df$Churn),col = c("red","green"), main = "Billing vs Churn")

#PaymentMethod

table(df$PaymentMethod)      
#PaymentMethod vs Churn

plot(table(df$PaymentMethod, df$Churn),col = c("red","green"), main = "PaymentMethod vs Churn")

#MonthlyServiceCharges

str(df$MonthlyServiceCharges)

summary(df$MonthlyServiceCharges)

#Histogram of MonthlyServiceCharges

hist(df$MonthlyServiceCharges,col = "blue", main = "Distribution of MonthlyServiceCharges",
     xlab="MonthlyServiceCharges")

#TotalAmount

str(df$TotalAmount)

summary(df$TotalAmount)

#Histogram of TotalAmount

hist(df$TotalAmount,col = "orange", xlab = "Bill Amount", ylim = c(0,5000), main = "Distribution of TotalAmount")

#customers who churned

table(df$Churn)
#Making a list of those columns.

cols_name <- c(10:15) 

#Changing "No internet Srvice" to "No" for those columns

for (i in 1:ncol(df[,cols_name])){
  df[,cols_name][,i]<- as.factor(mapvalues(df[,cols_name][,i],from = c("No internet service"), to = c("No")))}

str(df)

table(df$OnlineBackup)

#Changing "No phone service" to "No" in MultipleConnections Column
df$MultipleConnections <- as.factor(mapvalues(df$MultipleConnections,from=c("No phone service"),to = c("No")))

table(df$SeniorCitizen)

#Removing columns which are not very significant for our analysis

df$SeniorCitizen <- NULL

df$customerID <- NULL

df$gender <- NULL

## Statistical modeling
    #Decision tree analysis
install.packages("caTools")
library(caTools)

spl <- sample.split(df$Churn, SplitRatio = .70)

train <- df[spl==T,]

test <- df[spl==F,]

#Dimension of training and testing datasets.

noquote("Dimension of training dataset:"); dim(train)

noquote("Dimension of testing dataset:"); dim(test)

#full grown tree model
dev.off()
full_model_tree <- rpart(Churn~., train, method = "class",minsplit=0,cp=0)
#plotting model

plot(full_model_tree)

prp(full_model_tree)

#Prediction using testing dataset 

full_model_pred <- predict(full_model_tree, newdata = test, type = "class")
#Predictions

full_model_pred

## Accuracy and Confusion Matrix
#confusion matrix

conf_matrix_full_model <- table(test$Churn, full_model_pred)

conf_matrix_full_model
#Print out the accuracy

sum(diag(conf_matrix_full_model))/sum(conf_matrix_full_model)*100

### cp implementation
#"cp" stand for complexity parametr is used to control the size of the decision tree and to select the optimal tree size.

#Smaller cp value will lead to bigger trees/complexity increases

#Higher cp values will lead to smaller trees

library(caret)
library(e1071)
numfolds <- trainControl(method = "cv", number = 100)

#trainControl control parametrs for train
cpGrid <- expand.grid(cp=seq(0.01,.05,0.01))

cpGrid

### Checking the cross validation accuracy for cp parameters.

train(Churn~., data = train, method = "rpart", trControl = numfolds, tuneGrid = cpGrid)

## Pruning the full_model_tree(fully grown model)

pruned_tree <- prune(full_model_tree, cp = 0.01)
prp(pruned_tree)

printcp(pruned_tree)

# prediction using pruned tree
pruned_model_pred <- predict(pruned_tree, newdata = test, type = "class")

#predictions
pruned_model_pred

#Confusion Matrix
conf_matrix_pruned_model <- table(test$Churn,pruned_model_pred)
conf_matrix_pruned_model

#Print out the Accuracy
sum(diag(conf_matrix_pruned_model))/ sum(conf_matrix_pruned_model)

## Another tree by ctree2 technique called classification tree

model <- train(Churn~., data= train, method = "ctree2",
               trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(maxdepth = 10, mincriterion = 0.95))
plot(model$finalModel)

#Prediction using the fully grown tree
model_pred <- predict(model, newdata = test, type = "raw") 

model_pred
#Confusion Matrix
conf_matrix_model <- table(test$Churn, model_pred)
conf_matrix_model

#Print out the accuracy
sum(diag(conf_matrix_model))/ sum(conf_matrix_model)
