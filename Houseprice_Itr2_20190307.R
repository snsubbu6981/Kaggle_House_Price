train.data = read.csv("C:/Users/snarayanaswamy/Downloads/train.csv", header=TRUE)
test.data = read.csv("C:/Users/snarayanaswamy/Downloads/test.csv", header=TRUE)
train.data.complete=read.csv("C:/Users/snarayanaswamy/Downloads/nomisschecks.csv", header=TRUE)

variable.names(train.data)
variable.names(test.data)
str(train.data.complete)
densityplot(train.data.tree$SalePrice) ##Understanding distribution of dependent variable
colSums(is.na(train.data.complete))

## Removing high missing variables
library(dplyr)
train.data.tree = select(train.data,-c(PoolQC,Fence,MiscFeature,FireplaceQu,LotFrontage,Alley)) 
## Imputing missing values with mean - Continuous variable
train.data.tree$GarageYrBlt[is.na(train.data.tree$GarageYrBlt)] <- mean(train.data.tree$GarageYrBlt, na.rm = TRUE)

## Computing total missing values in each column
colSums(is.na(train.data.tree)) 

## Impute missing values using MICE package - cart function works for all types of variables

install.packages("mice")
install.packages("backports")
install.packages("broom")

library(mice)
train.data.tree1 = mice(train.data.tree, m=1, method='cart',printFlag=FALSE)
train.data.tree.nomiss = complete(train.data.tree1,1)
write.csv(train.data.tree.nomiss,"C:/Users/snarayanaswamy/Downloads/nomisschecks.csv")
colSums(is.na(train.data.tree.nomiss))


################################ Algorithm 1 ####################################################
## OLS Linear Regression
fit.lm.train = lm(SalePrice ~ (X1stFlrSF + X2ndFlrSF), data = train.data, na.action=na.exclude) #Itr1
fit.lm.train1 = lm(SalePrice ~ ., data = train.data, na.action=na.exclude) #Itr2
summary(fit.lm.train)
coef(fit.lm.train)

##predicting saleprice on test.data based on model fit
predicted1 = predict(fit.lm.train, test.data, type="response")
predicted1

##RMSE.lm = sqrt(mean((predicted1-train.data$SalePrice)^2))

##Binding test data with predictions 
dat2 = cbind.data.frame(test.data$Id,predicted1)
rownames(dat2) = NULL ## Removing default row names created from predicted1
variable.names(dat2)
colnames(dat2)[1] = "Id"
colnames(dat2)[2] = "SalePrice"

variable.names(dat2)
write.csv(dat2,"C:/Users/snarayanaswamy/Downloads/submission.csv")

################################ Algorithm 2 ####################################################
## Simple Decision Tree
library(rpart)
library(rattle)
library(caret)

## When we grow a tree in R (or any other software, for that matter), it internally performs a 10-fold 
## cross-validation. What this means is that 10 cycles are run, and in each of them 90% of the training data 
## is used for building the model, and the remaining 10%, even though they're part of the training data, 
## are only used to predict and evaluate the results. In another words, these 10% are momentarily a test set. 
## It should be obvious that, after the 10 cycles, 10 different chunks of data are used to test the model, 
## which means every single observation is used, at some point, not only to train but also to test the model

fit.tree = train(SalePrice ~ (PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+SaleCondition+X1stFlrSF+X2ndFlrSF),
                method = "rpart",data=train.data.complete)
print(fit.tree$results)

predicted2 = predict(fit.tree, test.data)
predicted2

##Binding test data with predictions 
dat2 = cbind.data.frame(test.data$Id,predicted2)
rownames(dat2) = NULL ## Removing default row names created from predicted1
variable.names(dat2)
colnames(dat2)[1] = "Id"
colnames(dat2)[2] = "SalePrice"

variable.names(dat2)
write.csv(dat2,"C:/Users/snarayanaswamy/Downloads/submission.csv")


################################ Algorithm 3 ####################################################
## Random Forrest

library(randomForest)

rf = randomForest(SalePrice ~ (PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+SaleCondition+X1stFlrSF+X2ndFlrSF),
                  data=train.data.complete, importance=TRUE,ntree=1000)
which.min(rf$mse)

imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
