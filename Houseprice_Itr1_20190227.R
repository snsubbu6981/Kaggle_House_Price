train.data = read.csv("C:/Users/snarayanaswamy/Downloads/train.csv", header=TRUE)
test.data = read.csv("C:/Users/snarayanaswamy/Downloads/test.csv", header=TRUE)

variable.names(train.data)
variable.names(test.data)

## OLS Linear Regression
fit.lm.train1 = lm(SalePrice ~ (X1stFlrSF + X2ndFlrSF), data = train.data, na.action=na.exclude) #Itr1
fit.lm.train1 = lm(SalePrice ~ ., data = train.data, na.action=na.exclude) #Itr2
summary(fit.lm.train)
coef = coef(fit.lm.train)
coef

##predicting y based on fit.lm.train
predicted1 = predict(fit.lm.train, data.frame(test.data), type="response")

dat2 = cbind.data.frame(test.data$Id,predicted1)
rownames(dat2) = NULL ## Removing default row names created from predicted1
variable.names(dat2)
colnames(dat2)[1] = "Id"
colnames(dat2)[2] = "SalePrice"

variable.names(dat2)
write.csv(dat2,"C:/Users/snarayanaswamy/Downloads/submission.csv")

install.packages("ggplot2")
library(ggplot2)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +0
  geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
  
}

ggplotRegression(lm(SalePrice ~ (X1stFlrSF + X2ndFlrSF), data = train.data))
