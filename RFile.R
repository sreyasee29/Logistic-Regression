

library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)

Path<- "D:/R assignments/Case Study 4"

setwd(Path)
getwd()

data<-read.csv("Data_for_Logistic_Regression.csv",header = TRUE)
data1=data

str(data1)
summary(data1)
dim(data1)

as.data.frame(rapply(data1,function(x)length(unique(x))))

data1$Default_On_Payment<-as.factor(data1$Default_On_Payment)

summary(data1)

cols_cat<-c("Status_Checking_Acc","Credit_History","Purposre_Credit_Taken","Savings_Acc","Years_At_Present_Employment","Marital_Status_Gender","Other_Debtors_Guarantors","Property","Other_Inst_Plans","Housing","Job","Telephone","Foreign_Worker")
data1[cols_cat]<-lapply(data1[cols_cat],factor)
summary(data1)

data.frame(colSums(is.na(data1)))

set.seed(123)
spl = sample.split(data1$Default_On_Payment, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)


model <- glm(Default_On_Payment~., data=data.train, family=binomial())
summary(model)

#Removing Customer_id and COunt
model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
             Credit_History + Purposre_Credit_Taken + Credit_Amount + 
             Savings_Acc + Years_At_Present_Employment + Inst_Rt_Income +
             Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs +
             Property + Age + Other_Inst_Plans + Housing + Num_CC + Job + Dependents+
             Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)


model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
               I(Credit_History== "A32") + I(Credit_History== "A33") + 
               I(Credit_History== "A34") + Purposre_Credit_Taken + Credit_Amount + 
               Savings_Acc + Years_At_Present_Employment + Inst_Rt_Income +
               Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs +
               Property + Age + Other_Inst_Plans + Housing + Num_CC + Job + Dependents+
               Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
               I(Credit_History== "A32") + I(Credit_History== "A33") + 
               I(Credit_History== "A34") + I(Purposre_Credit_Taken== "A41") + 
               I(Purposre_Credit_Taken== "A410") + I(Purposre_Credit_Taken== "A42") +
               I(Purposre_Credit_Taken== "A43") + I(Purposre_Credit_Taken== "A48") + 
               I(Purposre_Credit_Taken== "A49") + Credit_Amount + 
               Savings_Acc + Years_At_Present_Employment + Inst_Rt_Income +
               Marital_Status_Gender + Other_Debtors_Guarantors + Property + 
               Age + Other_Inst_Plans + Housing + Num_CC + Job + Dependents+
               Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
               I(Credit_History== "A32") + I(Credit_History== "A33") + 
               I(Credit_History== "A34") + I(Purposre_Credit_Taken== "A41") + 
               I(Purposre_Credit_Taken== "A410") + I(Purposre_Credit_Taken== "A42") +
               I(Purposre_Credit_Taken== "A43") + I(Purposre_Credit_Taken== "A48") + 
               I(Purposre_Credit_Taken== "A49") + Credit_Amount + 
               Savings_Acc + I(Years_At_Present_Employment == "A74") + Inst_Rt_Income +
               Marital_Status_Gender + Other_Debtors_Guarantors + Property + 
               Age + Other_Inst_Plans + Housing + Num_CC + Job + Dependents+
               Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
               I(Credit_History== "A32") + I(Credit_History== "A33") + 
               I(Credit_History== "A34") + I(Purposre_Credit_Taken== "A41") + 
               I(Purposre_Credit_Taken== "A410") + I(Purposre_Credit_Taken== "A42") +
               I(Purposre_Credit_Taken== "A43") + I(Purposre_Credit_Taken== "A48") + 
               I(Purposre_Credit_Taken== "A49") + Credit_Amount + 
               Savings_Acc + I(Years_At_Present_Employment == "A74") + Inst_Rt_Income +
               I(Marital_Status_Gender== "A93") + Other_Debtors_Guarantors + Property + 
               Age + I(Other_Inst_Plans== "A143") + Housing + Num_CC + Dependents+
               Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
               I(Credit_History== "A32") + I(Credit_History== "A33") + 
               I(Credit_History== "A34") + I(Purposre_Credit_Taken== "A41") + 
               I(Purposre_Credit_Taken== "A410") + I(Purposre_Credit_Taken== "A42") +
               I(Purposre_Credit_Taken== "A43") + I(Purposre_Credit_Taken== "A48") + 
               I(Purposre_Credit_Taken== "A49") + Credit_Amount + I(Savings_Acc== "A62") + 
               I(Savings_Acc== "A64") + I(Savings_Acc== "A65") + 
               I(Years_At_Present_Employment == "A74") + Inst_Rt_Income +
               I(Marital_Status_Gender== "A93") + Other_Debtors_Guarantors + Property + 
               Age + I(Other_Inst_Plans== "A143") + Housing + Num_CC +
               Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)

#-----------------------------------------Final Model---------------------------------------#


vif(model)

model <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + 
               I(Credit_History== "A33") + I(Credit_History== "A34") + 
               I(Purposre_Credit_Taken== "A41") + I(Purposre_Credit_Taken== "A410") + 
               I(Purposre_Credit_Taken== "A42") + I(Purposre_Credit_Taken== "A43") + 
               I(Purposre_Credit_Taken== "A48") + I(Purposre_Credit_Taken== "A49") + 
               Credit_Amount + I(Savings_Acc== "A62") + I(Savings_Acc== "A64") + 
               I(Savings_Acc== "A65") + I(Years_At_Present_Employment == "A74") + 
               Inst_Rt_Income + I(Marital_Status_Gender== "A93") + 
               Other_Debtors_Guarantors + Property + Age + I(Other_Inst_Plans== "A143") + 
               Housing + Num_CC + Telephone + Foreign_Worker, data=data.train, 
               family=binomial())
summary(model)


model <- glm(Default_On_Payment~ I(Status_Checking_Acc== "A13") + 
               I(Status_Checking_Acc== "A14") + Duration_in_Months + 
               I(Credit_History== "A33") + I(Credit_History== "A34") + 
               I(Purposre_Credit_Taken== "A41") + I(Purposre_Credit_Taken== "A410") + 
               I(Purposre_Credit_Taken== "A42") + I(Purposre_Credit_Taken== "A43") + 
               I(Purposre_Credit_Taken== "A48") + I(Purposre_Credit_Taken== "A49") + 
               Credit_Amount + I(Savings_Acc== "A62") + I(Savings_Acc== "A64") + 
               I(Savings_Acc== "A65") + I(Years_At_Present_Employment == "A74") + 
               Inst_Rt_Income + I(Marital_Status_Gender== "A93") + 
               Other_Debtors_Guarantors + Property + Age + I(Other_Inst_Plans== "A143") + 
               Housing + Num_CC + Telephone + Foreign_Worker, data=data.train, family=binomial())
summary(model)


vif(model)


wald.test(b=coef(model), Sigma= vcov(model), Terms=1:26)

modelChi <- model$null.deviance - model$deviance
modelChi

chidf <- model$df.null - model$df.residual
chidf

chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

1-pchisq(deviance(model), df.residual(model))

# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)

varImp(model)

predicted <- predict(model,newdata = data.train,type="response")

predicted

write.csv(predicted,"pred.csv")


rocCurve   <- roc(response = data.train$Default_On_Payment, predictor = factor(predicted, ordered =TRUE), 
                  levels = rev(levels(data.train$Default_On_Payment)))
data.train$Default_On_Payment <- as.factor(data.train$Default_On_Payment)


threshold<-as.numeric(coords(rocCurve,"best")[1])

predclass <-ifelse(predicted>threshold,1,0)

predclass

Confusion <- table(Predicted = predclass,Actual = data.train$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

Gini
Confusion
auc(rocCurve)
plot(rocCurve)


data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit 

logistic_data <- data.train

logistic_data$predicted.probabilities<-fitted(model)

logistic_data$standardized.residuals<-rstandard(model)
logistic_data$studentized.residuals<-rstudent(model)
logistic_data$dfbeta<-dfbeta(model)
logistic_data$dffit<-dffits(model)
logistic_data$leverage<-hatvalues(model)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, "Res.csv")


modeltest <- glm(Default_On_Payment~ I(Status_Checking_Acc== "A13") + 
               I(Status_Checking_Acc== "A14") + Duration_in_Months + 
               I(Credit_History== "A33") + I(Credit_History== "A34") + 
               I(Purposre_Credit_Taken== "A41") + I(Purposre_Credit_Taken== "A410") + 
               I(Purposre_Credit_Taken== "A42") + I(Purposre_Credit_Taken== "A43") + 
               I(Purposre_Credit_Taken== "A48") + I(Purposre_Credit_Taken== "A49") + 
               Credit_Amount + I(Savings_Acc== "A62") + I(Savings_Acc== "A64") + 
               I(Savings_Acc== "A65") + I(Years_At_Present_Employment == "A74") + 
               Inst_Rt_Income + I(Marital_Status_Gender== "A93") + 
               Other_Debtors_Guarantors + Property + Age + I(Other_Inst_Plans== "A143") + 
               Housing + Num_CC + Telephone + Foreign_Worker, data=data.test, family=binomial())
summary(modeltest)


modeltest <- glm(Default_On_Payment~ I(Status_Checking_Acc== "A13") + 
                   I(Status_Checking_Acc== "A14") + Duration_in_Months + 
                   I(Credit_History== "A33") + I(Credit_History== "A34") + 
                   I(Purposre_Credit_Taken== "A41") + I(Purposre_Credit_Taken== "A410") + 
                   I(Purposre_Credit_Taken== "A42") + I(Purposre_Credit_Taken== "A43") + 
                   I(Purposre_Credit_Taken== "A49") + 
                   Credit_Amount + I(Savings_Acc== "A62") + I(Savings_Acc== "A64") + 
                   I(Savings_Acc== "A65") + I(Years_At_Present_Employment == "A74") + 
                   Inst_Rt_Income + I(Marital_Status_Gender== "A93") + 
                   Other_Debtors_Guarantors + Property + Age + I(Other_Inst_Plans== "A143") + 
                   Housing + Num_CC + Telephone + Foreign_Worker, data=data.test, family=binomial())
summary(modeltest)

modeltest <- glm(Default_On_Payment~ I(Status_Checking_Acc== "A13") + 
                   I(Status_Checking_Acc== "A14") + Duration_in_Months + 
                   I(Credit_History== "A33") + I(Credit_History== "A34") + 
                   I(Purposre_Credit_Taken== "A41") + I(Purposre_Credit_Taken== "A410") + 
                   I(Purposre_Credit_Taken== "A42") + I(Purposre_Credit_Taken== "A43") + 
                   I(Purposre_Credit_Taken== "A49") + 
                   Credit_Amount + I(Savings_Acc== "A64") + 
                   I(Savings_Acc== "A65") + I(Years_At_Present_Employment == "A74") + 
                   Inst_Rt_Income + I(Marital_Status_Gender== "A93") + 
                   I(Other_Debtors_Guarantors=="A103") + Property + Age + I(Other_Inst_Plans== "A143") + 
                   Housing + Num_CC + Telephone + Foreign_Worker, data=data.test, family=binomial())
summary(modeltest)

modeltest <- glm(Default_On_Payment~ I(Status_Checking_Acc== "A13") + 
                   I(Status_Checking_Acc== "A14") + Duration_in_Months + 
                    I(Credit_History== "A34") + 
                   I(Purposre_Credit_Taken== "A41") + I(Purposre_Credit_Taken== "A410") + 
                   I(Purposre_Credit_Taken== "A42") + I(Purposre_Credit_Taken== "A43") + 
                   I(Purposre_Credit_Taken== "A49") + 
                   Credit_Amount + I(Savings_Acc== "A64") + 
                   I(Savings_Acc== "A65") + I(Years_At_Present_Employment == "A74") + 
                   Inst_Rt_Income + I(Marital_Status_Gender== "A93") + 
                   I(Other_Debtors_Guarantors=="A103") + Age + I(Other_Inst_Plans== "A143") + 
                   I(Housing=="A152") + Telephone , data=data.test, family=binomial())
summary(modeltest)


vif(modeltest)


library(car)
library(mlogit)


modelChi <- modeltest$null.deviance - modeltest$deviance
modelChi

chidf <- modeltest$df.null - modeltest$df.residual
chidf


chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

R2.hl<-modelChi/modeltest$null.deviance
R2.hl

R.cs <- 1 - exp ((modeltest$deviance - modeltest$null.deviance) /1500)
R.cs

R.n <- R.cs /(1-(exp(-(modeltest$null.deviance/1500))))
R.n


residuals(modeltest) # deviance residuals
residuals(modeltest, "pearson") # pearson residuals

sum(residuals(modeltest, type = "pearson")^2)
deviance(modeltest)


1-pchisq(deviance(modeltest), df.residual(modeltest))


hosmerlem <- function (y, yhat, g = 30) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

hosmerlem(y = data.test$Default_On_Payment, yhat = fitted(modeltest))
library(ResourceSelection)
hl <- hoslem.test(data.test$Default_On_Payment, fitted(modeltest), g=30)
hl


modeltest$coefficients

exp(modeltest$coefficients)


prediction <- predict(modeltest,newdata = data.test,type="response")
prediction

rocCurve   <- roc(response = data.test$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data.test$Default_On_Payment)))
data.test$Default_On_Payment <- as.factor(data.test$Default_On_Payment)

thresholdtest <-as.numeric(coords(rocCurve,"best")[1])

predictclass=ifelse(prediction>thresholdtest,1,0)
predictclass
predicttable=table (Predicted=predictclass, Actual = data.test$Default_On_Payment)
AccuracyRate=sum(diag(predicttable))/sum(predicttable)
Gini=2*auc(rocCurve)-1
AUCmetric=data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),
                       AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric=data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric)=NULL
names(AUCmetric)=c("Metric","Values")
AUCmetric
predicttable
plot(roccurve)

data.test$m1.yhat <- predict(modeltest, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit

logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(modeltest)
logistic_data$standardized.residuals<-rstandard(modeltest)
logistic_data$studentized.residuals<-rstudent(modeltest)
logistic_data$dfbeta<-dfbeta(modeltest)
logistic_data$dffit<-dffits(modeltest)
logistic_data$leverage<-hatvalues(modeltest)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, file = "D:/R assignments/Case Study 4/logistic_pred.csv")
