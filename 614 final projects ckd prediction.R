#install.packages('randomForest')
#install.packages('missForest')
#install.packages('glmnet')
#install.packages('caret')
#install.packages('tidyverse')
#install.packages('randomForest')
#install.packages('ROCR')
#install.packages('corrplot')
#install.packages('MASS')
#install.packages('car')
library(MASS)
library(car)
library(corrplot)
library(caret)
library(missForest)
library(randomForest)
library(glmnet)
library(tidyverse)
library(randomForest)
library(ROCR)
data = read.csv('C:/data/kidney_imputed.csv',header=T)
data=data[,-1]
data$kidney.rbc[data$kidney.rbc==""]=NA
data$kidney.pc[data$kidney.pc==""]=NA
data$classification[data$classification=="ckd\t"]="ckd"
data$classification=relevel(data$classification,ref='notckd')
#all cate to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                       as.factor)

#rf imputation
ran_miss = missForest(data[,-21])

data_new = ran_miss$ximp

data_new = cbind(data_new,data['classification'])
#replace all cates into binary
data_bin = data_new
data_bin['pcc']=ifelse(data_new['pcc']=='notpresent',0,1)
data_bin['ba']=ifelse(data_new['ba']=='notpresent',0,1)
data_bin['htn']=ifelse(data_new['htn']=='no',0,1)
data_bin['dm']=ifelse(data_new['dm']=='no',0,1)
data_bin['cad']=ifelse(data_new['cad']=='no',0,1)
data_bin['appet']=ifelse(data_new['appet']=='poor',0,1)
data_bin['pe']=ifelse(data_new['pe']=='no',0,1)
data_bin['ane']=ifelse(data_new['ane']=='no',0,1)
data_bin['kidney.rbc']=ifelse(data_new['kidney.rbc']=='abnormal',0,1)
data_bin['kidney.pc']=ifelse(data_new['kidney.pc']=='abnormal',0,1)
data_bin['classification']=ifelse(data_new['classification']=='notckd',0,1)
data_bin = data_bin[,-21]
#correlation mat
data.cor = cor(data_bin, method = "pearson")
corrplot(data.cor, order = "hclust", 
         tl.col = "black", tl.srt = 45)
#delete kid.pc, rc, pcv_,htn
data_bin_ex = data_bin[,-c(10,14,15,22,24)]
#corr mat after delete
data.cor_ex = cor(data_bin_ex, method = "pearson")
corrplot(data.cor_ex, order = "hclust", 
         tl.col = "black", tl.srt = 45)
#delete var 
#data_new = data_new[,-c(10,14,15,23,25)]



#train test split
set.seed(425)
training.samples <- data_new$classification %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- data_new[training.samples, ]
test.data <- data_new[-training.samples, ]
# Dumy code categorical predictor variables
x <- model.matrix(classification~., train.data)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train.data$classification == "ckd", 1, 0)
# Find the best lambda using cross-validation
set.seed(425) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
plot(cv.lasso)
summary(model)
# Make predictions on the test data
x.test <- model.matrix(classification ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "ckd", "notckd")
# Model accuracy
observed.classes <- test.data$classification
mean(predicted.classes == observed.classes)

#implement random forest
rf <- randomForest(classification~., data=train.data, importance=TRUE)
importance(rf) # How much accuracy reduces if delete one variable
pred_y = predict(rf,test.data)
#accuracy
mean(pred_y==observed.classes)
#ROC curve
pred_rf_prob = predict(rf,test.data,type='prob')
pred_lg_prob = 1-predict(model, s=0.01, newx=x.test, type="response")
pred <- prediction(pred_rf_prob[,2], test.data$classification)
pred1 = prediction(pred_lg_prob, test.data$classification)
perf1 <- performance(pred1,"tpr","fpr")
perf <- performance(pred,"tpr","fpr")
plot(perf,col = 'red',type='b',lty=2,lwd=1,pch=17)
par(new=TRUE)
plot(perf1,col = 'green',type='b',lty=2,lwd=1,pch=18)
legend("bottomright", legend = c("Random Forest", "Logistics Regression Lasso"),
       lwd = 3, col = c("red", "green"))

#Baseline: logistics with all variables
train.data$classification<-relevel(train.data$classification,ref='notckd')
#model_base <- glm(formula = classification~1,family = "binomial" ,data=train.data)
#model_full<-glm(formula = classification~.,family = "binomial" ,data=train.data)
#stepAIC(model_base,direction = 'forward',trace=0,scope=list(upper = model_full, lower = model_base))
model_AIC = glm(formula = classification~hemo,family = "binomial",data=train.data)
#y_base = predict(model_base,test.data,type = 'response')
#pred_class_base <- ifelse(y_base > 0.5, "ckd", "notckd")
y_AIC = predict(model_AIC,test.data,type = 'response')
pred_class_AIC <- ifelse(y_AIC > 0.5, "ckd", "notckd")
#mean(pred_class_base == test.data$classification)
mean(pred_class_AIC == test.data$classification)
#models with 'hemo' or 'pcv' as factors
train.data$classification<-relevel(train.data$classification,ref='ckd')
test.data$classification<-relevel(test.data$classification,ref='ckd')
model_he = glm(formula = classification~hemo,family = "binomial",data=train.data)
model_pc = glm(formula = classification~pcv_imputed.pcv,family = "binomial",data=train.data)


pred_he_prob = predict(model_he,test.data,type='response')
pred_pc_prob = predict(model_pc, test.data, type="response")
pred <- prediction(pred_he_prob, test.data$classification)
pred1 = prediction(pred_pc_prob, test.data$classification)
perf1 <- performance(pred1,"tpr","fpr")
perf <- performance(pred,"tpr","fpr")
plot(perf,col = 'red',type='b',lty=2,lwd=1,pch=17)
par(new=TRUE)
plot(perf1,col = 'green',type='b',lty=2,lwd=1,pch=18)
legend("bottomright", legend = c("LR_hemo,AUC=0.935", "LR_pcv,AUC=0.972"),
       lwd = 3, col = c("red", "green"))
auc1 <- performance(pred, measure = "auc")
auc2 <- performance(pred1, measure = "auc")
auc1@y.values
auc2@y.values
