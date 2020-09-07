##### Data Preprocessing (encoding, collapsing, and merging) #####
rm(list=ls())
setwd('D:/Data Analytics/Project/Data')
encounter=read.csv('encounters.csv')
medication=read.csv('medications.csv')
medication=medication[,-4]

library(dplyr)
medication_count=medication %>% 
  group_by(ENCOUNTER_ID, GENERIC_NAME) %>% 
  summarise(n = n())
rm(medication)

library(caret)
dmy=dummyVars(" ~ .", data=medication_count)
medication_hot=data.frame(predict(dmy, newdata=medication_count))
for(i in names(medication_hot)[2:815]){
  medication_hot[[paste(i)]]=(medication_hot[[i]]*medication_hot$n)
}
medication_hot=medication_hot[,-816]
rm(medication_count, dmy)

medication_hot=medication_hot %>% 
  group_by(ENCOUNTER_ID) %>% 
  summarise_all(funs(sum))
Total_1=inner_join(x=encounter, y=medication_hot, by='ENCOUNTER_ID')
rm(medication_hot)

procedure=read.csv('procedures.csv')
procedure=procedure[,-3]
procedure_count=procedure %>%
  group_by(ENCOUNTER_ID, PROCEDURE_ID) %>%
  summarise(n=n())
rm(procedure)

procedure_count$PROCEDURE_ID=as.factor(procedure_count$PROCEDURE_ID)
dmy=dummyVars(" ~ .", data=procedure_count)
procedure_hot=data.frame(predict(dmy, newdata=procedure_count))
for(i in names(procedure_hot)[2:675]){
  procedure_hot[[paste(i)]]=(procedure_hot[[i]]*procedure_hot$n)
}
procedure_hot=procedure_hot[,-676]
rm(procedure_count, dmy)

procedure_hot=procedure_hot %>% 
  group_by(ENCOUNTER_ID) %>% 
  summarise_all(funs(sum))
Total_2=inner_join(x=Total_1, y=procedure_hot, by='ENCOUNTER_ID')
rm(Total_1, procedure_hot)

lab=read.csv('labs.csv')
lab=lab[,-4]
lab_aggregate=lab %>%
  group_by(ENCOUNTER_ID, DETAIL_LAB_PROCEDURE_ID) %>%
  summarise(mean=mean(NUMERIC_RESULT), max=max(NUMERIC_RESULT), min=min(NUMERIC_RESULT))
rm(lab, encounter)

lab_aggregate$DETAIL_LAB_PROCEDURE_ID=as.factor(lab_aggregate$DETAIL_LAB_PROCEDURE_ID)
dmy=dummyVars(" ~ .", data=lab_aggregate)
lab_hot=data.frame(predict(dmy, newdata=lab_aggregate))
for(i in names(lab_hot)[2:789]){
  lab_hot[[paste0("mean_", i)]]=(lab_hot[[i]]*lab_hot$mean)
}
for(i in names(lab_hot)[2:789]){
  lab_hot[[paste0("max_", i)]]=(lab_hot[[i]]*lab_hot$max)
}
for(i in names(lab_hot)[2:789]){
  lab_hot[[paste0("min_", i)]]=(lab_hot[[i]]*lab_hot$min)
}
lab_hot=lab_hot[,-c(2:792)]
rm(lab_aggregate, dmy)

lab_hot=lab_hot %>% 
  group_by(ENCOUNTER_ID) %>% 
  summarise_all(funs(sum))
Total=inner_join(x=Total_2, y=lab_hot, by='ENCOUNTER_ID')
rm(Total_2, lab_hot)

Total=na.omit(Total)
Total=Total[,-1]
names(Total)[1:10]
Total=Total[,-c(2284:3860)] #removing mins maxs due to collinearity with means
Total$Mortality=as.factor(Total$Mortality)

##### Testing data setup #####
encounter=read.csv('encounters500.csv')
medication=read.csv('medications500.csv')
medication=medication[,-4]

medication_count=medication %>% 
  group_by(NEW_ENCOUNTER_ID, GENERIC_NAME) %>% 
  summarise(n = n())
rm(medication)

dmy=dummyVars(" ~ .", data=medication_count)
medication_hot=data.frame(predict(dmy, newdata=medication_count))
for(i in names(medication_hot)[2:624]){
  medication_hot[[paste(i)]]=(medication_hot[[i]]*medication_hot$n)
}
medication_hot=medication_hot[,-625]
rm(medication_count, dmy)

medication_hot=medication_hot %>% 
  group_by(NEW_ENCOUNTER_ID) %>% 
  summarise_all(funs(sum))
Total_1=inner_join(x=encounter, y=medication_hot, by='NEW_ENCOUNTER_ID')
rm(medication_hot)

procedure=read.csv('procedures500.csv')
procedure=procedure[,-3]
procedure_count=procedure %>%
  group_by(NEW_ENCOUNTER_ID, PROCEDURE_ID) %>%
  summarise(n=n())
rm(procedure)

procedure_count$PROCEDURE_ID=as.factor(procedure_count$PROCEDURE_ID)
dmy=dummyVars(" ~ .", data=procedure_count)
procedure_hot=data.frame(predict(dmy, newdata=procedure_count))
for(i in names(procedure_hot)[2:366]){
  procedure_hot[[paste(i)]]=(procedure_hot[[i]]*procedure_hot$n)
}
procedure_hot=procedure_hot[,-367]
rm(procedure_count, dmy)

procedure_hot=procedure_hot %>% 
  group_by(NEW_ENCOUNTER_ID) %>% 
  summarise_all(funs(sum))
Total_2=inner_join(x=Total_1, y=procedure_hot, by='NEW_ENCOUNTER_ID')
rm(Total_1, procedure_hot)

lab=read.csv('labs500.csv')
lab=lab[,-4]
lab_aggregate=lab %>%
  group_by(NEW_ENCOUNTER_ID, DETAIL_LAB_PROCEDURE_ID) %>%
  summarise(mean=mean(NUMERIC_RESULT), max=max(NUMERIC_RESULT), min=min(NUMERIC_RESULT))
rm(lab, encounter)

lab_aggregate$DETAIL_LAB_PROCEDURE_ID=as.factor(lab_aggregate$DETAIL_LAB_PROCEDURE_ID)
dmy=dummyVars(" ~ .", data=lab_aggregate)
lab_hot=data.frame(predict(dmy, newdata=lab_aggregate))
for(i in names(lab_hot)[2:644]){
  lab_hot[[paste0("mean_", i)]]=(lab_hot[[i]]*lab_hot$mean)
}
for(i in names(lab_hot)[2:644]){
  lab_hot[[paste0("max_", i)]]=(lab_hot[[i]]*lab_hot$max)
}
for(i in names(lab_hot)[2:644]){
  lab_hot[[paste0("min_", i)]]=(lab_hot[[i]]*lab_hot$min)
}
lab_hot=lab_hot[,-c(2:647)]
rm(lab_aggregate, dmy)

lab_hot=lab_hot %>% 
  group_by(NEW_ENCOUNTER_ID) %>% 
  summarise_all(funs(sum))
Total_test=inner_join(x=Total_2, y=lab_hot, by='NEW_ENCOUNTER_ID')
rm(Total_2, lab_hot)

Total_test=na.omit(Total_test)
names(Total_test)[1:10]
Total_test=Total_test[,-c(1638:2923)] #removing mins maxs due to collinearity with means

for (i in names(Total)[!(names(Total) %in% names(Total_test))]){
  Total_test[[i]]=0
}

##### Maching learning models #####
attach(Total)
library(boot)
library(glmnet)
library(tree)
library(randomForest)
library(gbm)
library(caret)
library(e1071)
set.seed(1)

#####(1-1) Lasso for variable selection
x=model.matrix(Mortality~.-ENCOUNTER_ID, data=Total)
y=Total$Mortality
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x, y, alpha=1, lambda=grid, family='binomial')
cv.lasso.out=cv.glmnet(x, y, alpha=1, family='binomial', type.measure="class")
plot(cv.lasso.out)
best.lam=cv.lasso.out$lambda.min
best.lam
##CV error
cv.lasso.error=cv.lasso.out$cvm[cv.lasso.out$lambda==cv.lasso.out$lambda.min]
cv.lasso.error
lasso.coef=predict(lasso.mod, type='coefficients', s=best.lam)
lasso.coef
lasso.nzcoef=lasso.coef[lasso.coef[,1]!=0,]
lasso.nzcoef
length(lasso.nzcoef)
form1=paste0(names(lasso.nzcoef)[-(1:3)],collapse='+')
form=paste0("Mortality~First_VASPRESOR+MARITAL_STATUS+",form1)

#####(1-2) 2nd Lasso for smaller selection
#x=model.matrix(as.formula(form), data=Total)
#lasso.mod=glmnet(x, y, alpha=1, lambda=grid, family='binomial')
#cv.lasso.out=cv.glmnet(x, y, alpha=1, family='binomial')
#best.lam=cv.lasso.out$lambda.min
#best.lam
#cv.lasso.error=cv.lasso.out$cvm[cv.lasso.out$lambda==cv.lasso.out$lambda.min]
#cv.lasso.error
#lasso.coef=predict(lasso.mod, type='coefficients', s=best.lam)
#lasso.coef
#lasso.nzcoef=lasso.coef[lasso.coef[,1]!=0,]
#lasso.nzcoef
#length(lasso.nzcoef)
#form2=paste0(names(lasso.nzcoef)[-(1:3)],collapse='+')
#form=paste0("Mortality~First_VASPRESOR+MARITAL_STATUS+",form2)

#####(2) Logistic regression with form
glm.fit=glm(formula=as.formula(form), data=Total, family=binomial)
summary(glm.fit)
summary(glm.fit)$coef
##training error rate
glm.probs=predict(glm.fit, type='response')
glm.pred=rep(0,length(Total$Mortality))
glm.pred[glm.probs>0.5]=1
table(glm.pred,Total$Mortality)
mean(glm.pred!=Total$Mortality)
##CV error rate
cv.glm.error=cv.glm(Total, glm.fit, K=10)
cv.glm.error$delta[1]
##test data prediction
glm.test.probs=predict(glm.fit, newdata=Total_test, type='response')
glm.test.pred=rep(0,length(Total_test$NEW_ENCOUNTER_ID))
glm.test.pred[glm.test.probs>0.5]=1
sum(glm.test.pred)

#####(3-1) LDA with form
library(MASS)
lda.fit=lda(as.formula(form), data=Total)
lda.fit
##training error
lda.pred=predict(lda.fit)
table(lda.pred$class, Total$Mortality)
mean(lda.pred$class!=Total$Mortality)
##LOOCV error
cv.lda=lda(as.formula(form), data=Total, CV=TRUE)
table(cv.lda$class,Total$Mortality)
mean(cv.lda$class!=Total$Mortality)

#####(4) Ridge with form
x=model.matrix(as.formula(form), data=Total)
ridge.mod=glmnet(x, y, alpha=0, lambda=grid, family='binomial')
cv.ridge.out=cv.glmnet(x, y, alpha=0, lambda=grid, family='binomial', type.measure="class")
best.lam=cv.ridge.out$lambda.min
best.lam
##CV error
ridge.coef=predict(ridge.mod, type='coefficients', s=best.lam)
ridge.coef
cv.ridge.error=cv.ridge.out$cvm[cv.ridge.out$lambda==cv.ridge.out$lambda.min]
cv.ridge.error
##training error
ridge.pred=rep(0,length(Total$Mortality))
ridge.pred[predict(ridge.mod, type='response', newx=x, s=best.lam)>0.5]=1
table(ridge.pred,Total$Mortality)
mean(ridge.pred!=Total$Mortality)
##test data prediction
x.test=model.matrix(as.formula(form), data=Total_test)
ridge.test.pred=rep(0,length(Total_test$NEW_ENCOUNTER_ID))
ridge.test.pred[predict(ridge.mod, type='response', newx=x.test, s=best.lam)>0.5]=1
sum(ridge.test.pred)
test.data=data.frame(NEW_ENCOUNTER_ID=Total_test$NEW_ENCOUNTER_ID, Pred_Mortality=ridge.test.pred)
write.csv(test.data, file='Predicted Mortality.csv')

#####(5-1) Random forest with form
cv.rf.error=matrix(NA, 5, 4, dimnames=list(paste(seq(10,100,20)),paste(c(50,100,200,500))))
k=0
for (i in seq(10,100,20)){
  k=k+1
  l=1
  for (j in c(50,100,200,500)){
    rf.fit=randomForest(as.formula(form), data=Total, mtry=i, ntree=j, importance=TRUE)
    cv.rf.error[k,l]=tail(rf.fit$err.rate[,1],n=1)
    l=l+1
  }
}
##CV error
cv.rf.error
rf.fit=randomForest(as.formula(form), data=Total, mtry=90, ntree=500, importance=TRUE)
tail(rf.fit$err.rate[,1],n=1)
#varImpPlot(rf.fit)
var.imp=importance(rf.fit)
var_importance=data_frame(variable=rownames(var.imp), importance=var.imp[,'MeanDecreaseGini'])
var_importance=arrange(var_importance, desc(importance))
var_importance$variable=factor(var_importance$variable, levels=var_importance$variable)
ggplot(var_importance[1:20,], aes(x=importance, y=variable))+
  geom_point(colour="blue",size=2)+scale_y_discrete(limits = unique(rev(var_importance$variable[1:20])))
##training error
rf.pred=predict(rf.fit, newdata=Total)
table(rf.pred,Total$Mortality)
mean(rf.pred!=Total$Mortality)

#####(5-2) Boosting with form
fitControl=trainControl(method="repeatedcv", number=10, repeats=1)
gbmGrid=expand.grid(n.trees=seq(2,10,2)*100, interaction.depth=c(1, 2, 4), 
                    shrinkage=c(0.001, 0.005, 0.1, 0.2, 1), n.minobsinnode=20)

gbm.form=train(as.formula(form), data=Total, method="gbm", distribution="bernoulli", 
               trControl=fitControl, verbose=FALSE, tuneGrid=gbmGrid)
plot(gbm.form)
##CV error
gbm.form$results[which.max(gbm.form$results$Accuracy),]
##Variance importance plot
var.imp=varImp(gbm.form)
plot(var.imp, top=20)

##training error
gbm.pred=predict(gbm.form, newdata=Total, n.trees=600)
table(gbm.pred,Total$Mortality)
mean(gbm.pred!=Total$Mortality)

#####(6) SVM with form
svmGrid=expand.grid(C=c(0.01, 0.1, 0.5, 1, 10, 100))
svm.Linear=train(as.formula(form), data=Total, method="svmLinear", trControl=fitControl, tuneGrid=svmGrid)
plot(svm.Linear)
##CV error
svm.Linear$results[which.max(svm.Linear$results$Accuracy),]
svm.Linear$finalModel
##training error
svm.Linear.pred=predict(svm.Linear, newdata=Total)
table(svm.Linear.pred,Total$Mortality)
mean(svm.Linear.pred!=Total$Mortality)

svmGrid=expand.grid(sigma=c(0.01, 0.1, 0.5, 1, 10), C=c(0.01, 0.1, 0.5, 1, 10))
svm.Radial=train(as.formula(form), data=Total, method="svmRadial", trControl=fitControl, tuneGrid=svmGrid)
plot(svm.Radial)
##CV error
svm.Radial$results[which.max(svm.Radial$results$Accuracy),]
svm.Radial$finalModel
##training error
svm.Radial.pred=predict(svm.Radial, newdata=Total)
table(svm.Radial.pred,Total$Mortality)
mean(svm.Radial.pred!=Total$Mortality)
