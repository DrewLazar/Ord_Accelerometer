train_RW=read.table("AL.RW.2A_corr.txt", header = T)
test_RW=read.table("AL.RW.2B.txt", header = T)


# principal Component

library(pls)
set.seed(7757)

# train data
pcr_RW=prcomp(train_RW[,-c(25:29)], center = TRUE, scale. = TRUE)

attributes(pcr_RW)

summary(pcr_RW)

train_RW_scale=scale(train_RW[,-c(25:29)],center = TRUE, scale = TRUE)

tr=predict(pcr_RW, train_RW_scale)
train=data.frame(tr,train_RW[,c(25:29)])


# test data

test_RW_scale=scale(test_RW[,-c(25:29)],center = TRUE, scale = TRUE)

te=predict(pcr_RW, test_RW_scale)
test=data.frame(te,test_RW[,c(25:29)])

# write the PCA data

write.csv(train, "C:/Users/buzzlightyear/Desktop/RW/AL.RW.2A.PCA.csv", row.names = FALSE)
write.csv(test, "C:/Users/buzzlightyear/Desktop/RW/AL.RW.2B.PCA.csv", row.names = FALSE)


# Converting ordered response 

train$COS.Intensity=ordered(train$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test$COS.Intensity=ordered(test$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))


# proportional odds cumulative logit model

library(MASS)
model.odds_RW=polr(COS.Intensity~.-PC21-PC22-PC23-PC24-PC25-PC26-PC27, data = train , Hess = TRUE, method = "logistic")
summary(model.odds_RW)


pred.odds_RW=predict(model.odds_RW, test[,-c(29)])
tab.odds_RW=table(pred.odds_RW,test[,29])
tab.odds_RW
mcer.odds_RW=1-sum(diag(tab.odds_RW))/sum(tab.odds_RW)
mcer.odds_RW


#### RANDOM FOREST #### 

library(randomForest)

set.seed(7757)
random.tree_RW=randomForest(COS.Intensity~.-PC21-PC22-PC23-PC24-PC25-PC26-PC27, data = train, mtry = 5, importance = TRUE)
random.tree_RW
importance(random.tree_RW)
varImpPlot(random.tree_RW)
pred.random_RW=predict(random.tree_RW, newdata = test[,-c(29)])
table_RW=table(pred.random_RW,test[,29])
table_RW
mcer.random_RW=1-(sum(diag(table_RW))/sum(table_RW))
mcer.random_RW



### Axis wise Principal component ####

### For x Axis ###

pcr_RW_X=prcomp(train_RW[,c(1:7)], center = TRUE, scale. = TRUE)

attributes(pcr_RW_X)
summary(pcr_RW_X)

# PC1 PC2 PC3 PC4 explains more than 95% of the variation

train_RW_X=scale(train_RW[,c(1:7)], center = TRUE, scale = TRUE)
tr_X=predict(pcr_RW_X, train_RW_X)

### For Y Axis ###

pcr_RW_Y=prcomp(train_RW[,c(8:14)], center = TRUE, scale. = TRUE)

attributes(pcr_RW_Y)
summary(pcr_RW_Y)

train_RW_Y=scale(train_RW[,c(8:14)], center = TRUE, scale = TRUE)
tr_Y=predict(pcr_RW_Y, train_RW_Y)

# PC1 PC2 PC3 PC4 explains more than 95 % of the variation

### For Z Axis ###

pcr_RW_Z=prcomp(train_RW[,c(15:21)], center = TRUE, scale. = TRUE)

attributes(pcr_RW_Z)
summary(pcr_RW_Z)

train_RW_Z=scale(train_RW[,c(15:21)], center = TRUE, scale = TRUE)
tr_Z=predict(pcr_RW_Z, train_RW_Z)

# PC1 PC2 PC3 PC4 explains more than 95% of the variation

### For corr  ###

pcr_RW_corr=prcomp(train_RW[,c(22:24)], center = TRUE, scale. = TRUE)

attributes(pcr_RW_corr)
summary(pcr_RW_corr)

train_RW_corr=scale(train_RW[,c(22:24)], center = TRUE, scale = TRUE)
tr_corr=predict(pcr_RW_corr, train_RW_corr)

# PC1 PC2 PC3 explains more than 95% of the variation

## For demographic ###

pcr_RW_demo=prcomp(train_RW[,c(26:28)], center = TRUE, scale. = TRUE)
summary(pcr_RW_demo)

train_RW_demo=scale(train_RW[,c(26:28)], center = TRUE, scale = TRUE)
tr_demo=predict(pcr_RW_demo, train_RW_demo)

# PC1 PC2 PC3 explains more than 95% of the variation



train=data.frame(tr_X[,c(1:4)],tr_Y[,c(1:4)],tr_Z[,c(1:4)],tr_corr[,c(1:3)],tr_demo[,c(1:3)],train_RW[,c(25,29)])



### For x Axis test data###

test_RW_X=scale(test_RW[,c(1:7)], center = TRUE, scale = TRUE)
tr_X_test=predict(pcr_RW_X, test_RW_X)

### For Y Axis test data###

test_RW_Y=scale(test_RW[,c(8:14)], center = TRUE, scale = TRUE)
tr_Y_test=predict(pcr_RW_Y, test_RW_Y)

### For Z Axis ###

test_RW_Z=scale(test_RW[,c(15:21)], center = TRUE, scale = TRUE)
tr_Z_test=predict(pcr_RW_Z, test_RW_Z)

### For corr Axis ###

test_RW_corr=scale(test_RW[,c(22:24)], center = TRUE, scale = TRUE)
tr_corr_test=predict(pcr_RW_corr, test_RW_corr)


### For demographic###

test_RW_demo=scale(test_RW[,c(26:28)], center = TRUE, scale = TRUE)
tr_demo_test=predict(pcr_RW_demo,test_RW_demo)

test=data.frame(tr_X_test[,c(1:4)],tr_Y_test[,c(1:4)],tr_Z_test[,c(1:4)],tr_corr_test[,c(1:3)],tr_demo_test[,c(1:3)],test_RW[,c(25,29)])


# proportional odds cumulative logit model

train$COS.Intensity=ordered(train$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test$COS.Intensity=ordered(test$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))


library(MASS)
model.odds_RW_axis=polr(COS.Intensity~., data = train , Hess = TRUE, method = "logistic")
summary(model.odds_RW_axis)


pred.odds_RW_axis=predict(model.odds_RW_axis, test[,-20])
table(pred.odds_RW_axis)
tab.odds_RW_axis=table(pred.odds_RW_axis,test[,17])
tab.odds_RW_axis
mcer.odds_RW_axis=1-sum(diag(tab.odds_RW_axis))/sum(tab.odds_RW_axis)
mcer.odds_RW_axis

## 1% improvement than previous PC

#### two level analysis ### 

train_RW$COS.Intensity=as.character(train_RW$COS.Intensity)
train_RW$COS.Intensity[train_RW$COS.Intensity=="SED"]="Non-active"
train_RW$COS.Intensity[train_RW$COS.Intensity=="LPA"]="Non-active"
train_RW$COS.Intensity[train_RW$COS.Intensity=="MPA"]="Active"
train_RW$COS.Intensity[train_RW$COS.Intensity=="VPA"]="Active"

table(train_RW$COS.Intensity)


test_RW$COS.Intensity=as.character(test_RW$COS.Intensity)
test_RW$COS.Intensity[test_RW$COS.Intensity=="SED"]="Non-active"
test_RW$COS.Intensity[test_RW$COS.Intensity=="LPA"]="Non-active"
test_RW$COS.Intensity[test_RW$COS.Intensity=="MPA"]="Active"
test_RW$COS.Intensity[test_RW$COS.Intensity=="VPA"]="Active"

table(test_RW$COS.Intensity)


train_RW$COS.Intensity=ordered(train_RW$COS.Intensity, levels = c("Non-active","Active"))
test_RW$COS.Intensity=ordered(test_RW$COS.Intensity, levels = c("Non-active","Active"))


### Proportional Odds Regression Model ####

library(MASS)
model.odds_RW_2=glm(COS.Intensity~.-mean.AL_RW_Z-var.AL_RW_Z-max.AL_RW_Z-min.AL_RW_Z-
                      quarlite.AL_RW_Z70-quarlite.AL_RW_Z80-quarlite.AL_RW_Z90, data = train_RW , family = binomial(link = "logit"))
summary(model.odds_RW_2)


pred.odds_RW_2=predict(model.odds_RW_2, test_RW[,-29], type = "response")

library(InformationValue)
cutOff=optimalCutoff(train_RW$COS.Intensity,pred.odds_RW_2[1])

pred.odds_RW_2=cut(pred.odds_RW_2, breaks = c(0,0.5128,1), labels = c("Non-active", "Active"))



tab.odds_RW_2=table(pred.odds_RW_2,test_RW[,29])
tab.odds_RW_2
mcer.odds_RW_2=1-sum(diag(tab.odds_RW_2))/sum(tab.odds_RW_2)
mcer.odds_RW_2


## random forest model ### 


library(randomForest)

set.seed(7757)
random.tree_RW_2=randomForest(COS.Intensity~., data = train_RW, mtry = 6, importance = TRUE)
random.tree_RW_2
importance(random.tree_RW_2)
varImpPlot(random.tree_RW_2)
pred.random_RW_2=predict(random.tree_RW_2, newdata = test_RW[,-29])
table_RW_2=table(pred.random_RW_2,test_RW[,29])
table_RW_2
mcer.random_RW_2=1-(sum(diag(table_RW_2))/sum(table_RW_2))
mcer.random_RW_2



