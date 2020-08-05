train_RW=read.table("AL.RW.2A_corr.txt", header = T)
test_RW=read.table("AL.RW.2B.txt", header = T)

train_RW$COS.Intensity=ordered(train_RW$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test_RW$COS.Intensity=ordered(test_RW$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))

train_RW$COS.Intensity=as.numeric(train_RW$COS.Intensity)
train_RW$COS.Intensity=as.factor(train_RW$COS.Intensity)


test_RW$COS.Intensity=as.numeric(test_RW$COS.Intensity)
test_RW$COS.Intensity=as.factor(test_RW$COS.Intensity)


train_RW$Wt=round(0.453592*train_RW$Wt,1)
train_RW$Ht=round(0.0254*train_RW$Ht,1)
train_RW$BMI=train_RW$Wt/train_RW$Ht^2

test_RW$Wt=round(0.453592*test_RW$Wt,1)
test_RW$Ht=round(0.0254*test_RW$Ht,1)
test_RW$BMI=test_RW$Wt/test_RW$Ht^2

## Ordinal Forest

library(ordinalForest)

rd.ord.RW=ordfor("COS.Intensity", train_RW[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                 perffunction = "proportional", nbest = 5,
                 ntreefinal = 5000, mtry = 6)



preds.RW=predict(rd.ord.RW, newdata = test_RW[,c(1:26,29,30)])

tab.RW=table(test_RW$COS.Intensity, preds.RW$ypred)

mcer.RW=1-(sum(diag(tab.RW))/sum(tab.RW))

library(vcd)
agreementplot(tab.RW, main="Agreement chart for RW", xlab="True class", ylab = "Predicted class")
Kappa(tab.RW, weights = "Equal-Spacing")



## Ordinal Forest equal

library(ordinalForest)

rd.ord.RW.eq=ordfor("COS.Intensity", train_RW[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                 perffunction = "equal", nbest = 5,
                 ntreefinal = 5000, mtry = 6)



preds.RW.eq=predict(rd.ord.RW.eq, newdata = test_RW[,c(1:26,29,30)])

tab.RW.eq=table(test_RW$COS.Intensity, preds.RW.eq$ypred)

mcer.RW.eq=1-(sum(diag(tab.RW.eq))/sum(tab.RW.eq))


library(vcd)
agreementplot(tab.RW.eq, main="Agreement chart for RW EQ", xlab="True class", ylab = "Predicted class")
Kappa(tab.RW.eq, weights = "Equal-Spacing")


## Random Forest

library(randomForest)


tune.RW=tuneRF(train_RW[,c(1:26,30)],train_RW[,29], mtryStart = 6,
               improve = .05, plot = TRUE, doBest = TRUE)



rf.RW=randomForest(train_RW[,c(1:26,30)],train_RW[,29], data = train_RW, mtry = 12,
                   importance = TRUE)

rf.RW

preds.RW.rf=predict(rf.RW, newdata = test_RW[,c(1:26,29,30)])

tab.RW.rf=table(test_RW$COS.Intensity, preds.RW.rf)

mcer.RW.rf=1-(sum(diag(tab.RW.rf))/sum(tab.RW.rf))

library(vcd)
Kappa(tab.RW.rf, weights = "Equal-Spacing")
agreementplot(tab.RW.rf, main="Agreement chart for RW rf", xlab="True class", ylab = "Predicted class")


## 2 level




library(randomForest)

train_RW=read.table("AL.RW.2A_corr.txt", header = T)
test_RW=read.table("AL.RW.2B.txt", header = T)

train_RW$COS.Intensity=as.character(train_RW$COS.Intensity)
train_RW$COS.Intensity[train_RW$COS.Intensity=="SED"]="Non-Active"
train_RW$COS.Intensity[train_RW$COS.Intensity=="LPA"]="Non-Active"
train_RW$COS.Intensity[train_RW$COS.Intensity=="MPA"]="Active"
train_RW$COS.Intensity[train_RW$COS.Intensity=="VPA"]="Active"

test_RW$COS.Intensity=as.character(test_RW$COS.Intensity)
test_RW$COS.Intensity[test_RW$COS.Intensity=="SED"]="Non-Active"
test_RW$COS.Intensity[test_RW$COS.Intensity=="LPA"]="Non-Active"
test_RW$COS.Intensity[test_RW$COS.Intensity=="MPA"]="Active"
test_RW$COS.Intensity[test_RW$COS.Intensity=="VPA"]="Active"

library(tidyverse)

train_RW$COS.Intensity=recode_factor(train_RW$COS.Intensity, 'Non-Active'=1,'Active'=2,.ordered = TRUE )
test_RW$COS.Intensity=recode_factor(test_RW$COS.Intensity,'Non-Active'=1,'Active'=2, .ordered = TRUE )

train_RW$Wt=round(0.453592*train_RW$Wt,1)
train_RW$Ht=round(0.0254*train_RW$Ht,1)
train_RW$BMI=train_RW$Wt/train_RW$Ht^2

test_RW$Wt=round(0.453592*test_RW$Wt,1)
test_RW$Ht=round(0.0254*test_RW$Ht,1)
test_RW$BMI=test_RW$Wt/test_RW$Ht^2


library(randomForest)

rdfor.RW=randomForest(COS.Intensity~., data = train_RW[,c(1:26,29,30)], mtry = 6, importance = TRUE)
rdfor.RW
varImpPlot(rdfor.RW)
pred.random_RW=predict(rdfor.RW, newdata = test_RW[,c(1:26,30)])
table_RW=table(pred.random_RW,test_RW[,29])
mcer.random_RW=1-(sum(diag(table_RW))/sum(table_RW))
mcer.random_RW
Kappa(table_RW, weights = "Equal-Spacing")
