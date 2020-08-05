train_LW=read.table("AL.LW.2A_corr.txt", header = T)
test_LW=read.table("AL.LW.2B.txt", header = T)

library(tidyverse)

train_LW$COS.Intensity=recode_factor(train_LW$COS.Intensity, 'SED'=1,'LPA'=2, 'MPA'=3, 'VPA'=4, .ordered = TRUE )
test_LW$COS.Intensity=recode_factor(test_LW$COS.Intensity, 'SED'=1,'LPA'=2, 'MPA'=3, 'VPA'=4, .ordered = TRUE )

train_LW$Wt=round(0.453592*train_LW$Wt,1)
train_LW$Ht=round(0.0254*train_LW$Ht,1)
train_LW$BMI=train_LW$Wt/train_LW$Ht^2

test_LW$Wt=round(0.453592*test_LW$Wt,1)
test_LW$Ht=round(0.0254*test_LW$Ht,1)
test_LW$BMI=test_LW$Wt/test_LW$Ht^2

## Ordinal Random Forest Proportional

library(ordinalForest)

rd.ord.LW=ordfor("COS.Intensity", train_LW[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                 perffunction = "proportional", nbest = 5,
                  ntreefinal = 5000, mtry = 6)


rd.ord.LW$varimp

preds.LW=predict(rd.ord.LW, newdata = test_LW[,c(1:26,29,30)])

tab.LW=table(test_LW$COS.Intensity, preds.LW$ypred)

mcer.LW=1-(sum(diag(tab.LW))/sum(tab.LW))


library(vcd)
agreementplot(tab.ANK, main="Agreement chart for ANK", xlab="True class", ylab = "Predicted class")
Kappa(tab.LW, weights = "Equal-Spacing")


## Ordinal Random Forest EQUAL

library(ordinalForest)

rd.ord.LW.eq=ordfor("COS.Intensity", train_LW[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                 perffunction = "equal", nbest = 5,
                 ntreefinal = 5000, mtry = 6)


rd.ord.LW.eq$varimp

preds.LW.eq=predict(rd.ord.LW.eq, newdata = test_LW[,c(1:26,29,30)])

tab.LW.eq=table(test_LW$COS.Intensity, preds.LW.eq$ypred)

mcer.LW.eq=1-(sum(diag(tab.LW.eq))/sum(tab.LW.eq))


library(vcd)
agreementplot(tab.LW.eq, main="Agreement chart for LW EQ", xlab="True class", ylab = "Predicted class")
Kappa(tab.LW.eq, weights = "Equal-Spacing")




## Random Forest

library(randomForest)


tune.LW=tuneRF(train_LW[,c(1:26,30)],train_LW[,29], mtryStart = 6,
               improve = .05, plot = TRUE, doBest = TRUE)



rf.LW=randomForest(train_LW[,c(1:26,30)],train_LW[,29], data = train_LW, mtry = 6,
                   importance = TRUE)

rf.LW

preds.LW.rf=predict(rf.LW, newdata = test_LW[,c(1:26,29,30)])

tab.LW.rf=table(test_LW$COS.Intensity, preds.LW.rf)

mcer.LW.rf=1-(sum(diag(tab.LW.rf))/sum(tab.LW.rf))

library(vcd)
Kappa(tab.LW.rf, weights = "Equal-Spacing")
agreementplot(tab.LW.rf, main="Agreement chart for LW rf", xlab="True class", ylab = "Predicted class")


## 2 level




library(randomForest)

train_LW=read.table("AL.LW.2A_corr.txt", header = T)
test_LW=read.table("AL.LW.2B.txt", header = T)

train_LW$COS.Intensity=as.character(train_LW$COS.Intensity)
train_LW$COS.Intensity[train_LW$COS.Intensity=="SED"]="Non-Active"
train_LW$COS.Intensity[train_LW$COS.Intensity=="LPA"]="Non-Active"
train_LW$COS.Intensity[train_LW$COS.Intensity=="MPA"]="Active"
train_LW$COS.Intensity[train_LW$COS.Intensity=="VPA"]="Active"

test_LW$COS.Intensity=as.character(test_LW$COS.Intensity)
test_LW$COS.Intensity[test_LW$COS.Intensity=="SED"]="Non-Active"
test_LW$COS.Intensity[test_LW$COS.Intensity=="LPA"]="Non-Active"
test_LW$COS.Intensity[test_LW$COS.Intensity=="MPA"]="Active"
test_LW$COS.Intensity[test_LW$COS.Intensity=="VPA"]="Active"

library(tidyverse)

train_LW$COS.Intensity=recode_factor(train_LW$COS.Intensity, 'Non-Active'=1,'Active'=2,.ordered = TRUE )
test_LW$COS.Intensity=recode_factor(test_LW$COS.Intensity,'Non-Active'=1,'Active'=2, .ordered = TRUE )

train_LW$Wt=round(0.453592*train_LW$Wt,1)
train_LW$Ht=round(0.0254*train_LW$Ht,1)
train_LW$BMI=train_LW$Wt/train_LW$Ht^2

test_LW$Wt=round(0.453592*test_LW$Wt,1)
test_LW$Ht=round(0.0254*test_LW$Ht,1)
test_LW$BMI=test_LW$Wt/test_LW$Ht^2


library(randomForest)

rdfor.LW=randomForest(COS.Intensity~., data = train_LW[,c(1:26,29,30)], mtry = 6, importance = TRUE)
rdfor.LW
varImpPlot(rdfor.LW)
pred.random_LW=predict(rdfor.LW, newdata = test_LW[,c(1:26,30)])
table_LW=table(pred.random_LW,test_LW[,29])
mcer.random_LW=1-(sum(diag(table_LW))/sum(table_LW))
mcer.random_LW
Kappa(table_LW, weights = "Equal-Spacing")
