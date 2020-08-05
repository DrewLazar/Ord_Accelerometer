train_ANK=read.table("AL.ANK.2A_corr.txt", header = T)
test_ANK=read.table("AL.ANK.2B.txt", header = T)



train_ANK$COS.Intensity=ordered(train_ANK$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test_ANK$COS.Intensity=ordered(test_ANK$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))

train_ANK$COS.Intensity=as.numeric(train_ANK$COS.Intensity)
train_ANK$COS.Intensity=as.factor(train_ANK$COS.Intensity)


test_ANK$COS.Intensity=as.numeric(test_ANK$COS.Intensity)
test_ANK$COS.Intensity=as.factor(test_ANK$COS.Intensity)



train_ANK$Wt=round(0.453592*train_ANK$Wt,1)
train_ANK$Ht=round(0.0254*train_ANK$Ht,1)
train_ANK$BMI=train_ANK$Wt/train_ANK$Ht^2

test_ANK$Wt=round(0.453592*test_ANK$Wt,1)
test_ANK$Ht=round(0.0254*test_ANK$Ht,1)
test_ANK$BMI=test_ANK$Wt/test_ANK$Ht^2

## ordinal forest prop

library(ordinalForest)

rd.ord.ANK=ordfor("COS.Intensity", train_ANK[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                  perffunction = "proportional", nbest = 5,
                  ntreefinal = 5000, mtry = 6)

preds.ANK=predict(rd.ord.ANK, newdata = test_ANK[,c(1:26,29,30)])

tab.ANK=table(test_ANK$COS.Intensity, preds.ANK$ypred)

mcer.ANK=1-(sum(diag(tab.ANK))/sum(tab.ANK))

library(vcd)
agreementplot(tab.ANK, main="Agreement chart for ANK", xlab="True class", ylab = "Predicted class")
Kappa(tab.LW, weights = "Equal-Spacing")


## ordinal forest eq

library(ordinalForest)

rd.ord.ANK.eq=ordfor("COS.Intensity", train_ANK[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                  perffunction = "equal", nbest = 5,
                  ntreefinal = 5000, mtry = 6)

preds.ANK.eq=predict(rd.ord.ANK.eq, newdata = test_ANK[,c(1:26,29,30)])

tab.ANK.eq=table(test_ANK$COS.Intensity, preds.ANK.eq$ypred)

mcer.ANK.eq=1-(sum(diag(tab.ANK.eq))/sum(tab.ANK.eq))


library(vcd)
agreementplot(tab.ANK, main="Agreement chart", xlab="True class", ylab = "Predicted class", ps = 10)
Kappa(tab.ANK)


## Random Forest

library(randomForest)


tune.ANK=tuneRF(train_ANK[,c(1:26,30)],train_ANK[,29], mtryStart = 6,
               improve = .05, plot = TRUE, doBest = TRUE)



rf.ANK=randomForest(train_ANK[,c(1:26,30)],train_ANK[,29], data = train_ANK, mtry = 12,
                   importance = TRUE)

rf.ANK

preds.ANK.rf=predict(rf.ANK, newdata = test_ANK[,c(1:26,29,30)])

tab.ANK.rf=table(test_ANK$COS.Intensity, preds.ANK.rf)

mcer.ANK.rf=1-(sum(diag(tab.ANK.rf))/sum(tab.ANK.rf))

library(vcd)
Kappa(tab.ANK.rf, weights = "Equal-Spacing")
agreementplot(tab.ANK.rf, main="Agreement chart for ANK rf", xlab="True class", ylab = "Predicted class")


### 2 Level

library(randomForest)

train_ANK=read.table("AL.ANK.2A_corr.txt", header = T)
test_ANK=read.table("AL.ANK.2B.txt", header = T)

train_ANK$COS.Intensity=as.character(train_ANK$COS.Intensity)
train_ANK$COS.Intensity[train_ANK$COS.Intensity=="SED"]="Non-Active"
train_ANK$COS.Intensity[train_ANK$COS.Intensity=="LPA"]="Non-Active"
train_ANK$COS.Intensity[train_ANK$COS.Intensity=="MPA"]="Active"
train_ANK$COS.Intensity[train_ANK$COS.Intensity=="VPA"]="Active"

test_ANK$COS.Intensity=as.character(test_ANK$COS.Intensity)
test_ANK$COS.Intensity[test_ANK$COS.Intensity=="SED"]="Non-Active"
test_ANK$COS.Intensity[test_ANK$COS.Intensity=="LPA"]="Non-Active"
test_ANK$COS.Intensity[test_ANK$COS.Intensity=="MPA"]="Active"
test_ANK$COS.Intensity[test_ANK$COS.Intensity=="VPA"]="Active"

library(tidyverse)

train_ANK$COS.Intensity=recode_factor(train_ANK$COS.Intensity, 'Non-Active'=1,'Active'=2,.ordered = TRUE )
test_ANK$COS.Intensity=recode_factor(test_ANK$COS.Intensity,'Non-Active'=1,'Active'=2, .ordered = TRUE )

train_ANK$Wt=round(0.453592*train_ANK$Wt,1)
train_ANK$Ht=round(0.0254*train_ANK$Ht,1)
train_ANK$BMI=train_ANK$Wt/train_ANK$Ht^2

test_ANK$Wt=round(0.453592*test_ANK$Wt,1)
test_ANK$Ht=round(0.0254*test_ANK$Ht,1)
test_ANK$BMI=test_ANK$Wt/test_ANK$Ht^2


library(randomForest)

rdfor.ANK=randomForest(COS.Intensity~., data = train_ANK[,c(1:26,29,30)], mtry = 6, importance = TRUE)
rdfor.ANK
varImpPlot(rdfor.ANK)
pred.random_ANK=predict(rdfor.ANK, newdata = test_ANK[,c(1:26,30)])
table_ANK=table(pred.random_ANK,test_ANK[,29])
mcer.random_ANK=1-(sum(diag(table_ANK))/sum(table_ANK))
mcer.random_ANK
Kappa(table_ANK, weights = "Equal-Spacing")
