train_HIP=read.table("AL.HIP.2A_corr.txt", header = T)
test_HIP=read.table("AL.HIP.2B.txt", header = T)


train_HIP$COS.Intensity=ordered(train_HIP$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test_HIP$COS.Intensity=ordered(test_HIP$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))

train_HIP$COS.Intensity=as.numeric(train_HIP$COS.Intensity)
train_HIP$COS.Intensity=as.factor(train_HIP$COS.Intensity)


test_HIP$COS.Intensity=as.numeric(test_HIP$COS.Intensity)
test_HIP$COS.Intensity=as.factor(test_HIP$COS.Intensity)


train_HIP$Wt=round(0.453592*train_HIP$Wt,1)
train_HIP$Ht=round(0.0254*train_HIP$Ht,1)
train_HIP$BMI=train_HIP$Wt/train_HIP$Ht^2

test_HIP$Wt=round(0.453592*test_HIP$Wt,1)
test_HIP$Ht=round(0.0254*test_HIP$Ht,1)
test_HIP$BMI=test_HIP$Wt/test_HIP$Ht^2

## Ordinal forest

library(ordinalForest)

rd.ord.HIP=ordfor("COS.Intensity", train_HIP[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                 perffunction = "proportional", nbest = 5,
                 ntreefinal = 5000, mtry = 6)


preds.HIP=predict(rd.ord.HIP, newdata = test_HIP[,c(1:26,29,30)])

tab.HIP=table(test_HIP$COS.Intensity, preds.HIP$ypred)

mcer.HIP=1-(sum(diag(tab.HIP))/sum(tab.HIP))


library(vcd)
agreementplot(tab.HIP, main="Agreement chart for HIP", xlab="True class", ylab = "Predicted class")
Kappa(tab.HIP, weights = "Equal-Spacing")


## Ordinal forest Equal

library(ordinalForest)

rd.ord.HIP.eq=ordfor("COS.Intensity", train_HIP[,c(1:26,29,30)],nsets = 2000, ntreeperdiv = 100, 
                  perffunction = "equal", nbest = 5,
                  ntreefinal = 5000, mtry = 6)


preds.HIP.eq=predict(rd.ord.HIP.eq, newdata = test_HIP[,c(1:26,29,30)])

tab.HIP.eq=table(test_HIP$COS.Intensity, preds.HIP.eq$ypred)

mcer.HIP.eq=1-(sum(diag(tab.HIP.eq))/sum(tab.HIP.eq))

agreementplot(tab.HIP.eq, main="Agreement chart for HIP EQ", xlab="True class", ylab = "Predicted class")

Kappa(tab.HIP.eq, weights = "Equal-Spacing")

## Random Forest

library(randomForest)


tune.HIP=tuneRF(train_HIP[,c(1:26,30)],train_HIP[,29], mtryStart = 6,
               improve = .05, plot = TRUE, doBest = TRUE)



rf.HIP=randomForest(train_HIP[,c(1:26,30)],train_HIP[,29], data = train_HIP, mtry = 6,
                   importance = TRUE)

rf.HIP

preds.HIP.rf=predict(rf.HIP, newdata = test_HIP[,c(1:26,29,30)])

tab.HIP.rf=table(test_HIP$COS.Intensity, preds.HIP.rf)

mcer.HIP.rf=1-(sum(diag(tab.HIP.rf))/sum(tab.HIP.rf))

library(vcd)
Kappa(tab.HIP.rf, weights = "Equal-Spacing")
agreementplot(tab.HIP.rf, main="Agreement chart for HIP rf", xlab="True class", ylab = "Predicted class")


## 2 level




library(randomForest)

train_HIP=read.table("AL.HIP.2A_corr.txt", header = T)
test_HIP=read.table("AL.HIP.2B.txt", header = T)

train_HIP$COS.Intensity=as.character(train_HIP$COS.Intensity)
train_HIP$COS.Intensity[train_HIP$COS.Intensity=="SED"]="Non-Active"
train_HIP$COS.Intensity[train_HIP$COS.Intensity=="LPA"]="Non-Active"
train_HIP$COS.Intensity[train_HIP$COS.Intensity=="MPA"]="Active"
train_HIP$COS.Intensity[train_HIP$COS.Intensity=="VPA"]="Active"

test_HIP$COS.Intensity=as.character(test_HIP$COS.Intensity)
test_HIP$COS.Intensity[test_HIP$COS.Intensity=="SED"]="Non-Active"
test_HIP$COS.Intensity[test_HIP$COS.Intensity=="LPA"]="Non-Active"
test_HIP$COS.Intensity[test_HIP$COS.Intensity=="MPA"]="Active"
test_HIP$COS.Intensity[test_HIP$COS.Intensity=="VPA"]="Active"

library(tidyverse)

train_HIP$COS.Intensity=recode_factor(train_HIP$COS.Intensity, 'Non-Active'=1,'Active'=2,.ordered = TRUE )
test_HIP$COS.Intensity=recode_factor(test_HIP$COS.Intensity,'Non-Active'=1,'Active'=2, .ordered = TRUE )

train_HIP$Wt=round(0.453592*train_HIP$Wt,1)
train_HIP$Ht=round(0.0254*train_HIP$Ht,1)
train_HIP$BMI=train_HIP$Wt/train_HIP$Ht^2

test_HIP$Wt=round(0.453592*test_HIP$Wt,1)
test_HIP$Ht=round(0.0254*test_HIP$Ht,1)
test_HIP$BMI=test_HIP$Wt/test_HIP$Ht^2


library(randomForest)

rdfor.HIP=randomForest(COS.Intensity~., data = train_HIP[,c(1:26,29,30)], mtry = 6, importance = TRUE)
rdfor.HIP
varImpPlot(rdfor.HIP)
pred.random_HIP=predict(rdfor.HIP, newdata = test_HIP[,c(1:26,30)])
table_HIP=table(pred.random_HIP,test_HIP[,29])
mcer.random_HIP=1-(sum(diag(table_HIP))/sum(table_HIP))
mcer.random_HIP
Kappa(table_HIP, weights = "Equal-Spacing")
