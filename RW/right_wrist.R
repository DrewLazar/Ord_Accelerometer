train_RW=read.table("AL.RW.2A_corr.txt", header = T)
test_RW=read.table("AL.RW.2B.txt", header = T)

train_RW$COS.Intensity=ordered(train_RW$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test_RW$COS.Intensity=ordered(test_RW$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))



library(tidyverse)

agg_RW=count(train_RW,Age,Ht,Wt,Sex)
table(agg_RW$Sex)
agg2_RW=count(test_RW,Age,Ht,Wt,Sex)
table(agg2_RW$Sex)

## Exploratory analysis on Z axis covariates ####

z_axis=train_RW[,c(15:21)]
par(mfrow=c(3,2))
hist(z_axis$mean.AL_RW_Z)
hist(train_RW$mean.AL_RW_X)
hist(z_axis$var.AL_RW_Z)
hist(train_RW$var.AL_RW_X)
hist(z_axis$max.AL_RW_Z)
hist(train_RW$max.AL_RW_X)

par(mfrow=c(3,2))

hist(z_axis$quarlite.AL_RW_Z70)
hist(train_RW$quarlite.AL_RW_X70)
hist(z_axis$quarlite.AL_RW_Z80)
hist(train_RW$quarlite.AL_RW_X80)
hist(z_axis$quarlite.AL_RW_Z90)
hist(train_RW$quarlite.AL_RW_X90)



hist(z_axis$min.AL_RW_Z)
hist(train_RW$min.AL_RW_X)

### Proportional Odds Regression Model ####


library(MASS)
model.odds_RW=polr(COS.Intensity~.-mean.AL_RW_Z-var.AL_RW_Z-max.AL_RW_Z-min.AL_RW_Z-
                     quarlite.AL_RW_Z70-quarlite.AL_RW_Z80-quarlite.AL_RW_Z90, data = train_RW , Hess = TRUE, method = "logistic")
summary(model.odds_RW)


coeff_RW=coef(summary(model.odds_RW))
p_RW=pnorm(abs(coeff_RW[,"t value"]), lower.tail = FALSE)*2
coeff_RW=cbind(coeff_RW,"p value"= p_RW)
coeff_RW


pred.odds_RW=predict(model.odds_RW, test_RW[,-29])
tab.odds_RW=table(pred.odds_RW,test_RW[,29])
tab.odds_RW
mcer.odds_RW=1-sum(diag(tab.odds_RW))/sum(tab.odds_RW)
mcer.odds_RW



#### Bayesian Proportional Odds Model ####

library(rstan)
library(rstanarm)
library(ggplot2)
library(bayesplot)


bayes.model_RW=stan_polr(COS.Intensity~.-mean.AL_RW_Z-var.AL_RW_Z-max.AL_RW_Z-min.AL_RW_Z-
                           quarlite.AL_RW_Z70-quarlite.AL_RW_Z80-quarlite.AL_RW_Z90, data = train_RW, method = "logistic", prior = R2(location = .75, what = "median"), prior_counts = dirichlet(0.25))
summary(bayes.model_RW)

posterior_RW=as.matrix(bayes.model_RW)
plot_title=ggtitle("Posterior Distributions")
mcmc_areas(posterior_RW,pars=c("mean.AL_RW_X","mean.AL_RW_Y"),prob = 0.95)+plot_title
mcmc_areas(posterior_RW,pars=c("var.AL_RW_X","var.AL_RW_Y"),prob = 0.95)+plot_title
mcmc_areas(posterior_RW,pars=c("min.AL_RW_X","min.AL_RW_Y"),prob = 0.95)+plot_title
mcmc_areas(posterior_RW,pars=c("max.AL_RW_X","max.AL_RW_Y"),prob = 0.95)+plot_title
mcmc_areas(posterior_RW,pars=c("quarlite.AL_RW_X70","quarlite.AL_RW_Y70"),prob = 0.95)+plot_title
mcmc_areas(posterior_RW,pars=c("corri.AL_RW_XY","corri.AL_RW_YZ"),prob = 0.95)+plot_title
mcmc_areas(posterior_RW,pars=c("Age","Ht","Wt"),prob = 0.95)+plot_title


## trace plot

trace_RW=stan_trace(bayes.model_RW)
trace_RW
## posterior predictive checks

pp_RW=pp_check(bayes.model_RW)
pp_RW
## posterior distribution

hit_RW=stan_hist(bayes.model_RW)
hit_RW
## Prediction

pred_bayes_RW=posterior_predict(bayes.model_RW,test_RW[,-29])

prop=function(x){
  tab=table(x)
  pr=prop.table(tab)
  mx=which.max(pr)
  return(labels(mx))
}

new_RW=apply(pred_bayes_RW, 2, prop)
new_RW=ordered(new_RW, levels = c("SED","LPA","MPA","VPA"))

mcer.bayes_RW=1-sum(diag(table(new_RW,test_RW[,29])))/sum(table(new_RW,test_RW[,29]))


#### RANDOM FOREST #### 

library(randomForest)

set.seed(7757)
random.tree_RW=randomForest(COS.Intensity~., data = train_RW, mtry = 6, importance = TRUE)
random.tree_RW
importance(random.tree_RW)
varImpPlot(random.tree_RW)
pred.random_RW=predict(random.tree_RW, newdata = test_RW[,-29])
table_RW=table(pred.random_RW,test_RW[,29])
mcer.random_RW=1-(sum(diag(table_RW))/sum(table_RW))
mcer.random_RW



#### Bagging ####


set.seed(7757)
bag.tree_RW=randomForest(COS.Intensity~., data = train_RW, mtry = 28, importance = TRUE)
bag.tree_RW
importance(bag.tree_RW)
varImpPlot(bag.tree_RW)
pred.bag_RW=predict(bag.tree_RW, newdata = test_RW[,-29])
table_RW=table(pred.bag_RW,test_RW[,29])
mcer.bag.RW=1-(sum(diag(table_RW))/sum(table_RW))
mcer.bag.RW




##### Boosting ####

library(gbm)

set.seed(7751)
boosting.RW=gbm(COS.Intensity~., data = train_RW, distribution = "multinomial", n.trees = 1000, interaction.depth = 2,
                shrinkage = .01)
summary(boosting.RW)
pred.bag_RW=predict.gbm(boosting.RW, newdata = test_RW[,-29], n.trees = 1000, type = "response")
p.predbag_RW = apply(pred.bag_RW, 1, which.max)
table1_RW=table(p.predbag_RW,test_RW[,29])
table1_RW
mcer.boosting_RW=1-sum(diag(table1_RW))/sum(table1_RW)
mcer.boosting_RW


#### Result of Right Wrist ####



results=cbind(mcer.bayes_RW,mcer.odds_RW,mcer.random_RW,mcer.bag.RW,mcer.boosting_RW)
result_RW=data.frame(results, row.names = "RW")
write.table(result_RW, "C:/Users/shossain/Desktop/RW.txt")
