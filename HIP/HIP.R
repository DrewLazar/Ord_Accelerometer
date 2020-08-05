train_HIP=read.table("AL.HIP.2A_corr.txt", header = T)
test_HIP=read.table("AL.HIP.2B.txt", header = T)

train_HIP$COS.Intensity=ordered(train_HIP$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test_HIP$COS.Intensity=ordered(test_HIP$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))


library(tidyverse)

agg_HIP=count(train_HIP,Age,Ht,Wt,Sex)
table(agg_HIP$Sex)
agg2_HIP=count(test_HIP,Age,Ht,Wt,Sex)
table(agg2_HIP$Sex)

### Proportional Odds Regression Model ####

library(MASS)
model.odds_HIP=polr(COS.Intensity~., data = train_HIP , Hess = TRUE, method = "logistic")
summary(model.odds_HIP)


coeff_HIP=coef(summary(model.odds_HIP))
p_HIP=pnorm(abs(coeff[,"t value"]), lower.tail = FALSE)*2
coeff_HIP=cbind(coeff,"p value"= p_HIP)
coeff_HIP


pred.odds_HIP=predict(model.odds_HIP, test_HIP[,-29])
tab_HIP=table(pred.odds_HIP,test_HIP[,29])
tab_HIP
mcer.odds_HIP=1-sum(diag(tab_HIP))/sum(tab_HIP)
mcer.odds_HIP

#### Bayesian Proportional Odds Model ####

library(rstan)
library(rstanarm)
library(ggplot2)
library(bayesplot)


bayes.model_HIP=stan_polr(COS.Intensity~., data = train_HIP, method = "logistic", prior = NULL, prior_counts = dirichlet(1))
summary(bayes.model_HIP)

posterior_HIP=as.matrix(bayes.model_HIP)
plot_title=ggtitle("Posterior Distributions")
mcmc_areas(posterior_HIP,pars=c("mean.AL_HIP_X","mean.AL_HIP_Y","mean.AL_HIP_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_HIP,pars=c("var.AL_HIP_X","var.AL_HIP_Y","var.AL_HIP_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_HIP,pars=c("min.AL_HIP_X","min.AL_HIP_Y","min.AL_HIP_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_HIP,pars=c("max.AL_HIP_X","max.AL_HIP_Y","max.AL_HIP_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_HIP,pars=c("quarlite.AL_HIP_X70","quarlite.AL_HIP_Y70","quarlite.AL_HIP_Z70"),prob = 0.95)+plot_title
mcmc_areas(posterior_HIP,pars=c("corri.AL_HIP_XY","corri.AL_HIP_YZ","corri.AL_HIP_XZ"),prob = 0.95)+plot_title
mcmc_areas(posterior_HIP,pars=c("Age","Ht","Wt"),prob = 0.95)+plot_title


## trace plot

trace_HIP=stan_trace(bayes.model_HIP)
trace_HIP


## posterior predictive checks

pp_HIP=pp_check(bayes.model_HIP)
pp_HIP

## Prediction

pred_bayes_HIP=posterior_predict(bayes.model_HIP,test_HIP[,-29])

prop=function(x){
  tab=table(x)
  pr=prop.table(tab)
  mx=which.max(pr)
  return(labels(mx))
}

new_HIP=apply(pred_bayes_HIP, 2, prop)
new_HIP=ordered(new_HIP, levels = c("SED","LPA","MPA","VPA"))

mcer.bayes_HIP=1-sum(diag(table(new_HIP,test_HIP[,29])))/sum(table(new_HIP,test_HIP[,29]))



#### RANDOM FOREST ####

library(randomForest)

set.seed(7757)
random.tree_HIP=randomForest(COS.Intensity~., data = train_HIP, mtry = 9, importance = TRUE)
random.tree_HIP
importance(random.tree_HIP)
varImpPlot(random.tree_HIP)
pred.random_HIP=predict(random.tree_HIP, newdata = test_HIP[,-29])
table_HIP=table(pred.random_HIP,test_HIP[,29])
mcer.random_HIP=1-(sum(diag(table_HIP))/sum(table_HIP))
mcer.random_HIP



#### Bagging ####


set.seed(7757)
bag.tree_HIP=randomForest(COS.Intensity~., data = train_HIP, mtry = 28, importance = TRUE)
bag.tree_HIP
importance(bag.tree_HIP)
varImpPlot(bag.tree_HIP)
pred.bag_HIP=predict(bag.tree_HIP, newdata = test_HIP[,-29])
table_HIP=table(pred.bag_HIP,test_HIP[,29])
mcer.bag.HIP=1-(sum(diag(table_HIP))/sum(table_HIP))
mcer.bag.HIP


##### Boosting ####

library(gbm)

set.seed(7751)
boosting.HIP=gbm(COS.Intensity~., data = train_HIP, distribution = "multinomial", n.trees = 1000, interaction.depth = 2,
                shrinkage = .01)
summary(boosting.HIP)
pred.bag_HIP=predict.gbm(boosting.HIP, newdata = test_HIP[,-29], n.trees = 1000, type = "response")
p.predbag_HIP = apply(pred.bag_HIP, 1, which.max)
table1_HIP=table(p.predbag_HIP,test_HIP[,29])
table1_HIP
mcer.boosting_HIP=1-sum(diag(table1_HIP))/sum(table1_HIP)
mcer.boosting_HIP



#### Result of HIP ####


results=cbind(mcer.bayes_HIP,mcer.odds_HIP,mcer.random_HIP,mcer.bag.HIP,mcer.boosting_HIP)
result_HIP=data.frame(results, row.names = "HIP")
