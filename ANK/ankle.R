train_ANK=read.table("AL.ANK.2A_corr.txt", header = T)
test_ANK=read.table("AL.ANK.2B.txt", header = T)

train_ANK$COS.Intensity=ordered(train_ANK$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))
test_ANK$COS.Intensity=ordered(test_ANK$COS.Intensity, levels = c("SED","LPA","MPA","VPA"))


agg_ANK=count(train_ANK,Age,Ht,Wt,Sex)
table(agg_ANK$Sex)
agg2_ANK=count(test_ANK,Age,Ht,Wt,Sex)
table(agg2_ANK$Sex)


library(MASS)
model.odds_ANK=polr(COS.Intensity~., data = train_ANK , Hess = TRUE, method = "logistic")
summary(model.odds_ANK)




coeff_ANK=coef(summary(model.odds_ANK))
p_ANK=pnorm(abs(coeff[,"t value"]), lower.tail = FALSE)*2
coeff_ANK=cbind(coeff,"p value"= p_ANK)
coeff_ANK

library(effects)
Effect(focal.predictors = "Sex",model.odds_ANK)
par(mfrow=c(2,1))
plot(Effect(focal.predictors = "Age",model.odds_ANK))
plot(Effect(focal.predictors = "Wt",model.odds_ANK))
plot(Effect(focal.predictors = c("Age", "Wt"),model.odds_ANK))

pred.odds_ANK=predict(model.odds_ANK, test_ANK[,-29])
tab_ANK=table(pred.odds_ANK,test_ANK[,29])
tab_ANK
mcer.odds_ANK=1-sum(diag(tab_ANK))/sum(tab_ANK)
mcer.odds_ANK



#### Bayesian Proportional Odds Model


library(rstanarm)
library(ggplot2)
library(bayesplot)


bayes.model_ANK=stan_polr(COS.Intensity~., data = train_ANK, method = "logistic", prior = R2(location = .25, what = "mean"), prior_counts = dirichlet(0.5))
summary(bayes.model_ANK)

posterior_ANK=as.matrix(bayes.model_ANK)
plot_title=ggtitle("Posterior Distributions")
mcmc_areas(posterior_ANK,pars=c("mean.AL_ANK_X","mean.AL_ANK_Y","mean.AL_ANK_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_ANK,pars=c("var.AL_ANK_X","var.AL_ANK_Y","var.AL_ANK_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_ANK,pars=c("min.AL_ANK_X","min.AL_ANK_Y","min.AL_ANK_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_ANK,pars=c("max.AL_ANK_X","max.AL_ANK_Y","max.AL_ANK_Z"),prob = 0.95)+plot_title
mcmc_areas(posterior_ANK,pars=c("quarlite.AL_ANK_X70","quarlite.AL_ANK_Y70","quarlite.AL_ANK_Z70"),prob = 0.95)+plot_title
mcmc_areas(posterior_ANK,pars=c("corri.AL_ANK_XY","corri.AL_ANK_YZ","corri.AL_ANK_XZ"),prob = 0.95)+plot_title
mcmc_areas(posterior_ANK,pars=c("Age","Ht","Wt"),prob = 0.95)+plot_title


###launch_shinystan(bayes.model_ANK, ppd = FALSE)

## Prediction

pred_bayes_ANK=posterior_predict(bayes.model_ANK,test_ANK[,-29])

prop=function(x){
  tab=table(x)
  pr=prop.table(tab)
  mx=which.max(pr)
  return(labels(mx))
}

new_ANK=apply(pred_bayes_ANK, 2, prop)
new_ANK=ordered(new_ANK, levels = c("SED","LPA","MPA","VPA"))

mcer.bayes_ANK=1-sum(diag(table(new_ANK,test_ANK[,29])))/sum(table(new_ANK,test_ANK[,29]))


#### RANDOM FOREST

library(randomForest)

set.seed(7757)
random.tree_ANK=randomForest(COS.Intensity~., data = train_ANK, mtry = 9, importance = TRUE)
random.tree_ANK
importance(random.tree_ANK)
varImpPlot(random.tree_ANK)
pred.random_ANK=predict(random.tree_ANK, newdata = test_ANK[,-29])
table_ANK=table(pred.random_ANK,test_ANK[,29])
mcer.random_ANK=1-(sum(diag(table_ANK))/sum(table_ANK))
mcer.random_ANK

#### Bagging ####


set.seed(7757)
bag.tree_ANK=randomForest(COS.Intensity~., data = train_ANK, mtry = 28, importance = TRUE)
bag.tree_ANK
importance(bag.tree_ANK)
varImpPlot(bag.tree_ANK)
pred.bag_ANK=predict(bag.tree_ANK, newdata = test_ANK[,-29])
table_ANK=table(pred.bag_ANK,test_ANK[,29])
mcer.bag.ANK=1-(sum(diag(table_ANK))/sum(table_ANK))
mcer.bag.ANK



##### Boosting ####

library(gbm)

set.seed(7751)
boosting.ANK=gbm(COS.Intensity~., data = train_ANK, distribution = "multinomial", n.trees = 1000, interaction.depth = 2,
                shrinkage = .01)
summary(boosting.ANK)
pred.bag_ANK=predict.gbm(boosting.ANK, newdata = test_ANK[,-29], n.trees = 1000, type = "response")
p.predbag_ANK = apply(pred.bag_ANK, 1, which.max)
table1_ANK=table(p.predbag_ANK,test_ANK[,29])
table1_ANK
mcer.boosting_ANK=1-sum(diag(table1_ANK))/sum(table1_ANK)
mcer.boosting_ANK



#### Result of Left Wrist ####



results=cbind(mcer.bayes_ANK,mcer.odds_ANK,mcer.random_ANK,mcer.bag.ANK,mcer.boosting_ANK)
result_ANK=data.frame(results, row.names = "ANK")
write.table(result_ANK,"C:/Users/shossain/Desktop/ANK.txt")
