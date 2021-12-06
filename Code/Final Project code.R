#install.packages('mice')
#install.packages('VIM')
library(mice)
library(VIM)
library(arm)

#load csv
churn <-read.table('/Users/mohammadanas/Desktop/Duke MIDS/Fall 2021/MODELLING AND REPRESENTATION OF DATA/Final Project/Churn_Modelling.csv', 
                   sep =",", header = TRUE, quote ="\"")

churn <- rename(churn, Salary = EstimatedSalary)
# load again as you messed up with creating missing in the same data rather than creating a copy 
churn_complete <-read.table('/Users/mohammadanas/Desktop/Duke MIDS/Fall 2021/MODELLING AND REPRESENTATION OF DATA/Final Project/Churn_Modelling.csv', 
                   sep =",", header = TRUE, quote ="\"")


columns = dim(churn)[1]
columns
unique(churn$Geography)

churn$gender_b = 0
churn$gender_b[churn$Gender == 'Female'] = 1


churn$Spain = 0
churn$Spain[churn$Geography == 'Spain'] = 1

churn$Germany = 0
churn$Germany[churn$Geography == 'Germany'] = 1

churn$France = 0
churn$France[churn$Geography == 'France'] = 1

# Age dependent variables 
age <- churn[,c('gender_b','Germany','France')]
age$French_w <- age$gender_b*age$France
age$Germany_w <- age$gender_b*age$Germany


estimators <- as.matrix(c(-3.8,2.8,-0.02,-0.01,1.1,1.9)) #MAR
data <- as.matrix(cbind(rep(1,columns),age[,c(1,2,3,4,5)]))

logit_pi_R_age <- data %*% estimators

pi_R_age <- exp(logit_pi_R_age)/(1+exp(logit_pi_R_age))
set.seed(10)
R_age <- rbinom(columns,1,pi_R_age)

per_age_missing <- (sum(R_age)*100)/columns
per_age_missing




# Age dependent variables 
#age <- churn[,c('gender_b')]



#estimators <- as.matrix(c(-0.19,0.14)) #MAR
#data <- as.matrix(cbind(rep(1,columns),age[,c(1)]))

#logit_pi_R_age <- data %*% estimators

#pi_R_age <- exp(logit_pi_R_age)/(1+exp(logit_pi_R_age))
#R_age <- rbinom(columns,1,pi_R_age)

#per_age_missing <- (sum(R_age)*100)/columns
#per_age_missing


# salary dependent variables 
Salary <- churn[,c('Tenure','IsActiveMember')]

estimators <- as.matrix(c(4.4,-1.3,-1.2)) #MAR
data <- as.matrix(cbind(rep(1,columns),Salary[,c(1,2)]))

logit_pi_R_salary <- data %*% estimators

pi_R_salary <- exp(logit_pi_R_salary)/(1+exp(logit_pi_R_salary))
set.seed(10)
R_salary <- rbinom(columns,1,pi_R_age)

per_salary_missing <- (sum(R_salary)*100)/columns
per_salary_missing

## append indicators to data and introducing missing values
churn <- cbind(churn, R_age, R_salary)
churn$Age[churn$R_age == 1] <- NA
churn$EstimatedSalary[churn$R_salary == 1] <- NA
#dim(churn[is.na(churn$EstimatedSalary),])

# filter out final data to model

churn_missing <- churn[,4:14]
churn_only_missing_columns <- churn_missing[,c(4,10)]
# check pattern
md.pattern(churn_only_missing_columns)
churn_missing$Salary <- churn_missing$EstimatedSalary
# do not include these variables as predictors for each other
marginplot(churn_missing[,c("Age","Salary")],col=c("lightblue3","darkred"))
churn2 <- churn_missing

churn2$Age_Missing <- 'Not Missing'
churn2$Salary_Missing <- 'Not Missing'
churn2$Age_Missing[is.na(churn2$Age)] <- 'Missing'
churn2$Salary_Missing[is.na(churn2$Salary)] <-'Missing'
library(ggplot2)
ggplot(churn2, aes(x=Age_Missing, y = Salary)) + geom_boxplot() 
ggplot(churn2, aes(x=Salary_Missing, y = Age)) + geom_boxplot() 


# Lets do some imputation
churn_missing$Geography <- factor(churn_missing$Geography)
churn_missing$Gender <- factor(churn_missing$Gender)
churn_missing$NumOfProducts <- factor(churn_missing$NumOfProducts)
churn_missing$HasCrCard <- factor(churn_missing$HasCrCard)
churn_missing$IsActiveMember <- factor(churn_missing$IsActiveMember)
churn_missing$Exited <- factor(churn_missing$Exited)

# impute them 

set.seed(1026)
imputed_Ds_ppm <- mice(churn_missing, m=10, defaultMethod = c("pmm", "rf", "rf", "ppm","ppm","ppm","rf","rf","rf","pmm","rf"), print=F)

predmatrix <- imputed_Ds_ppm$predictorMatrix
predmatrix["Age","Salary"] <- 0
predmatrix["Salary","Age"] <- 0

imputed_Ds_ppm <- mice(churn_missing, m=10, defaultMethod = c("pmm", "rf", "rf", "ppm","ppm","ppm","rf","rf","rf","pmm","rf"), print=F, predictorMatrix = predmatrix)


## 
#methods(mice)
# Distribution Plots of imputed datasets dont use this wont help much
#stripplot(imputed_Ds_ppm, col=c("grey", "darkred"))
options(scipen=10)
# use this in plot
densityplot(imputed_Ds_ppm)[2]

# just a check
xyplot(imputed_Ds_ppm, EstimatedSalary ~ .imp|Geography, col=c("grey","darkred"), pch=c(1,20))

# just a check
xyplot(imputed_Ds_ppm, Age ~ .imp|Gender, col=c("grey","darkred"), pch=c(1,20))

# Pick out two data sets at random and do eda on one or model building on one of them
d3 <- complete(imputed_Ds_ppm, 3)

d7 <- complete(imputed_Ds_ppm, 7)
d6 <- complete(imputed_Ds_ppm, 6)
d8 <- complete(imputed_Ds_ppm, 8)
d9 <- complete(imputed_Ds_ppm, 9)
d10 <- complete(imputed_Ds_ppm, 10)

# we choose data dataset 7 and go on with it

#EDA
# low p-value geography is dependent include in the model
chisq.test(table(d6[,c("Geography","Exited")]))
# include in the model low p-value include in the model
chisq.test(table(d6[,c("Gender","Exited")]))
# include in the model low p-value  include in the model
chisq.test(table(d6[,c("NumOfProducts","Exited")]))
# surprisingly independent so do not include
chisq.test(table(d6[,c("HasCrCard","Exited")]))
# surprisingly independent so do not include
chisq.test(table(d6[,c("HasCrCard","Exited")]))
# dependent so include
chisq.test(table(d6[,c("IsActiveMember","Exited")]))

# continuous variables
library(ggplot2)
# all most the same makes sense the as having a credit card was not also independent 
# Probably the product was not revolving credit card
ggplot(d6,aes(x=Exited, y=CreditScore)) + geom_boxplot() 
# include in the model as older people more likely to exit
ggplot(d6,aes(x=Exited, y=Age)) + geom_boxplot() 
# tenure does not matter much
ggplot(d6,aes(x=Exited, y=Tenure)) + geom_boxplot() 
# tenure does not matter much
ggplot(d6,aes(x=Exited, y=Tenure)) + geom_boxplot() 
# there is a difference but might just include
ggplot(d6,aes(x=Exited, y=Balance)) + geom_boxplot()
# there is not much affect of salary but given that we have imputed it, lets just include
ggplot(d6,aes(x=Exited, y=EstimatedSalary)) + geom_boxplot()


# check for interactions for salary (Except Geography)



# Gender
ggplot(d7,aes(x=Exited, y=EstimatedSalary)) + geom_boxplot() + facet_wrap(~Gender)

# Member
ggplot(d7,aes(x=Exited, y=EstimatedSalary)) + geom_boxplot() + facet_wrap(~IsActiveMember)

# Credit card
ggplot(d7,aes(x=Exited , y=EstimatedSalary)) + geom_boxplot() + facet_wrap(~HasCrCard)

# Num of products
ggplot(d7,aes(x=Exited, y=EstimatedSalary)) + geom_boxplot() + facet_wrap(~NumOfProducts)

# Geography
ggplot(d7,aes(x=Exited, y=EstimatedSalary)) + geom_boxplot() + facet_wrap(~Geography)




# check for interactions for balance (Except Geography)



# Gender
ggplot(d7,aes(x=Exited, y=Balance)) + geom_boxplot() + facet_wrap(~Gender)

# Member
ggplot(d7,aes(x=Exited, y=Balance)) + geom_boxplot() + facet_wrap(~IsActiveMember)

# Credit card
ggplot(d7,aes(x=Exited , y=Balance)) + geom_boxplot() + facet_wrap(~HasCrCard)

# Num of products
ggplot(d7,aes(x=Exited, y=Balance)) + geom_boxplot() + facet_wrap(~NumOfProducts)

# Geography
ggplot(d7,aes(x=Exited, y=Balance)) + geom_boxplot() + facet_wrap(~Geography)


# check for interactions for CreditScore (none needed)



# Gender
ggplot(d7,aes(x=Exited, y=CreditScore)) + geom_boxplot() + facet_wrap(~Gender)

# Member
ggplot(d7,aes(x=Exited, y=CreditScore)) + geom_boxplot() + facet_wrap(~IsActiveMember)

# Credit card
ggplot(d7,aes(x=Exited , y=CreditScore)) + geom_boxplot() + facet_wrap(~HasCrCard)

# Num of products
ggplot(d7,aes(x=Exited, y=CreditScore)) + geom_boxplot() + facet_wrap(~NumOfProducts)

# Geography
ggplot(d7,aes(x=Exited, y=CreditScore)) + geom_boxplot() + facet_wrap(~Geography)


# check for interactions for Age (none needed)



# Gender
ggplot(d7,aes(x=Exited, y=Age)) + geom_boxplot() + facet_wrap(~Gender)

# Member
ggplot(d7,aes(x=Exited, y=Age)) + geom_boxplot() + facet_wrap(~IsActiveMember)

# Credit card
ggplot(d7,aes(x=Exited , y=Age)) + geom_boxplot() + facet_wrap(~HasCrCard)

# Num of products
ggplot(d7,aes(x=Exited, y=Age)) + geom_boxplot() + facet_wrap(~NumOfProducts)

# Geography
ggplot(d7,aes(x=Exited, y=Age)) + geom_boxplot() + facet_wrap(~Geography)



### variables needed for model
# Geography
# active member
# Products
# Gender
# Age 
# Balance
# Salary
# Salary and Geography 
# Balance and Geography

 # model building AIC and BIC
modeld7 <- glm(formula = Exited ~ Age + Geography + IsActiveMember + CreditScore + HasCrCard +
                 Gender + Balance + EstimatedSalary + IsActiveMember + EstimatedSalary:Geography + 
                 Balance:Geography, family = binomial, data = d6)


null_model <- glm(Exited~1, data=d7, family=binomial)

step(null_model,scope=formula(modeld7),direction="both",
     trace=0)

modeld7 <-glm(formula = Exited ~ Age + IsActiveMember + Geography + Gender + EstimatedSalary +
              Balance + CreditScore, family = binomial, data = d6)

summary(modeld7)
# model assessment
rawresid1 <- residuals(modeld7,"resp")
binnedplot(x=fitted(modeld7),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")




# model assesesments
binnedplot(x=d7$Age,y=rawresid1,xlab="Parity centered",
           col.int="red4",ylab="Avg. residuals",main="Residual vs Age",col.pts="navy")


## We see that age is messed up 
# center and age square added  and cube also added 
d6$CreditScore <- d6$CreditScore - mean(d6$CreditScore)
d6$Age <- d6$Age - mean(d6$Age)
d6$EstimatedSalary <- d6$Estimated - mean(d6$EstimatedSalary)
d6$Balance <- d6$Balance - mean(d6$Balance)
d6$Age_sq <- (d6$Age)^2
d6$Age_cb <- (d6$Age)^3

# build model again 
modeld6 <-glm(formula = Exited ~ Age + Age_sq + Age_cb + IsActiveMember + Geography + Gender + EstimatedSalary +
                Balance + CreditScore, family = binomial, data = d6)
summary(modeld6)

# check assumption again
rawresid1 <- residuals(modeld6,"resp")
binnedplot(x=fitted(modeld6),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


binnedplot(x=d6$Age,y=rawresid1,xlab="Age centered",
           col.int="red4",ylab="Avg. residuals",main="Residual vs Age",col.pts="navy")

### trend improves with age cubed but not that much

#check the same model on d6

d6$CreditScore <- d6$CreditScore - mean(d6$CreditScore)
d6$Age <- d6$Age - mean(d6$Age)
d6$EstimatedSalary <- d6$Estimated - mean(d6$EstimatedSalary)
d6$Balance <- d6$Balance - mean(d6$Balance)
d6$Age_sq <- (d6$Age)^2
d6$Age_cb <- (d6$Age)^3

modeld6 <-glm(formula = Exited ~ Age + Age_sq + Age_cb + IsActiveMember + Geography + Gender + EstimatedSalary +
                Balance + CreditScore, family = binomial, data = d6)

summary(modeld6)

# check assumption again
rawresid1 <- residuals(modeld6,"resp")
binnedplot(x=fitted(modeld6),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


binnedplot(x=d6$Age,y=rawresid1,xlab="Parity centered",
           col.int="red4",ylab="Avg. residuals",main="Residual vs Age",col.pts="navy")


#check the same model on d3

d3$CreditScore <- d3$CreditScore - mean(d3$CreditScore)
d3$Age <- d3$Age - mean(d3$Age)
d3$EstimatedSalary <- d3$Estimated - mean(d3$EstimatedSalary)
d3$Balance <- d3$Balance - mean(d3$Balance)
d3$Age_sq <- (d3$Age)^2
d3$Age_cb <- (d3$Age)^3

modeld3 <-glm(formula = Exited ~ Age + Age_sq + Age_cb + IsActiveMember + Geography + Gender + EstimatedSalary +
                Balance + CreditScore, family = binomial, data = d3)

summary(modeld3)

# check assumption again
rawresid1 <- residuals(modeld3,"resp")
binnedplot(x=fitted(modeld3),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


binnedplot(x=d3$Age,y=rawresid1,xlab="Parity centered",
           col.int="red4",ylab="Avg. residuals",main="Residual vs Age",col.pts="navy")



# i think we are good to go 


centering <- function(s){
  a = s - mean(s)
  return(a)
}

centering_power <- function(s,power){
  k = s - mean(s)
  l <- k^power
  return(l)
  }
  
model_all <- with(data=imputed_Ds_ppm, glm(Exited ~  centering(Age) + centering_power(Age,2) + centering_power(Age,3) + IsActiveMember + Geography + Gender + centering(Salary) +
                                             centering(Balance) + centering(CreditScore), family = binomial))

pool_model <- pool(model_all)
a <- summary(pool_model)
Estimates <- a[,2]
P_value <- a[,6]
kable(cbind(Estimates, P_value))

## running the same model on actual dataset
churn_complete$Geography <- factor(churn_complete$Geography)
churn_complete$Gender <- factor(churn_complete$Gender)
churn_complete$NumOfProducts <- factor(churn_complete$NumOfProducts)
churn_complete$HasCrCard <- factor(churn_complete$HasCrCard)
churn_complete$IsActiveMember <- factor(churn_complete$IsActiveMember)
churn_complete$Exited <- factor(churn_complete$Exited)
churn_complete$CreditScore <- churn_complete$CreditScore - mean(churn_complete$CreditScore)
churn_complete$Age <- churn_complete$Age - mean(churn_complete$Age)
churn_complete$EstimatedSalary <- churn_complete$Estimated - mean(churn_complete$EstimatedSalary)
churn_complete$Balance <- churn_complete$Balance - mean(churn_complete$Balance)
churn_complete$Age_sq <- (churn_complete$Age)^2
churn_complete$Age_cb <- (churn_complete$Age)^3


modelactual <-glm(formula = Exited ~ Age + Age_sq + Age_cb + IsActiveMember + Geography + Gender + EstimatedSalary +
                Balance + CreditScore, family = binomial, data = churn_complete)

ac <- summary(modelactual)
ac <- as.data.frame(ac$coefficients)
estimates <- ac[,1]
P <- ac[,4]
library(xtable)
xtable(cbind(estimates, P))

## K-nearest neighbor imputation

library(VIM)
churn_knn <- kNN(churn_missing, variable = c('Age','Salary'), k=5)    

churn_knn$Geography <- factor(churn_knn$Geography)
churn_knn$Gender <- factor(churn_knn$Gender)
churn_knn$NumOfProducts <- factor(churn_knn$NumOfProducts)
churn_knn$HasCrCard <- factor(churn_knn$HasCrCard)
churn_knn$IsActiveMember <- factor(churn_knn$IsActiveMember)
churn_knn$Exited <- factor(churn_knn$Exited)
churn_knn$CreditScore <- churn_knn$CreditScore - mean(churn_knn$CreditScore)
churn_knn$Age <- churn_knn$Age - mean(churn_knn$Age)
churn_knn$EstimatedSalary <- churn_knn$EstimatedSalary - mean(churn_knn$EstimatedSalary)
churn_knn$Balance <- churn_knn$Balance - mean(churn_knn$Balance)
churn_knn$Age_sq <- (churn_knn$Age)^2
churn_knn$Age_cb <- (churn_knn$Age)^3

modelknn <-glm(formula = Exited ~ Age + Age_sq + Age_cb + IsActiveMember + Geography + Gender + EstimatedSalary +
                    Balance + CreditScore, family = binomial, data = churn_knn)
summary(modelknn)
knn <- as.data.frame(knn$coefficients)
estimates <- knn[,1]
P <- knn[,4]
library(xtable)
xtable(cbind(estimates, P))

summary(modelknn)
summary(modelactual)
summary(pool_model)


## estimate sigificance of salary
model_salary <- with(data=imputed_Ds_ppm, glm(Exited ~  centering(Age) + centering_power(Age,2) + centering_power(Age,3) + IsActiveMember + Geography + Gender + centering(EstimatedSalary) +
                                             centering(Balance) + centering(CreditScore), family = binomial))
model_nosalary <- with(data=imputed_Ds_ppm, glm(Exited ~  centering(Age) + centering_power(Age,2) + centering_power(Age,3) + IsActiveMember + Geography + Gender +
                                                centering(Balance) + centering(CreditScore), family = binomial))
pool.compare(model_salary, model_nosalary)[c(9:12,18)]

## shows that salary can be removed
#
pool_estimates <- summary(pool_model)$estimate
names(pool_estimates) <- names(model_pool_predict$coefficients)
pool_estimates
# make a model to replace its coefficients
model_pool_predict <- modelactual

# replace this models coefficients with model from pool_estimates
model_pool_predict$coefficients <- pool_estimates
predicted <- predict(model_pool_predict, churn_complete)
predicted_probabilities <- exp(predicted)/(1+exp(predicted))

## predict for knn model
predict <- predict(modelknn, churn_complete)
predict_by_knn <- exp(predict)/(1+exp(predict))


pool_estimates <- summary(pool_model)$estimate
names(pool_estimates) <- names(model_pool_predict$coefficients)
pool_estimates
# make a model to replace its coefficients
model_pool_predict <- modelactual

# replace this models coefficients with model from pool_estimates
model_pool_predict$coefficients <- pool_estimates
predicted <- predict(model_pool_predict, churn_complete)
predicted_probabilities <- exp(predicted)/(1+exp(predicted))

# For ROC and confusion matrix
#model multiple imputed
roc(churn_complete$Exited,predicted_probabilities,plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
# model multiple actual
roc(churn_complete$Exited,fitted(modelactual),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
# model multiple knn
roc(churn_complete$Exited,predict_by_knn,plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#model multiple imputed
Conf_mat_MI <- confusionMatrix(as.factor(ifelse(predicted_probabilities >= 0.213, "1","0")),
                            as.factor(churn_complete$Exited),positive = "1")
# model multiple actual
Conf_mat_actual <- confusionMatrix(as.factor(ifelse(fitted(modelactual) >= 0.205, "1","0")),
                            as.factor(churn_complete$Exited),positive = "1")
# model multiple knn
Conf_mat_KNN <- confusionMatrix(as.factor(ifelse(predict_by_knn >= 0.209, "1","0")),
                            as.factor(churn_complete$Exited),positive = "1")
#model multiple imputed
Conf_mat_MI$table
Conf_mat_MI$overall["Accuracy"];
Conf_mat_MI$byClass[c("Sensitivity","Specificity")]


#model multiple actual
Conf_mat_actual$table
Conf_mat_actual$overall["Accuracy"];
Conf_mat_actual$byClass[c("Sensitivity","Specificity")]

#model knn
Conf_mat_KNN$table
Conf_mat_KNN$overall["Accuracy"];
Conf_mat_KNN$byClass[c("Sensitivity","Specificity")]


library(pROC)
library(caret)

install.packages('knitr')









glm(Exited ~  centering(Age) + centering_power(Age,2) + centering_power(Age,3) + IsActiveMember + Geography + Gender + centering(EstimatedSalary) +
      centering(Balance) + centering(CreditScore), family = binomial, data=d10)
