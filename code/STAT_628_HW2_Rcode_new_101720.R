Fatdata<-read.csv("D:\\Documents\\Stat 628\\BodyFat.csv",header=TRUE)

str(Fatdata)
summary(Fatdata)

install.packages("Metrics")
library(Metrics)


install.packages("caret")
library(caret)


Xvar<-c("AGE","WEIGHT","HEIGHT","ADIPOSITY","NECK","CHEST","ABDOMEN","HIP","THIGH","KNEE",
        "ANKLE","BICEPS","FOREARM","WRIST")

boxdata<-Fatdata[Xvar]
require(ggplot2)
par(mar=c(1,1,1,1))
boxplot(boxdata)


WEIGHT.out <- boxplot.stats(Fatdata$WEIGHT, coef=3)$out
WEIGHT_index <- which(Fatdata$WEIGHT %in% WEIGHT.out)
WEIGHT_index


HEIGHT.out <- boxplot.stats(Fatdata$HEIGHT, coef=3)$out
HEIGHT_index <- which(Fatdata$HEIGHT %in% HEIGHT.out)
HEIGHT_index

ADIPOSITY.out <- boxplot.stats(Fatdata$ADIPOSITY, coef=3)$out
ADIPOSITY_index <- which(Fatdata$ADIPOSITY %in% ADIPOSITY.out)
ADIPOSITY_index

NECK.out <- boxplot.stats(Fatdata$NECK, coef=3)$out
NECK_index <- which(Fatdata$NECK %in% NECK.out)
NECK_index


CHEST.out <- boxplot.stats(Fatdata$CHEST, coef=3)$out
CHEST_index <- which(Fatdata$CHEST %in% CHEST.out)
CHEST_index

ABDOMEN.out <- boxplot.stats(Fatdata$ABDOMEN, coef=3)$out
ABDOMEN_index <- which(Fatdata$ABDOMEN %in% ABDOMEN.out)
ABDOMEN_index

HIP.out <- boxplot.stats(Fatdata$HIP, coef=3)$out
boxplot(Fatdata$HIP,ylab = "HIP")
HIP_index <- which(Fatdata$HIP %in% HIP.out)
HIP_index

THIGH.out <- boxplot.stats(Fatdata$THIGH, coef=3)$out
THIGH_index <- which(Fatdata$THIGH %in% THIGH.out)
THIGH_index

KNEE.out <- boxplot.stats(Fatdata$KNEE, coef=3)$out
KNEE_index <- which(Fatdata$KNEE %in% KNEE.out)
KNEE_index

ANKLE.out <- boxplot.stats(Fatdata$ANKLE, coef=3)$out
ANKLE_index <- which(Fatdata$ANKLE %in% ANKLE.out)
ANKLE_index

BICEPS.out <- boxplot.stats(Fatdata$BICEPS, coef=3)$out
BICEPS_index <- which(Fatdata$BICEPS %in% BICEPS.out)
BICEPS_index

FOREARM.out <- boxplot.stats(Fatdata$FOREARM, coef=3)$out
FOREARM_index <- which(Fatdata$FOREARM %in% FOREARM.out)
FOREARM_index

WRIST.out <- boxplot.stats(Fatdata$WRIST, coef=3)$out
WRIST_index <- which(Fatdata$WRIST %in% WRIST.out)
WRIST_index

rm_index<-c(WEIGHT_index,HEIGHT_index,ADIPOSITY_index,NECK_index,CHEST_index,ABDOMEN_index,
            HIP_index,THIGH_index,KNEE_index,ANKLE_index,BICEPS_index,FOREARM_index,WRIST_index)
        
rm_index

rm_i<-c(39,42,31,86)

Fatdata_new<-Fatdata[-c(39,42,31,86),]

data_tr<-Fatdata_new[1:200,]
nn<-nrow(Fatdata_new)
nn
n<-nrow(Fatdata)
n
dim(Fatdata_new)
data_te<-Fatdata_new[201:nn,]
dim(data_te)

Fat_lm<-lm(BODYFAT~AGE
              +WEIGHT+HEIGHT
              +ADIPOSITY
              +NECK+CHEST+ABDOMEN+HIP
              +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST, 
              data=Fatdata_new)
Fat_lm.tr<-lm(BODYFAT~AGE
           +WEIGHT+HEIGHT
           +ADIPOSITY
           +NECK+CHEST+ABDOMEN+HIP
      +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST, 
      data=data_tr)
pred_lm<-predict(Fat_lm.tr,data_te)
RMSE(pred = pred_lm, obs = data_te$BODYFAT)
summary(Fat_lm.tr)
compare <- cbind (actual=data_te$BODYFAT, pred_lm)
compare

X<-Fatdata_new[,4:17]
round(cor(X), 2) # Correlation Test
BFAT<-Fatdata_new[,2]

install.packages("psycho")
library(psycho)


install.packages("GGally")
library(GGally)
ggpairs(X)

install.packages("mctest")
library(mctest)
install.packages("tidyverse")
install.packages("car")
library(car)

#detect existence of multicollinearity
omcdiag(Fat_lm)
#detect individual collinearity
imcdiag(Fat_lm)

vif(Fat_lm)

install.packages("glmnet")
library(glmnet)


lambdas <- 10^seq(3, -2, by = -.1)

XX<-data.matrix(X)

Fat_ridge <- cv.glmnet(XX, BFAT, alpha = 0, family="gaussian",standardize=TRUE, type.measure="mse")
summary(Fat_ridge)
Fat_ridge
plot(Fat_ridge)

opt_lambda <- Fat_ridge$lambda.min
Fat_fit <- Fat_ridge$glmnet.fit
coef(Fat_ridge)
iii_r <- which(Fat_ridge$lambda == Fat_ridge$lambda.min)
mse.min_r <- Fat_ridge$cvm[iii_r]
mse.min_r


#lasso
Fat_lasso = cv.glmnet(XX, BFAT, alpha = 1,standardize=TRUE) # Fit lasso model on training data

plot(Fat_lasso)    # Draw plot of coefficients
opt_lambda_L <- Fat_lasso$lambda.min
log(opt_lambda_L)
Fat_fit_L <- Fat_lasso$glmnet.fit
coef(Fat_lasso)
iii_L <- which(Fat_lasso$lambda == Fat_lasso$lambda.min)
mse.min_L <- Fat_lasso$cvm[iii_L]
mse.min_L


plot(Fat_lasso, xvar = "dev", label = TRUE)


#elastic net regression

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 20, # # fold of cross validation
                           repeats=20,
                           search = "random",
                           verboseIter = TRUE)
# Train the model
Fat_elastic <- train(BODYFAT~AGE
                     +WEIGHT
                     +HEIGHT
                     +ADIPOSITY
                     +NECK+CHEST+ABDOMEN+HIP
                     +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,
                     data = data_tr,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 25,  #search 20 alpha values and 20 lambda values for each  
                     trControl = train_cont)


# Best tuning parameter
Fat_elastic
Fat_elastic$bestTune
coef(Fat_elastic$finalModel, Fat_elastic$bestTune$lambda)
best_alpha<-Fat_elastic$bestTune$alpha
best_lambda<-Fat_elastic$bestTune$lambda

XX_en<-as.matrix(X)
elastic_mod <- cv.glmnet(XX, BFAT, family = "gaussian", 
                      alpha = best_alpha)
coef(elastic_mod)
elastic_mod$cvm
iii_en <- which(elastic_mod$lambda == elastic_mod$lambda.min)
mse.min_en <- elastic_mod$cvm[iii_en]
mse.min_en