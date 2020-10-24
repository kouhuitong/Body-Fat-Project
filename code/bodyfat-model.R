rm(list = ls()) # Initialization

# Import pachages
if (!require("ggplot2")) {
  install.packages("ggplot2")
  stopifnot(require("ggplot2"))
}
if (!require("Metrics")) {
  install.packages("Metrics")
  stopifnot(require("Metrics"))
}
if (!require("caret")) {
  install.packages("caret")
  stopifnot(require("caret"))
}
if (!require("psycho")) {
  install.packages("psycho")
  stopifnot(require("psycho"))
}
if (!require("GGally")) {
  install.packages("GGally")
  stopifnot(require("GGally"))
}
if (!require("mctest")) {
  install.packages("mctest")
  stopifnot(require("mctest"))
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  stopifnot(require("tidyverse"))
}
if (!require("car")) {
  install.packages("car")
  stopifnot(require("car"))
}
if (!require("glmnet")) {
  install.packages("glmnet")
  stopifnot(require("glmnet"))
}

# Read the data
Fatdata<-read.csv(file = "data/BodyFat.csv",header=TRUE)

str(Fatdata)
summary(Fatdata) #A brief summary statistic of all the variables in the data


# Data Cleaning
Xvar<-c("AGE","WEIGHT","HEIGHT","ADIPOSITY","NECK","CHEST","ABDOMEN","HIP","THIGH","KNEE",
        "ANKLE","BICEPS","FOREARM","WRIST")

boxdata<-Fatdata[Xvar]
par(mar=c(1,1,1,1))
boxplot(boxdata)

#set the length of the whiskers as 3 multiplying the Inter-quartile range.
WEIGHT.out <- boxplot.stats(Fatdata$WEIGHT, coef=3)$out
WEIGHT_index <- which(Fatdata$WEIGHT %in% WEIGHT.out)

HEIGHT.out <- boxplot.stats(Fatdata$HEIGHT, coef=3)$out
HEIGHT_index <- which(Fatdata$HEIGHT %in% HEIGHT.out)

ADIPOSITY.out <- boxplot.stats(Fatdata$ADIPOSITY, coef=3)$out
ADIPOSITY_index <- which(Fatdata$ADIPOSITY %in% ADIPOSITY.out)

NECK.out <- boxplot.stats(Fatdata$NECK, coef=3)$out
NECK_index <- which(Fatdata$NECK %in% NECK.out)

CHEST.out <- boxplot.stats(Fatdata$CHEST, coef=3)$out
CHEST_index <- which(Fatdata$CHEST %in% CHEST.out)

ABDOMEN.out <- boxplot.stats(Fatdata$ABDOMEN, coef=3)$out
ABDOMEN_index <- which(Fatdata$ABDOMEN %in% ABDOMEN.out)

HIP.out <- boxplot.stats(Fatdata$HIP, coef=3)$out
HIP_index <- which(Fatdata$HIP %in% HIP.out)

THIGH.out <- boxplot.stats(Fatdata$THIGH, coef=3)$out
THIGH_index <- which(Fatdata$THIGH %in% THIGH.out)

KNEE.out <- boxplot.stats(Fatdata$KNEE, coef=3)$out
KNEE_index <- which(Fatdata$KNEE %in% KNEE.out)

ANKLE.out <- boxplot.stats(Fatdata$ANKLE, coef=3)$out
ANKLE_index <- which(Fatdata$ANKLE %in% ANKLE.out)

BICEPS.out <- boxplot.stats(Fatdata$BICEPS, coef=3)$out
BICEPS_index <- which(Fatdata$BICEPS %in% BICEPS.out)

FOREARM.out <- boxplot.stats(Fatdata$FOREARM, coef=3)$out
FOREARM_index <- which(Fatdata$FOREARM %in% FOREARM.out)

WRIST.out <- boxplot.stats(Fatdata$WRIST, coef=3)$out
WRIST_index <- which(Fatdata$WRIST %in% WRIST.out)

rm_index<-unique(c(WEIGHT_index,HEIGHT_index,ADIPOSITY_index,NECK_index,CHEST_index,ABDOMEN_index,
                   HIP_index,THIGH_index,KNEE_index,ANKLE_index,BICEPS_index,FOREARM_index,WRIST_index))

#remove extreme outliers
Fatdata_new<-Fatdata[-rm_index,]

nn<-nrow(Fatdata_new)
n<-nrow(Fatdata)

X<-Fatdata_new[,4:17]
round(cor(X), 2) # Correlation Test
BFAT<-Fatdata_new[,2]

Fat_lm<-lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY
              +NECK+CHEST+ABDOMEN+HIP
              +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST, 
              data=Fatdata_new)
# Detect existence of overall multicollinearity
omcdiag(Fat_lm)
# Detect individual collinearity
imcdiag(Fat_lm)
#variance inflation factor for assessing multicollinearity
vif(Fat_lm)

# All potential lambda values
lambdas <- 10^seq(3, -2, by = -.1)

XX<-data.matrix(X)

# Ridge Regression
Fat_ridge <- cv.glmnet(XX, BFAT, alpha = 0, family="gaussian",standardize=TRUE, type.measure="mse")
summary(Fat_ridge)
Fat_ridge
#plot(Fat_ridge)

opt_lambda <- Fat_ridge$lambda.min
Fat_fit <- Fat_ridge$glmnet.fit
coef(Fat_ridge)
iii_r <- which(Fat_ridge$lambda == Fat_ridge$lambda.min)
mse.min_r <- Fat_ridge$cvm[iii_r]
mse.min_r


# Lasso Regression
Fat_lasso = cv.glmnet(XX, BFAT, alpha = 1,standardize=TRUE) # Fit lasso model on training data

#plot(Fat_lasso)    # Draw plot of coefficients
opt_lambda_L <- Fat_lasso$lambda.min
log(opt_lambda_L)
Fat_fit_L <- Fat_lasso$glmnet.fit
coef(Fat_lasso)
iii_L <- which(Fat_lasso$lambda == Fat_lasso$lambda.min)
mse.min_L <- Fat_lasso$cvm[iii_L]
mse.min_L
#plot(Fat_lasso, xvar = "dev", label = TRUE)


# Elastic Net Regression

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 20, # # fold of cross validation
                           repeats=20,
                           search = "random",
                           verboseIter = TRUE)
# Train the model
set.seed(123)
Fat_elastic <- train(BODYFAT~AGE
                     +WEIGHT
                     +HEIGHT
                     +ADIPOSITY
                     +NECK+CHEST+ABDOMEN+HIP
                     +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,
                     data = Fatdata_new,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 20,  #search 20 alpha values and 20 lambda values for each  
                     trControl = train_cont)


# Best tuning parameter
Fat_elastic$bestTune
best_alpha<-Fat_elastic$bestTune$alpha
best_lambda<-Fat_elastic$bestTune$lambda

XX_en<-as.matrix(X)
elastic_mod <- cv.glmnet(XX, BFAT, family = "gaussian", standardize=TRUE,
                         alpha = best_alpha)
coef(elastic_mod)
elastic_mod$cvm
iii_en <- which(elastic_mod$lambda == elastic_mod$lambda.min)
mse.min_en <- elastic_mod$cvm[iii_en]
mse.min_en

elastic_pred <- predict(elastic_mod, XX, nfolds=10)
data.frame(
  RMSE = RMSE(elastic_pred, BFAT),
  Rsquare = R2(elastic_pred, BFAT)
)

# Get fitted values and residuals
BFAT_fitted <- as.vector(predict(elastic_mod, newx = XX))
resid_en <- BFAT_fitted - Fatdata_new$BODYFAT
resid_std_en <- (resid_en - mean(resid_en)) / sd(resid_en)

# Standardized Residual Plot
par(mfrow = c(1,2))
plot(x = BFAT_fitted, y = resid_std_en,pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

# Normal Q-Q Plot of the Residuals
qqnorm(resid_std_en,pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
