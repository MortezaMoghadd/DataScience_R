### Libraries
#The libraries we need are:
library(ggplot2)
library(GGally)
library(plotly)
library(dplyr)
library(reshape2)
library(tibble)
library(e1071) #skewness
library(Amelia) #missing plot
library(corrplot) #correlations
library(car) #plotmatrix for scatter plot matrix
library(caret)
library(MASS) #LDA
library(glmnet) #glmnet
library(rpart) #CART
library(klaR) #Naive Bayes
library(kernlab) #SVM
library(ipred) #Bagged CART (BAG)
library(randomForest)
library(pROC) #AUC
#load the dataset
dataset <- read.csv("charity.csv",header=TRUE, sep = ",", fill=TRUE) #used this for normalized column names
dataset1 <- dataset #used this for original column names
attach(dataset)
head(dataset, n=10)
#dimensions
dim(dataset)
#### Normalize column names/Rename Columns
#To make them more readable, we rename the columns.
names(dataset)[names(dataset)=="reg1"] <- "reg1_int"
names(dataset)[names(dataset)=="reg2"] <- "reg2_int"
names(dataset)[names(dataset)=="reg3"] <- "reg3_int"
names(dataset)[names(dataset)=="reg4"] <- "reg4_int"
names(dataset)[names(dataset)=="home"] <- "is_homeowner_int"
names(dataset)[names(dataset)=="chld"] <- "numofchildren_int"
names(dataset)[names(dataset)=="hinc"] <- "income_category_int"
names(dataset)[names(dataset)=="genf"] <- "gender_int"
names(dataset)[names(dataset)=="wrat"] <- "wealthrate_segment_int"
names(dataset)[names(dataset)=="avhv"] <- "avg_homevalue1000_int"
names(dataset)[names(dataset)=="incm"] <- "median_income1000_int"
names(dataset)[names(dataset)=="inca"] <- "avg_income1000_int"
names(dataset)[names(dataset)=="plow"] <- "lowincome_percent_int"
names(dataset)[names(dataset)=="npro"] <- "number_promo_received_int"
names(dataset)[names(dataset)=="tgif"] <- "total_gift_amount_int"
names(dataset)[names(dataset)=="lgif"] <- "largest_gift_amount_int"
names(dataset)[names(dataset)=="rgif"] <- "last_gift_amount"
names(dataset)[names(dataset)=="tdon"] <- "month_since_last_donation_int"
names(dataset)[names(dataset)=="tlag"] <- "month_first_secnd_donation_int"
names(dataset)[names(dataset)=="agif"] <- "avg_gift_amount_int"
names(dataset)[names(dataset)=="donr"] <- "isdonated_int"
names(dataset)[names(dataset)=="damt"] <- "donation_amount_prediction_int"
#data types
str(dataset)

### Set up data for analysis
# predictor transformations (do it later not for EDA)
dataset.t <- dataset
dataset1.t <- dataset1
dataset_tv <- dataset.t[dataset$part!="test",]
dataset1_tv <- dataset1.t[dataset1$part!="test",]
dim(dataset_tv) #6002 rows and 24 varaibles


###############################################
## Exploratory Data Analysis 
###############################################

#distribution of class variable
y <- dataset_tv$isdonated_int
cbind(freq=table(y), percentage=prop.table(table(y))*100)
summary(dataset_tv[,2:24])
### Standard Deviation
# calculate standard deviation for all attributes
sapply(dataset_tv[,2:22], sd)
#create a numeric data frame
dataset_numeric <- dataset_tv %>%
  dplyr::select(avg_homevalue1000_int,median_income1000_int,avg_income1000_int,lowincome_percent_int,number_promo_received_int,total_gift_amount_int,largest_gift_amount_int,last_gift_amount,month_since_last_donation_int,month_first_secnd_donation_int,avg_gift_amount_int)
dataset1_numeric <- dataset1_tv %>%
  dplyr::select(incm, inca, tgif, agif, tdon, avhv, tlag, npro, plow, lgif, rgif)
# calculate standard deviation for all attributes
sapply(dataset_numeric, sd)
# calculate standard deviation for numeric attributes
sapply(dataset_numeric, sd)
### Skewness
#calculate skewness for each variable
skew <- apply(dataset_numeric, 2, skewness)
# display skewness, larger/smaller deviations from 0 show more skew
print(skew)

### Univariate Visualization
# create histograms for each attribute
ggplot(melt(dataset_numeric),aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x")+
  geom_histogram()
# create  density plots
ggplot(melt(dataset_numeric),aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x")+
  geom_density()
# Box And Whisker Plots
ggplot(stack(dataset1_numeric), aes(x = ind, y = values)) +
  geom_boxplot()
# Bar Plot
# Create a categorical data frame
dataset_categorical <- dataset_tv %>%
  dplyr::select(reg1_int, reg2_int, reg3_int, reg4_int, is_homeowner_int, numofchildren_int,
                income_category_int, gender_int, wealthrate_segment_int,  isdonated_int)
ggplot(melt(dataset_categorical),aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x")+
  geom_bar()

#EDA Findings
summary(dataset_tv[,2:5]) # for regions
summary(dataset_tv[,6:9]) # for is_homeowner_int, numofchildren_int, income_category_int, gender
#number of children detailed plot
dataset_tv %>% count(chld = numofchildren_int, numofchld = factor(numofchildren_int),...) %>% 
  ungroup() %>%
  mutate(percentage = round(prop.table(n) * 100),0) %>% 
  ggplot(aes(x = numofchld, y = percentage )) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = "steelblue") + 
  geom_text(aes(y = percentage + 1.1,    # nudge above top of bar
                label = paste0(percentage, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 4)+theme_minimal()
# Income category detailed plot
dataset_tv %>% count(hinc = income_category_int, incomecat = factor(income_category_int)) %>% 
  ungroup() %>%
  mutate(percentage = round(prop.table(n) * 100),0) %>% 
  ggplot(aes(x = incomecat, y = percentage)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = "steelblue") + 
  geom_text(aes(y = percentage + 1.1,    # nudge above top of bar
                label = paste0(percentage, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 4)+theme_minimal()
#WRAT-wealth rate group details
dataset_tv %>% count(WRAT = wealthrate_segment_int, Segment = factor(wealthrate_segment_int)) %>% 
  ungroup() %>%    # drop if you want percentages per segment
  mutate(percentage = round(prop.table(n) * 100,1))
summary(dataset_tv[,11:13]) # for avg_homevalue1000_int,median_income1000_int, avg_income1000_int
# calculate standard deviation for avg_homevalue1000_int,median_income1000_int, avg_income1000_int
sapply(dataset_tv[,11:13], sd)
# Detailed histogram for avg_homevalue1000_int,median_income1000_int, avg_income1000_int
ggplot(melt(dataset_tv[,c(11:13)]),aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x")+
  geom_histogram(bins=20)
#month_since_last_donation_int, month_first_secnd_donation_int
summary(dataset_tv[,19:20])
# calculate standard deviation for month_since_last_donation_int, month_first_secnd_donation_int
sapply(dataset_tv[,19:20], sd)
#Dollar amount of gifts
summary(dataset_tv[,21:22])
# calculate standard deviation for all attributes
sapply(dataset_tv[,21:22], sd)

# create a missing map
missmap(dataset1, col=c("black", "grey"), legend=FALSE)

#### Multivariate Visualization
#Scatter Plot Matrix
# pairwise scatter plots of all attributes
pairs(dataset_tv[,2:22])
pairs(dataset_numeric) #scatter plots of of numeric variables
#better scatter plot on agif
par(mfrow=c(4,6))
for(i in 1:21){
  plot(dataset1_tv[,i], 
       dataset1_tv$agif, 
       main=names(dataset[i]), 
       ylab=names(dataset$agif), 
       xlab="", col='steelblue')
}
#better scatter plot on donr
par(mfrow=c(4,6))
for(i in 1:22){
  plot(dataset1_tv[,i], 
       dataset1_tv$donr, 
       main=names(dataset1[i]), 
       ylab=names(dataset1$donr), 
       xlab="", col='steelblue')
}
#Scatter plot Matrix By Class
par(mfrow=c(4,6))
for(i in 1:21){
  plot(dataset1_tv[,i], 
       dataset1_tv$agif, 
       main=names(dataset[i]), 
       ylab=names(dataset$agif), 
       xlab="", col=factor(dataset_tv$isdonated_int))
}
#better scatter plot on donr
par(mfrow=c(4,6))
for(i in 1:22){
  plot(dataset1_tv[,i], 
       dataset1_tv$donr, 
       main=names(dataset1[i]), 
       ylab=names(dataset1$donr), 
       xlab="", col=factor(dataset_tv$isdonated_int))
}
##Box Plots By Class
# create  box plot by class
x <- dataset_numeric
y <- factor(dataset_tv$isdonated_int)
featurePlot(x=x, y=y, plot="box")

## Correlations
#calculate a correlation matrix for numeric variables
correlations <- cor(dataset_numeric)
# create correlation plot
corrplot(correlations, method="circle")


##################################################
######Data Pre-Processing
##################################################

# Remove redundant variable Id
dataset <- dataset[,-1] #for the whole dataset
#dataset_train <- dataset_train[,-1] # for training
#dataset_validation <- dataset_validation[,-1] # for validation
#dataset_test <- dataset_test[,-1] # for test

# Remove the donation_amount_prediction_int for classification dataset
dataset.c <- dataset[,-22] #for classification we remove donation_amount_prediction_int and later after split we remove "part"
#dataset_train.c <- dataset_train[,-22:-23] # for training
#dataset_validation.c <- dataset_validation[,-22:-23] # for validation
#dataset_test.c <- dataset_test[,-22:-23] # for test

# Convert numeric variables to categorical variables
cols <- c("reg1_int", "reg2_int", "reg3_int", "reg4_int",
          "is_homeowner_int", "numofchildren_int", "income_category_int",
          "gender_int", "wealthrate_segment_int", "isdonated_int")
dataset.c[cols] <- lapply(dataset.c[cols], factor)
str(dataset.c)

###Standardization
# calculate the pre-process parameters from the dataset - preProcess() can pass categorical variables that's why we put the whole dataset
preprocessParams <- preProcess(dataset.c, method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, dataset.c)
# summarize the transformed dataset
summary(transformed)
# before transformation
sapply(dataset[,10:20], sd)
# after transformation
sapply(transformed[,10:20], sd)

###Box-Cox data transform
# calculate the pre-process parameters from the dataset - preProcess() can pass categorical variables that's why we put the whole dataset
preprocessParams.box <- preProcess(dataset.c, method=c("BoxCox"))
# summarize transform parameters
print(preprocessParams.box)
# transform the dataset using the parameters
transformed.box <- predict(preprocessParams.box, dataset.c)
# summarize the transformed dataset
summary(transformed.box)
# before transformation
sapply(dataset[,10:20], sd)
# after transformation
sapply(transformed.box[,10:20], sd)

#########################
###################### standard
#########################
#rename the valus from integer to class in the target variable
transformed$isdonated_int <- as.factor(ifelse(transformed$isdonated_int==1,'yes','no'))

#create a test/training/validation datasets
dataset_test.c <- transformed[transformed$part=="test",]
dim(dataset_test.c) #2007 rows and 22 varaibles
dataset_train.c <- transformed[transformed$part=="train",]
dim(dataset_train.c) #3984 rows and 22 columns
dataset_validation.c <- transformed[transformed$part=="valid",]
dim(dataset_validation.c) #2018 rows and 22 columns

#remove "part"
dataset_train.c <- dataset_train.c[,-22] # for training
dataset_validation.c <- dataset_validation.c[,-22] # for validation
dataset_test.c <- dataset_test.c[,-22] # for test

#calculate a correlation matrix for numeric variables on training
correlations.c <- cor(dataset_train.c[,10:20])
# create correlation plot
corrplot(correlations.c, method="circle")


## remove highly correlated variables
dataset_train.c_clean <- dataset_train.c %>%
  dplyr::select(-avg_homevalue1000_int, -median_income1000_int,-lowincome_percent_int,
         -total_gift_amount_int, -largest_gift_amount_int, -last_gift_amount)

#########################
##########################Box-Cox data transform
#########################

#create a test/training/validation datasets
dataset_test.c.box <- transformed.box[transformed.box$part=="test",]
dim(dataset_test.c.box) #2007 rows and 22 varaibles
dataset_train.c.box <- transformed.box[transformed.box$part=="train",]
dim(dataset_train.c.box) #3984 rows and 22 columns
dataset_validation.c.box <- transformed.box[transformed.box$part=="valid",]
dim(dataset_validation.c.box) #2018 rows and 22 columns

#remove "part"
dataset_train.c.box <- dataset_train.c.box[,-22] # for training
dataset_validation.c.box <- dataset_validation.c.box[,-22] # for validation
dataset_test.c.box <- dataset_test.c.box[,-22] # for test

#calculate a correlation matrix for numeric variables on training
correlations.c.box <- cor(dataset_train.c.box[,10:20])
# create correlation plot
corrplot(correlations.c.box, method="circle")


## remove highly correlated variables
dataset_train.c.box_clean <- dataset_train.c.box %>%
  dplyr::select(-avg_homevalue1000_int, -median_income1000_int,-lowincome_percent_int,
         -total_gift_amount_int, -largest_gift_amount_int, -last_gift_amount)


##################################################
######Evaluate Algorithms: Baseline
##################################################

#Baseline on standardized dataset
#distribution of class variable
y <- dataset_train.c_clean$isdonated_int
cbind(freq=table(y), percentage=prop.table(table(y))*100) # %50
summary(dataset_train.c_clean)
# generalize outcome and predictor varaibles
outcomeName <- 'isdonated_int'
predictorsName <- names(dataset_train.c_clean)[names(dataset_train.c_clean) != outcomeName]
#Prepare algorithm evaluation test harness.
# 10-fold cross-validation with 3 repeats
#create caret train control object to control the number of cross-validations performed
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE) #classProb tells caret that is the classification model
metric <- "Accuracy"

#Estimate the accuracy of a suite of machine learning algorithms
#LG
set.seed(7)
fit.glm <- train(isdonated_int~., data=dataset_train.c_clean, method="glm", metric=metric,
                 trControl=trainControl, na.action=na.omit)
#find out variable importance
summary(fit.glm)
# find out model detials
fit.glm
# LDA
set.seed(7)
fit.lda <- train(isdonated_int~., data=dataset_train.c_clean, method="lda", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# GLMNET
set.seed(7)
fit.glmnet <- train(isdonated_int~., data=dataset_train.c_clean, method="glmnet", metric=metric,
                    trControl=trainControl, na.action=na.omit)
# KNN
set.seed(7)
fit.knn <- train(isdonated_int~., data=dataset_train.c_clean, method="knn", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# CART
set.seed(7)
fit.cart <- train(isdonated_int~., data=dataset_train.c_clean, method="rpart", metric=metric,
                  trControl=trainControl, na.action=na.omit)
# Naive Bayes
set.seed(7)
fit.nb <- train(isdonated_int~., data=dataset_train.c_clean, method="nb", metric=metric, trControl=trainControl,
                na.action=na.omit)
# SVM
set.seed(7)
fit.svm <- train(isdonated_int~., data=dataset_train.c_clean, method="svmRadial", metric=metric,
                 trControl=trainControl, na.action=na.omit)


# Compare algorithms
results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn,
                          CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)
dotplot(results)

##################################################
######Evaluate Algorithms: Box-Cox data transform
##################################################

# 10-fold cross-validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

#Estimate the accuracy of a suite of machine learning algorithms
#LG
set.seed(7)
fit.glm <- train(isdonated_int~., data=dataset_train.c.box_clean, method="glm", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# LDA
set.seed(7)
fit.lda <- train(isdonated_int~., data=dataset_train.c.box_clean, method="lda", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# GLMNET
set.seed(7)
fit.glmnet <- train(isdonated_int~., data=dataset_train.c.box_clean, method="glmnet", metric=metric,
                    trControl=trainControl, na.action=na.omit)
# KNN
set.seed(7)
fit.knn <- train(isdonated_int~., data=dataset_train.c.box_clean, method="knn", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# CART
set.seed(7)
fit.cart <- train(isdonated_int~., data=dataset_train.c.box_clean, method="rpart", metric=metric,
                  trControl=trainControl, na.action=na.omit)
# Naive Bayes
set.seed(7)
fit.nb <- train(isdonated_int~., data=dataset_train.c.box_clean, method="nb", metric=metric, trControl=trainControl,
                na.action=na.omit)
# SVM
set.seed(7)
fit.svm <- train(isdonated_int~., data=dataset_train.c.box_clean, method="svmRadial", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# Compare algorithms
results.box <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn,
                          CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results.box)
dotplot(results.box)



##################################################
######Algorithm Tuning
##################################################

#To find the parameters of a model that can be tuned, you can use
modelLookup(model='glm')
modelLookup(model='svmRadial') #sigma + C
modelLookup(model='lda')
modelLookup(model='glmnet') # alpha + lambda

######SVM Tuning - Using tuneGrid
# 10-fold cross-validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
# add tuneGrid for SVM
fit.svm <- train(isdonated_int~., data=dataset_train.c_clean, method="svmRadial", metric=metric,
                 tuneGrid=grid,trControl=trainControl, na.action=na.omit)
print(fit.svm)
plot(fit.svm)

######glmnet Tuning - Using tuneLength
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
fit.glmnet <- train(isdonated_int~., data=dataset_train.c.box_clean, method="glmnet", metric=metric,
                    tuneLength=10,trControl=trainControl, na.action=na.omit)
print(fit.glmnet) #did not improve our model
plot(fit.glmnet)

##################################################
######Ensemble Algorithms 
##################################################

# 10-fold cross-validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# generalize outcome and predictor varaibles
outcomeName <- 'isdonated_int'
predictorsName <- names(dataset_train.c_clean)[names(dataset_train.c_clean) != outcomeName]

# Bagged CART
set.seed(7)
fit.treebag <- train(isdonated_int~., data=dataset_train.c_clean, method="treebag", metric=metric,
                     trControl=trainControl, na.action=na.omit)
# Random Forest
set.seed(7)
fit.rf <- train(isdonated_int~., data=dataset_train.c_clean, method="rf", metric=metric,
                trControl=trainControl, na.action=na.omit)
# Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(isdonated_int~., data=dataset_train.c_clean, method="gbm", metric=metric,
                 trControl=trainControl, verbose=FALSE, na.action=na.omit)
# C5.0
set.seed(7)
fit.c50 <- train(isdonated_int~., data=dataset_train.c_clean, method="C5.0", metric=metric,
                 trControl=trainControl, na.action=na.omit)
# Compare results
ensembleResults <- resamples(list(BAG=fit.treebag, RF=fit.rf, GBM=fit.gbm, C50=fit.c50))
summary(ensembleResults)
dotplot(ensembleResults)

##################################################
#############Validation
#########################
#########################
#############Finalize Model
##################################################
#get the name of all caret supported models
names(getModelInfo())

#########################
######LDA######
#########################
# create final standalone model using all training data
fit.lda <- train(isdonated_int~., data=dataset_train.c_clean, method="lda", metric=metric,
                 na.action=na.omit)
# class prediction
predictions <- predict(fit.lda, dataset_validation.c, type='raw')
head(predictions)
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy 0.8914
confusionMatrix(predictions, dataset_validation.c$isdonated_int) #use it with type='raw'
#Reference
#Prediction  no yes
#no  867  67
#yes 152 932

# probabilities
predictions <- predict(fit.lda, dataset_validation.c, type='prob')
head(predictions) #interpretation: for example the first row is %97 is 'yes'
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy NA

#for probabilities we can do AUC
auc <- roc(ifelse(dataset_validation.c[,outcomeName]=="yes",1,0), predictions[[2]]) #use the probability of yes in column 2
print(auc$auc) #Area under the curve: 0.9565

post.valid.lda1 <- predictions
head(post.valid.lda1)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1[[2]], decreasing=T)]-2) #our second column is the yes class
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1268.0 11804.5

cutoff.lda1 <- sort(post.valid.lda1[[2]], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1[[2]]>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#                  0 740  10
#                  1 279  989
# check n.mail.valid = 279+989 = 1268.0
# check profit = 14.5*981-2*1268.0 = 11804.5
(740+989)/2018#accuracy: .8567
error.lda1 <- round(mean(chat.valid.lda1!=c.valid),4)
error.lda1 #0.1432

#########################
######LG######
#########################
# create final standalone model using all training data
fit.glm <- train(isdonated_int~., data=dataset_train.c_clean, method="glm", metric=metric,
                 na.action=na.omit)
# class prediction
predictions <- predict(fit.glm, dataset_validation.c, type='raw')
head(predictions)
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy 0.8899
confusionMatrix(predictions, dataset_validation.c$isdonated_int) #use it with type='raw'
#Reference
#Prediction  no yes
#no  885  88
#yes 134 911

# probabilities
predictions <- predict(fit.glm, dataset_validation.c, type='prob')
head(predictions) #interpretation: for example the first row is %98 is 'yes'
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy NA

#for probabilities we can do AUC
auc <- roc(ifelse(dataset_validation.c[,outcomeName]=="yes",1,0), predictions[[2]]) #use the probability of yes in column 2
print(auc$auc) #Area under the curve: 0.9584

post.valid.log1 <- predictions
head(post.valid.log1)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1[[2]], decreasing=T)]-2) #our second column is the yes class
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1295 11823

cutoff.log1 <- sort(post.valid.log1[[2]], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1[[2]]>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 718   5
#              1 301 994
# check n.mail.valid = 301+994 = 1295
# check profit = 14.5*981-2*1291 = 11823
(718+994)/2018#accuracy: .8483
error.log1 <- round(mean(chat.valid.log1!=c.valid),4)
error.log1 #0.1516


#########################
######GLMNET######
#########################

# create final standalone model using all training data
fit.glmnet <- train(isdonated_int~., data=dataset_train.c_clean, method="glmnet", metric=metric,
                    tuneLength=10,na.action=na.omit)
# class prediction
predictions <- predict(fit.glmnet, dataset_validation.c, type='raw')
head(predictions)
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy 0.8880
confusionMatrix(predictions, dataset_validation.c$isdonated_int) #use it with type='raw'
#Reference
#Prediction  no yes
        #no  884  88
        #yes 135 911

# probabilities
predictions <- predict(fit.glmnet, dataset_validation.c, type='prob')
head(predictions) #interpretation: for example the first row is %97 is 'yes'
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy NA

#for probabilities we can do AUC
auc <- roc(ifelse(dataset_validation.c[,outcomeName]=="yes",1,0), predictions[[2]]) #use the probability of yes in column 2
print(auc$auc) #Area under the curve: 0.9587

post.valid.glmnet1 <- predictions
head(post.valid.glmnet1)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.glmnet1 <- cumsum(14.5*c.valid[order(post.valid.glmnet1[[2]], decreasing=T)]-2) #our second column is the yes class
plot(profit.glmnet1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.glmnet1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.glmnet1)) # report number of mailings and maximum profit
# 1284.0 11830.5

cutoff.glmnet1 <- sort(post.valid.glmnet1[[2]], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.glmnet1 <- ifelse(post.valid.glmnet1[[2]]>cutoff.glmnet1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.glmnet1, c.valid) # classification table
#               c.valid
#chat.valid.glmnet1   0   1
#                  0 728  6
#                 1  291  993
# check n.mail.valid = 291+993 = 1284
# check profit = 14.5*981-2*1274 = 11830
(728+993)/2018#accuracy: .8528
error.glmnet1 <- round(mean(chat.valid.glmnet1!=c.valid),4)
error.glmnet1 #0.1432


#########################
######SVM######
#########################

fit.svm <- train(isdonated_int~., data=dataset_train.c_clean, method="svmRadial", metric=metric,
          na.action=na.omit)
# class prediction
predictions <- predict(fit.svm, dataset_validation.c, type='raw')
head(predictions)
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy 0.8860
confusionMatrix(predictions, dataset_validation.c$isdonated_int) #use it with type='raw'
#Reference
#Prediction  no yes
#no  881  92
#yes 138 907

# probabilities
predictions <- predict(fit.svm, dataset_validation.c, type='prob')
head(predictions) #interpretation: for example the first row is %0.59 is 'yes'
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy NA

#for probabilities we can do AUC
auc <- roc(ifelse(dataset_validation.c[,outcomeName]=="yes",1,0), predictions[[2]]) #use the probability of yes in column 2
print(auc$auc) #Area under the curve: 0.9345

post.valid.svm1 <- predictions
head(post.valid.svm1)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.svm1 <- cumsum(14.5*c.valid[order(post.valid.svm1[[2]], decreasing=T)]-2) #our second column is the yes class
plot(profit.svm1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm1)) # report number of mailings and maximum profit
# 1304 11616.5

cutoff.svm1 <- sort(post.valid.svm1[[2]], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.svm1 <- ifelse(post.valid.svm1[[2]]>cutoff.svm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.svm1, c.valid) # classification table
#               c.valid
#chat.valid.svm1      0   1
#                  0 696  19
#                  1 323  980
# check n.mail.valid = 323+980 = 1304
# check profit = 14.5*981-2*1304 = 11616.5
(696+980)/2018#accuracy: .8305
error.svm1 <- round(mean(chat.valid.svm1!=c.valid),4)
error.svm1 #0.1695

#########################
######gbm######
#########################

# class prediction
# create final standalone model using all training data
fit.gbm <- train(isdonated_int~., data=dataset_train.c_clean, method="gbm", metric=metric,
                 verbose=FALSE, na.action=na.omit)
predictions <- predict(fit.gbm, dataset_validation.c, type='raw')
head(predictions)
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy 0.9033
confusionMatrix(predictions, dataset_validation.c$isdonated_int) #use it with type='raw'
#Reference
#Prediction  no yes
#no  884  60
#yes 135 939

# probabilities
predictions <- predict(fit.gbm, dataset_validation.c, type='prob')
head(predictions) #interpretation: for example the first row is %81 is 'yes'
outcomeName <- 'isdonated_int'
postResample(pred = predictions, obs = as.factor(dataset_validation.c[,outcomeName]))
# Accuracy NA

#for probabilities we can do AUC
auc <- roc(ifelse(dataset_validation.c[,outcomeName]=="yes",1,0), predictions[[2]]) #use the probability of yes in column 2
print(auc$auc) #Area under the curve: 0.9666

post.valid.gbm1 <- predictions
head(post.valid.gbm1)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.gbm1 <- cumsum(14.5*c.valid[order(post.valid.gbm1[[2]], decreasing=T)]-2) #our second column is the yes class
plot(profit.gbm1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gbm1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gbm1)) # report number of mailings and maximum profit
# 1208.0 11866.5

cutoff.gbm1 <- sort(post.valid.gbm1[[2]], decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gbm1 <- ifelse(post.valid.gbm1[[2]]>cutoff.gbm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gbm1, c.valid) # classification table
#               c.valid
#chat.valid.gbm1      0   1
#                  0 796  14
#                  1 223  985
# check n.mail.valid = 223+985 = 1208
# check profit = 14.5*981-2*1251 = 11866
(761+985)/2018#accuracy: .8652
error.gbm1 <- round(mean(chat.valid.gbm1!=c.valid),4)
error.gbm1 #0.1174


##################################################
############################## final model summary
##################################################
#### GBM is the winner!
#find out variable importance
summary(fit.gbm)
# find out model detials
fit.gbm
plot(varImp(fit.gbm,scale=F))
# FINAL RESULTS

# select fit.gbm since it has maximum profit in the validation sample

post.test <- predict(fit.gbm, dataset_test.c, type="prob") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.gbm1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test[[2]], decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test[[2]]>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1722  285





##############################################################
###################################REGRESSION#################
#############################################################

#########################
######Data Pre-Processing for Regression
#########################

# to predict damt when donr=1
dataset.y <- dataset %>%
  filter(isdonated_int=='1'|part == 'test')
# Remove the isdonated_int for regression dataset
dataset.y <- dataset.y[,-21] #for regression we remove isdonated_int and later after split we remove "part"


str(dataset.y)
dataset_tv.y <- dataset.y %>%
  filter(part!='test')

#calculate a correlation matrix for numeric variables on training - regression dataset
correlations.y <- cor(dataset_tv.y[,1:21])
# create correlation plot
corrplot(correlations.y, method="circle")

## remove highly correlated variables
#dataset.y <- dataset.y %>%
  dplyr::select(-avg_homevalue1000_int, -median_income1000_int,-lowincome_percent_int,
                -total_gift_amount_int, -largest_gift_amount_int, -last_gift_amount,-avg_gift_amount_int)
## convert numeric to factor
# Convert numeric variables to categorical variables
cols2 <- c("reg1_int", "reg2_int", "reg3_int", "reg4_int",
          "is_homeowner_int", "numofchildren_int", "income_category_int",
          "gender_int", "wealthrate_segment_int")
dataset.y[cols2] <- lapply(dataset.y[cols2], factor)
str(dataset.y)

#########################
#############################Standardization
#########################
#convert the target variable to factor so we don't transform it
dataset.y$donation_amount_prediction_int <- as.factor(dataset.y$donation_amount_prediction_int)
# calculate the pre-process parameters from the dataset - preProcess() can pass categorical variables that's why we put the whole dataset
preprocessParams.y <- preProcess(dataset.y, method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams.y)
# transform the dataset using the parameters
transformed.y <- predict(preprocessParams.y, dataset.y)
# summarize the transformed dataset
summary(transformed.y)

#back to integer format
dataset.y$donation_amount_prediction_int <- as.integer(dataset.y$donation_amount_prediction_int)
str(dataset.y)
transformed.y$donation_amount_prediction_int <- as.integer(transformed.y$donation_amount_prediction_int)
# before transformation
sapply(dataset.y[,10:20], sd)
# after transformation
sapply(transformed.y[,10:20], sd)

# create  density plots before transformation
ggplot(melt(dataset.y[,10:20]),aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x")+
  geom_density()
# create  density plots after transformation
ggplot(melt(transformed.y[,10:20]),aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x")+
  geom_density()

#create a test/training/validation datasets
dataset_test.y <- transformed.y[transformed.y$part=="test",]
dim(dataset_test.y) #2007 rows and 22 varaibles
dataset_train.y <- transformed.y[transformed.y$part=="train",]
dim(dataset_train.y) #1995 rows and 22 columns
dataset_validation.y <- transformed.y[transformed.y$part=="valid",]
dim(dataset_validation.y) #999 rows and 22 columns

#remove "part"
dataset_train.y <- dataset_train.y[,-22] # for training
dataset_validation.y <- dataset_validation.y[,-22] # for validation
dataset_test.y <- dataset_test.y[,-22] # for test


## remove highly correlated variables
dataset_train.y_clean <- dataset_train.y  %>%
  dplyr::select(-avg_homevalue1000_int, -median_income1000_int,-lowincome_percent_int,
                -number_promo_received_int, -largest_gift_amount_int, -last_gift_amount)

#########################
##########################Box-Cox data transform
#########################
###Box-Cox data transform
# calculate the pre-process parameters from the dataset - preProcess() can pass categorical variables that's why we put the whole dataset
#we don't want to transform the target variable so we convert it to a factor variable and then later convert it back to integer
dataset.y$donation_amount_prediction_int <- as.factor(dataset.y$donation_amount_prediction_int)
preprocessParams.box.y <- preProcess(dataset.y, method=c("BoxCox"))
# summarize transform parameters
print(preprocessParams.box.y)
# transform the dataset using the parameters
transformed.box.y <- predict(preprocessParams.box.y, dataset.y)
# summarize the transformed dataset
summary(transformed.box.y)
# before transformation
sapply(dataset.y[,10:20], sd)
# after transformation
sapply(transformed.box.y[,10:20], sd)
#back to integer format
dataset.y$donation_amount_prediction_int <- as.integer(dataset.y$donation_amount_prediction_int)
str(dataset.y)
transformed.box.y$donation_amount_prediction_int <- as.integer(transformed.box.y$donation_amount_prediction_int)

#create a test/training/validation datasets
dataset_test.y.box <- transformed.box.y[transformed.box.y$part=="test",]
dim(dataset_test.y.box) #2007 rows and 22 varaibles
dataset_train.y.box <- transformed.box.y[transformed.box.y$part=="train",]
dim(dataset_train.y.box) #1995 rows and 22 columns
dataset_validation.y.box <- transformed.box.y[transformed.box.y$part=="valid",]
dim(dataset_validation.y.box) #999 rows and 22 columns

#remove "part"
dataset_train.y.box <- dataset_train.y.box[,-22] # for training
dataset_validation.y.box <- dataset_validation.y.box[,-22] # for validation
dataset_test.y.box <- dataset_test.y.box[,-22] # for test


## remove highly correlated variables
dataset_train.y.box_clean <- dataset_train.y.box %>%
dplyr::select(-avg_homevalue1000_int, -median_income1000_int,-lowincome_percent_int,
              -number_promo_received_int, -largest_gift_amount_int, -last_gift_amount)


##################################################
######Evaluate Algorithms: Baseline
##################################################
#Baseline on standardized dataset

# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"

# LM
set.seed(7)
fit.lm.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="lm", metric=metric, trControl=trainControl)
# GLM
set.seed(7)
fit.glm.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="glm", trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="glmnet", metric=metric, trControl=trainControl)
# SVM
set.seed(7)
fit.svm.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="svmRadial", metric=metric, trControl=trainControl)
# KNN
set.seed(7)
fit.knn.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="knn", metric=metric, trControl=trainControl)

# Compare algorithms
results.y <- resamples(list(LM=fit.lm.y, GLM=fit.glm.y, GLMNET=fit.glmnet.y, SVM=fit.svm.y,
                           KNN=fit.knn.y))
summary(results.y)
dotplot(results.y)
# See below for saving chat.test into a file for submission

##################################################
######Evaluate Algorithms: Box-Cox Transform
##################################################
# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"

# LM
set.seed(7)
fit.lm.y.box <- train(donation_amount_prediction_int~., data=dataset_train.y.box_clean, method="lm", metric=metric, trControl=trainControl)
# GLM
set.seed(7)
fit.glm.y.box <- train(donation_amount_prediction_int~., data=dataset_train.y.box_clean, method="glm", trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet.y.box <- train(donation_amount_prediction_int~., data=dataset_train.y.box_clean, method="glmnet", metric=metric, trControl=trainControl)
# SVM
set.seed(7)
fit.svm.y.box <- train(donation_amount_prediction_int~., data=dataset_train.y.box_clean, method="svmRadial", metric=metric, trControl=trainControl)
# KNN
set.seed(7)
fit.knn.y.box <- train(donation_amount_prediction_int~., data=dataset_train.y.box_clean, method="knn", metric=metric, trControl=trainControl)

# Compare algorithms
results.y.box <- resamples(list(LM=fit.lm.y.box, GLM=fit.glm.y.box, GLMNET=fit.glmnet.y.box, SVM=fit.svm.y.box,
                            KNN=fit.knn.y.box))
summary(results.y.box)
dotplot(results.y.box)


##################################################
######Algorithm Tuning
##################################################

#To find the parameters of a model that can be tuned, you can use
modelLookup(model='glm')
modelLookup(model='svmRadial') #sigma + C
modelLookup(model='lda')
modelLookup(model='glmnet') # alpha + lambda
modelLookup(model='knn') # k

######SVM Tuning - Using tuneGrid
#what are the default parameteres
print(fit.svm.y) #c= 1, sigma=0.02416129
# 10-fold cross-validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"

set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
# add tuneGrid for SVM
fit.svm.y.tuned <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="svmRadial", metric=metric,
                 tuneGrid=grid,trControl=trainControl, na.action=na.omit)
print(fit.svm.y.tuned)
plot(fit.svm.y.tuned)

######glmnet Tuning - Using tuneLength
#what are the default parameteres
print(fit.glmnet.y)
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
set.seed(7)
fit.glmnet.y.tuned <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="glmnet", metric=metric,
                    tuneLength=10,trControl=trainControl, na.action=na.omit)
print(fit.glmnet.y.tuned) #did not improve our model
plot(fit.glmnet.y.tuned)

##################################################
######Ensemble Algorithms 
##################################################
# 10-fold cross-validation with 3 repeats

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
# Random Forest
set.seed(7)
fit.rf.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="rf", metric=metric, trControl=trainControl)
# Stochastic Gradient Boosting
set.seed(7)
fit.gbm.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="gbm", metric=metric, trControl=trainControl, verbose=FALSE)
# Cubist
set.seed(7)
fit.cubist.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="cubist", metric=metric, trControl=trainControl)
# Compare algorithms
ensembleResults.y <- resamples(list(RF=fit.rf.y, GBM=fit.gbm.y, CUBIST=fit.cubist.y))
summary(ensembleResults.y)
dotplot(ensembleResults.y)


##################################################
######Ensemble Algorithms Tuning
##################################################
#look at parameters used for Cubist
print(fit.cubist.y)

# Tune the Cubist algorithm
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
set.seed(7)
grid <- expand.grid(.committees=seq(15, 25, by=1), .neighbors=c(3, 5, 7))
fit.cubist.y.tunned <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="cubist", metric=metric,
                                 tuneGrid=grid, trControl=trainControl)
print(fit.cubist.y.tunned)
plot(fit.cubist.y.tunned)

##################################################
###############################Validation
##################################################
###########################
#############Finalize Model
###########################

#########################
##############Gradient Boosting Machines (GBM), RMSE = 1.3356
#########################
# create final standalone model using all training data
fit.gbm.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="gbm", verbose=FALSE)
pred.valid.ls1 <- predict(fit.gbm.y, newdata = dataset_validation.y) # validation predictions
mean((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2) # mean prediction error
#1.783914
rmse <- RMSE(pred.valid.ls1, dataset_validation.y$donation_amount_prediction_int)
rmse
# 1.335632
sd((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1698233

#########################
################Cubist boosting (CUBIST), RMSE = 1.307936
#########################
# create final standalone model using all training data
fit.cubist.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="cubist")
pred.valid.ls1 <- predict(fit.cubist.y, newdata = dataset_validation.y) # validation predictions
mean((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2) # mean prediction error
#1.710698
sd((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1653947
rmse <- RMSE(pred.valid.ls1, dataset_validation.y$donation_amount_prediction_int)
rmse
# 1.307936

#########################
#################Random Forest, bagging (RF), RMSE = 1.413377
#########################
# create final standalone model using all training data
fit.rf.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="rf")
pred.valid.ls1 <- predict(fit.rf.y, newdata = dataset_validation.y) # validation predictions
mean((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2) # mean prediction error
#1.997635
sd((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.180089
rmse <- RMSE(pred.valid.ls1, dataset_validation.y$donation_amount_prediction_int)
rmse
# 1.413377

#########################
##################Penalized Linear Regression (GLMNET), RMSE = 1.345253
#########################

# create final standalone model using all training data
fit.glmnet.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="glmnet")
pred.valid.ls1 <- predict(fit.glmnet.y, newdata = dataset_validation.y) # validation predictions
mean((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2) # mean prediction error
#1.809704
sd((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1614274
rmse <- RMSE(pred.valid.ls1, dataset_validation.y$donation_amount_prediction_int)
rmse
# 1.345253

#########################
###################Linear Regression (LM), RMSE= 1.3429
#########################
# create final standalone model using all training data
fit.lm.y<- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="lm")
pred.valid.ls1 <- predict(fit.lm.y, newdata = dataset_validation.y) # validation predictions
mean((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2) # mean prediction error
#1.803363
sd((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1611134
rmse <- RMSE(pred.valid.ls1, dataset_validation.y$donation_amount_prediction_int)
rmse
# 1.342893

#########################
####################Generalized Linear Regression (GLM), RMSE = 1.342893
#########################
# create final standalone model using all training data
fit.glm.y <- train(donation_amount_prediction_int~., data=dataset_train.y_clean, method="glm")
pred.valid.ls1 <- predict(fit.glm.y, newdata = dataset_validation.y) # validation predictions
mean((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2) # mean prediction error
#1.803363
sd((dataset_validation.y$donation_amount_prediction_int - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1611134
rmse <- RMSE(pred.valid.ls1, dataset_validation.y$donation_amount_prediction_int)
rmse
# 1.342893

##################################################
############################## final model summary
##################################################
#### Cubist boosting (CUBIST) is the winner!
#find out variable importance
summary(fit.cubist.y)
# find out model detials
fit.cubist.y
plot(varImp(fit.cubist.y,scale=F))


# select fit.cubist.y.box since it has minimum mean prediction error in the validation sample

yhat.test <- predict(fit.cubist.y, newdata = dataset_test.y) # test predictions
# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="Morteza_Moghaddam_FinalProject_Predict422.csv", row.names=FALSE) # use your initials for the file name
