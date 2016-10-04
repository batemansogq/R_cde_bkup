################################################################
#  prepare text for modelling 
################################################################

emails <- read.csv("E://R/Analytic Course/text analytics/emails.csv", stringsAsFactors=FALSE)
# start the pre-processing
library(tm)
# create corpus
ce = Corpus(VectorSource(emails$text))
#lowercase
ce = tm_map(ce, tolower)
#clean up the corpus
ce = tm_map(ce, PlainTextDocument)
ce = tm_map(ce, removePunctuation)
ce = tm_map(ce, removeWords, stopwords("english"))
ce = tm_map(ce, stemDocument)

# put into matrix to remove under used terms
dtme= DocumentTermMatrix(ce)

# remove spare terms
spdtm = removeSparseTerms(dtme, 0.95)
#create the data frame
emailsSparse = as.data.frame(as.matrix(spdtm))
#set column names to useful r terms
colnames(emailsSparse) <- make.names(colnames(emailsSparse))


#add the dep variable
emailsSparse <- cbind(emailsSparse, "spam" = emails$spam)
# create ham df
ham <- subset(emailsSparse, spam==0)
which(colSums(ham)>=5000)
spam <- subset(emailsSparse, spam==1)
which(colSums(spam)>=1000)

# factor for modeling
emailsSparse$spam = as.factor(emailsSparse$spam)
#===========================================
# split out the data
#===========================================

library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl==TRUE)
test <- subset(emailsSparse, spl==FALSE)

#===========================================
# Make Region an ordered factor
#===========================================
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))



#===========================================
# Linear Models
#===========================================
#all features except Z
lm(x~ . -z, data=df)
#drop the incept term
lm(x~ 0 + y + z, data=df)

# function to remove intercept term, modelling the seasonal trend in time series
dairy.seasons <- function(df, col = 'Milk.Prod'){
  df$y = df[, col]
  fit = lm(y ~ 0 + Month, data = df)
  predict(fit, newdata = df)
}

#use functions
lm(x~ y + I(y^2) + z + I(z+2), data=df)
#interaction term
lm(x~ y + z + y:z, data=df)

#===========================================
# Generalized Linear Modelsm - logestic regression
#===========================================
mod <- glm(GenSolarBinary ~ GenHydro+GenSolar, data = train, family=binomial)
# Predictions on the test set
predictTest = predict(mod, type="response", newdata=test)
# Confusion matrix with threshold of 0.5
table(test$GenSolarBinary, predictTest > 0.5)
# Accuracy
sum(diag(table(test$GenSolarBinary, predictTest > 0.5)))/nrow(test)
# find the AUC value 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)



#===========================================
# CART model
# ======================================
# required libraries
library(rpart)
library(rpart.plot)
# CART
emailCART = rpart(responsive~., data=train, method="class")

prp(emailCART)
# Make predictions on the test set
pred = predict(emailCART, newdata=test)
# set the predict label
pred.prob = pred[,2]
# Compute accuracy
table(test$responsive, pred.prob >= 0.5)
# create a ROC curve
library(ROCR)
# use the ROC predcition 
predROCR = prediction(pred.prob, test$responsive)
# calc the curve
perfROCR = performance(predROCR, "tpr", "fpr")
# plot the curve with the colour side bart
plot(perfROCR, colorize=TRUE)
# Compute AUC figure
performance(predROCR, "auc")@y.values


# ======================================
#install.packages("randomForest")
# ======================================
library(randomForest)
# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
# Random forest
StevensForest = randomForest(Reverse ~ Circuit + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
# confusion matrix
table(Test$Reverse, PredictForest)

#===========================================
# cross-validation then predict
# ======================================
# Install cross-validation packages
library(caret)
library(e1071)

# Define cross-validation experiment
set.seed(201)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 

# Perform the cross validation, get the optimal cp value
train(RaisedFedFunds ~ PreviousRate + Streak, 
      data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
fedCART = rpart(RaisedFedFunds ~ PreviousRate + Streak, 
                data = training, method="class", cp = 0.016)
#plot the tree
prp(fedCART)
# Make predictions
PredictCART = predict(fedCART, newdata = testing, type = "class")
# confusion matrix
table(testing$RaisedFedFunds, PredictCART)

#===========================================
# heirarchical clustering
# ======================================
#transform to a matrix for the distance calc
mdk <- as.matrix(dk)
#compute the distances
dmdk <- dist(mdk, method = "euclidean")
# Hierarchical clustering
cdk = hclust(dmdk, method="ward.D")
# Plot the dendrogram
plot(cdk)


#===========================================
# cluster then predict
# ======================================
train.limited <- train[,c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")]
test.limited <- test[,c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")]
library(caret)
# preprocess the data for the normalisation
trainpreproc = preProcess(train.limited)
testpreproc = preProcess(test.limited)
# normalise
train.norm = predict(trainpreproc, train.limited)
test.norm = predict(testpreproc, test.limited)
#transform to a matrix for the distance calc
mtrain <- as.matrix(train.norm)
mtest <- as.matrix(test.norm)
# compute the KMeans
set.seed(144)
KMtrain = kmeans(mtrain, centers = 2, iter.max = 1000)
KMtest = kmeans(mtest, centers = 2, iter.max = 1000)
# Extract clusters
library(flexclust)
kmtrain.kcca = as.kcca(KMtrain, train.norm)
kmtest.kcca = as.kcca(KMtest, test.norm)
clusterTrain = predict(kmtrain.kcca)
clusterTest = predict(kmtest.kcca)
#split out the data into cluster
train1 <- subset(train , clusterTrain==1)
train2 <- subset(train , clusterTrain==2)
test1 <- subset(test , clusterTest==1)
test2 <- subset(test , clusterTest==2)
# model the 1st set
mod1 <- glm(GenSolarBinary ~ GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, 
            data = train1, family=binomial)
# Predictions on the test set
pdTestmd1 = predict(mod1, type="response", newdata=test1)
#accuracy of model 1
sum(diag(table(test1$GenSolarBinary, pdTestmd1 > 0.5)))/nrow(test1)
# model the 2nd set
mod2 <- glm(GenSolarBinary ~ GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, 
            data = train2, family=binomial)
# Predictions on the test set
pdTestmd2 = predict(mod2, type="response", newdata=test2)
# accuracy of model 2
sum(diag(table(test2$GenSolarBinary, pdTestmd2 > 0.5)))/nrow(test2)

# the overall test-set accuracy of the cluster-then-predict approach
AllPredictions <- c(pdTestmd2, pdTestmd1)
AllOutcomes <- c(test2$GenSolarBinary, test1$GenSolarBinary)
# find the accuracy of the combined approach, a slight improvement on the baseline
sum(diag(table(AllOutcomes,AllPredictions>0.5 )))/sum(table(AllOutcomes,AllPredictions>0.5 ))

#===================================================================
# training time series
#===================================================================
# time series package
install.packages("zoo")
library(zoo)
# create the lag
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)
# plot result
plot(log(ILILag2), log(FluTrain$ILI)) 
# model
FluTrend2 <- lm(  log(ILI)~Queries+log(ILILag2), data=FluTrain)
#predcit result 
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
# calc the RMSE
sqrt(mean((PredTest2-FluTest$ILI)^2))


