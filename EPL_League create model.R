#############################################################
# create the models
############################################################# 

# load packages 
x <- c("dplyr", "randomForest", "zoo", "caTools", "DataCombine", "caret", "ggplot2")
lapply(x, require, character.only=TRUE)

# read in source data
setwd("E:/R/Football")
df_model <- read.csv2( file="df_model.csv", header = TRUE, sep=",",  stringsAsFactors = FALSE)

#update betting col to numeric
df_model[,c(13:21)] <- lapply(df_model[,13:21],as.numeric)

# remove NA for random forest
df_model<- subset(df_model, !is.na(Lg_F3_HST))

#remove different betting
#df_model<- df_model[,c(-16:-21)]

# graph out
ggplot(df_model,aes(Lg_F3_FTHG, Lg_F3_HTAG)) + 
  geom_point(aes(shape = factor(FTR), colour=factor(FTR)))
# multi coloured pillers
ggplot(df_model,aes(tab_grp, Rd, colour=FTR))   + 
  geom_jitter(position=position_jitter(0.5))

#find correlation between elements
cor(df_model[,c(13, 15:21)], use="complete.obs", method="kendall")
#remove char
df_model_tst <- (df_model[,c(-3, -11, -12, -34, -77)])
#caret to find highly correlated features
cor_mod <- cor(df_model_tst)
cor_out <- findCorrelation(cor_mod, cutoff = 0.85)
names(df_model_tst)[cor_out]

#suggested pred columns
df_model_lim <- df_model %>% select(FTR,	B365H,	B365A, B365D,	
                                    wn, dw, ls, df, lp, tab_grp,
                                    Opp_wn, Opp_dw, Opp_ls, Opp_df, Opp_lp, Opp_tab_grp,
                                    Lg_F3_HST, Lg_F3_AS, Lg_F3_FTHG,
                                    Opp_Lg_F3_HS, Opp_Lg_F3_AS,Opp_Lg_F3_FTHG
                                    )


#split the test & train
set.seed(123)
spl <- sample.split(df_model_lim$FTR, SplitRatio = 0.7)
train <- subset(df_model, spl==TRUE)
test <- subset(df_model, spl==FALSE)


#===========================================
# CART model
# ======================================
# required libraries
library(rpart)
library(rpart.plot)
# CART
FCART = rpart(FTR~., data=train, method="class")

# Make predictions on the test set
predCT = predict(FCART, newdata=test)
# set the predict label
pred.prob = predCT[,2]
# Compute accuracy
table(test$FTR, pred.prob >= 0.5)

plot(FCART)

# ======================================
#install.packages("randomForest")
# ======================================

# Convert outcome to factor
train$FTR = as.factor(train$FTR)
test$FTR = as.factor(test$FTR)
# Random forest
RmF = randomForest(FTR ~ ., data = train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(RmF, newdata = test)
# confusion matrix
table(test$FTR, PredictForest)

#===========================================
# cross-validation then predict
# ======================================
# Install cross-validation packages
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

# Define cross-validation experiment
set.seed(201)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 

# Perform the cross validation, get the optimal cp value
train(FTR ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

eplLDA <- train(FTR~., data=train, method="lda")
plot(eplLDA$finalModel)
text(eplLDA$finalModel)

# Create a new CART model
fedCART = rpart(RaisedFedFunds ~ PreviousRate + Streak, 
                data = training, method="class", cp = 0.016)
#plot the tree
prp(fedCART)
# Make predictions
PredictCART = predict(fedCART, newdata = testing, type = "class")
# confusion matrix
table(testing$RaisedFedFunds, PredictCART)


