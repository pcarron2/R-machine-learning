#set working directory
setwd("/Users/nugthug/Documents/cmpsci/348/Assignment/04")

#code to read in data (provided)
train <- read.csv("ann-train.csv")
colnames(train) <- c("age", "sex", "thyroxine", "maybe_thyroxine", "antihyroid", "sick", "pregnant", "thyroid_surgery", "l131_treatment", "hypothyroid", "hyperthyroid", "on_lithium", "has_goitre", "has_tumor", "hypopituitary", "psychological_symptoms", "TSH", "T3", "TT4", "T4U", "FTI", "class")
train$class <- as.factor(train$class)


test <- read.csv("ann-test.csv")
colnames(test) <- colnames(train)
test$class <- as.factor(test$class)

#install packages 

#install.packages('caret')
#install.packages('cvTools')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('rattle')

#load libraries
library(caret)
library(cvTools)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(rattle)
library(e1071)
#summary(train)
head(train)
#this was provided in the assignment
all.fit<-rpart(class~., data=train)
all.fit

#i read about rpart.plot at http://www.milbo.org/rpart-plot/prp.pdf
text(all.fit)
rpart.plot(all.fit)
summary(all.fit)

#This is from the titanic tutorial
#http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
fancyRpartPlot(all.fit)
#question 4 get accuracy
#predict function discussed in question. 
train.acc<-predict(all.fit,train,type='vector')
table(train$class,train.acc)
init.predict<-predict(all.fit,test,type='vector')
table(test$class,init.predict)

#training accuracy calculation
(3771-8)/3771

#test accuracy calculation
(3427-22)/3427

#question 5 crossvalidation. used link in assignment describing the caret package
#https://cran.r-project.org/web/packages/caret/caret.pdf

#I used the dplyr cheat sheet for below
train.and.test<-bind_rows(train,test)
#Used caret documentation 
#https://cran.r-project.org/web/packages/caret/caret.pdf
#http://topepo.github.io/caret/model-training-and-tuning.html#basic-parameter-tuning
#also used this link to help: https://rpubs.com/chengjiun/52658 Good tutorial.
fitcontrol<-trainControl(
            method = 'repeatedcv',
            number = 10,
            repeats= 10,
            savePredictions = TRUE)

q5.fit<-train(class~.,
              data=train.and.test,
              method="rpart",
              trControl=fitcontrol)

summary(q5.fit)



q5.best.fit<-predict(q5.fit$finalModel,test,type='vector')

rpart.plot(q5.fit$finalModel)
fancyRpartPlot(q5.fit$finalModel)
table(test$class,q5.best.fit)

rpartGrid<-expand.grid(
  cp=c(.001,.002,.003,.004,.005,.006,.007,.008,.009,
       .01,.011,.012,.013,.014,.015,.016,.017,.018,
       .019,.02,.021,.022,.023,.024)
)
#i used the modeltype parameters in the ?train documentation.
q6.fit<-train(class~.,
              data=train.and.test,
              method="rpart",
              trControl=fitcontrol,
              tuneGrid=rpartGrid
              )
saveRDS(q6.fit,'q6fit.RDS')
summary(q6.fit)
q6.fit



plot(q6.fit,
     main="Tree Cross-validation Accuracy Over \n Range of Complexity Parameter Values")
#using rpart2 to control maxdepth as described in this post:
#http://datascience.stackexchange.com/questions/6037/rpart-and-rpart2
#https://rpubs.com/chengjiun/52658 also used here
#http://topepo.github.io/caret/train-models-by-tag.html#Tree_Based_Model
rpartGrid2<-expand.grid(
  maxdepth=c(1,2,3,4,5,6,7,8,9,10)
)

q6.depth.fit<-train(class~.,
                    data=train.and.test,
                    method="rpart2",
                    trControl=fitcontrol,
                    tuneGrid=rpartGrid2
)
summary(q6.depth.fit)
plot(q6.depth.fit,
     main="Tree Cross-validation Accuracy Over \n Range of Tree Depths")
fancyRpartPlot(q6.depth.fit$finalModel)
