## My Results about Decision Trees

load('~/Downloads/titanic_train.Rdata')
data = titanic.train[,1:6]

library('caTools')
library('rpart')

# Repeats 1000 times the process of creating a decision tree with the simplest 
# parameters configured
set.seed(1)
ccr_train = ccr_test = rep(0, 1000) 
for (i in 1:1000){
  
  # We divide the data for trainig and testing
  partition = sample.split(data$Survived, SplitRatio = 0.75)
  data_train = data[partition == TRUE,]
  data_test = data[partition == FALSE,]
  
  # We train our desicion tree
  tree = rpart(formula = Survived~., data = data_train, method = 'class')
  
  # We use our tree to predict the same training set
  pred_train = predict(tree, newdata = data_train[,-1], type = 'class')
  ccr_train[i] = sum(pred_train == data_train$Survived)/length(pred_train) # Saves the accuracy in the ccr_train vector
  
  # We use the tree to predict the test set
  pred_test = predict(tree, newdata = data_test[,-1], type = 'class')
  ccr_test[i] = sum(pred_test == data_test$Survived)/length(pred_test)# Saves the accuracy in the vector ccr_test
}

# We check the results
mean(ccr_train) # Aprox 0.852
mean(ccr_test) # Aprox 0.816
(mean(ccr_train) + mean(ccr_test))/2 # Aprox 0.834


# We are going to save the results in a data set in order to analize them later
train = cbind.data.frame(rep('train', length(ccr_train)), ccr_train)
colnames(train) = c('set', 'accuracy')
test = cbind.data.frame(rep('test', length(ccr_test)), ccr_test) 
colnames(test) = c('set', 'accuracy')

# We join the test and train sets
results1 = rbind.data.frame(train, test)

# We label the data with the name of the algorithm
results1 = cbind.data.frame(results1, rep(c('Decision tree'), 2000))
colnames(results1) = c('set', 'accuracy', 'algorithm')



## My Results about kNN

load('~/Downloads/titanic_train.Rdata')

# We prepare the data
data = titanic.train[, 1:6]

class = data[,1] # The labels for classifying (data$Survived)
X=data[,2:6] # The variables we are going to use for training 

# First of all, we need to convert all the variables of X to numeric
levels(X$Sex) = c(0,1) # We replace the factors female and male in X$Sex by 0 and 1
# we convert all the variables of X as numeric
for (i in 1:4){
  X[,i] = as.numeric(as.character(X[,i])) 
}
X = scale(X) # We scale the data

# Now we are ready to work with our data
set.seed(1)
library(class) 
library(caTools)
# We are going to repeat the classification 1000 times
# In each time we are going to save the correct classification rate (accuracy) of the trainig set
# and the correct classification rate of the test set in the rows of the vectors ccr_train
# and ccr test respectively
ccr_train = ccr_test = rep(0, 1000) # Creates the correct classification rate vectors for training and testing
for (i in 1:1000){
  partition=sample.split(class,SplitRatio=0.75) # Creates a boolean vector of the length of class
  
  # The training set and its training class are going to be the rows where partition == TRUE
  tr_set=X[partition==TRUE,] 
  tr_class = class[partition == TRUE]
  
  # The test set and its test class are going to be the rows where partition == FALSE
  tst_set=X[partition==FALSE,]
  tst_class = class[partition == FALSE]
  
  # We get our prediction with our test set
  prediction_test=knn(train=tr_set, 
                      cl=tr_class, 
                      test=tst_set, 
                      k=5) 
  
  table_test = table(prediction_test,tst_class) # Creates the confussion matrix with our test set
  ccr_test[i] = sum(diag(table_test))/sum(table_test) # Saves the current accuracy of the prediction of the test set in the current row of ccr_test
  
  # We get our prediction with our train set
  prediction_train = knn(train = tr_set, 
                         cl = tr_class,
                         test = tr_set,
                         k = 5)
  
  table_train = table(prediction_train, tr_class) # Creates the confussion matrix with our training set
  ccr_train[i] = sum(diag(table_train))/sum(table_train) # Saves the current accuracy of the prediction of the training set in the current row of  ccr_train
}

#We check our results

#Average of accuray in our train set
mean(ccr_train) # aprox 0.855

#Average of accuracy in our test set
mean(ccr_test) # aprox 0.813

#Average of the averages of accuracy
(mean(ccr_train) + mean(ccr_test)) / 2 # aprox 0.835


# We are going to save the results in a data set
results3 = NULL

# We prepare the results giving them a the labels of train and test
train = cbind.data.frame(rep('train', length(ccr_train)), ccr_train)
colnames(train) = c('set', 'accuracy')
test = cbind.data.frame(rep('test', length(ccr_test)), ccr_test) 
colnames(test) = c('set', 'accuracy')

# We join the train and the test's results
results3 = rbind(train, test)

# Finally we label the results with the name of the algorithm they were gotten from
results3 = cbind(results3, rep('Knn', 2000))
colnames(results3) = c('set', 'accuracy', 'algorithm')




## My Results about Naive Bayes

load('~/Downloads/titanic_train.Rdata')
data = titanic.train[,1:6]

library(e1071)
library(caTools)
set.seed(1)

ccr_train = ccr_test = rep(0, 1000)
for (i in 1:1000) {
  partition = sample.split(data$Survived, SplitRatio = 0.75)
  
  data_train = data[partition == TRUE,]
  data_test = data[partition == FALSE,]
  
  nv = naiveBayes(formula = Survived~., data = data_train)
  
  pred_train = predict(nv, newdata = data_train[,-1])
  ccr_train[i] = sum(pred_train == data_train[,1])/length(pred_train)
  
  pred_test = predict(nv, newdata = data_test[,-1])
  ccr_test[i] = sum(pred_test == data_test[,1])/length(pred_test)
}

mean(ccr_train) # Aprox 0.799
mean(ccr_test) # Aprox 0.79
(mean(ccr_train) + mean(ccr_test)) / 2 # Aprox 0.795

results5 = NULL
train = cbind.data.frame(rep('train', 1000), ccr_train)
colnames(train) = c('set', 'accuracy')
test = cbind.data.frame(rep('test', 1000), ccr_test)
colnames(test) = colnames(train)
results5 = rbind.data.frame(train, test)
results5 = cbind(results5, rep('Naive Bayes', 2000))
colnames(results5) = c('set', 'accuracy', 'algorithm')




## My Results about Random Forest

setwd('~/Downloads')
load('titanic_train.Rdata')

data = titanic.train[,1:6]

library('caTools')
library('randomForest')

# We will repeat the process 1000 times in order to get a better accuracy while predicting
set.seed(1)
ccr_train= ccr_test = rep(0, 1000)
for (i in 1:1000){
  
  # We split the data for training and for testing
  partition = sample.split(data$Survived, SplitRatio = 0.75)
  data_train = data[partition == TRUE,]
  data_test = data[partition == FALSE,]
  
  # We train our random forest with our taining set
  forest = randomForest(formula = Survived~., data = data_train, ntree = 5 )    
  
  # We use the algorithm to predict the training set
  pred_train = predict(forest, newdata = data_train[,-1], type = 'class')
  ccr_train[i] = sum(pred_train == data_train$Survived)/length(pred_train) # Saves the accuracy in ccr_train
  
  # We predict our test set
  pred_test = predict(forest, newdata = data_test[,-1], type = 'class')
  ccr_test[i] = sum(pred_test == data_test$Survived)/length(pred_test) # Saves the accuracy in ccr_test
}

# We check our results
mean(ccr_train) # aprox  0.89 
mean(ccr_test) # aprox 0.80
mean(c(mean(ccr_train),mean(ccr_test))) # aprox 0.85

# We are going to save the results in a data set in order to analize them later
results2 = NULL

# We prepare the results giving them a the labels of train and test
train = cbind.data.frame(rep('train', length(ccr_train)), ccr_train)
colnames(train) = c('set', 'accuracy')
test = cbind.data.frame(rep('test', length(ccr_test)), ccr_test) 
colnames(test) = c('set', 'accuracy')

# We join the train and test sets
results2 = rbind.data.frame(train, test)

# We label the results with the name of the algorithm they belong to
results2 = cbind.data.frame(results2, rep('Random Forest', 2000))
colnames(results2) = c('set', 'accuracy', 'algorithm')




## My Results about SVM

load('~/Downloads/titanic_train.Rdata')
data = titanic.train[, 1:9]
data$Ticket = NULL
class = data[,1] # The labels for classifying (data$Survived)
X=data[,2:6] # The variables we are going to use for training 

# First of all we need to convert all the variables of X to numeric
levels(X$Sex) = c(0,1) # We replace the factors female and male in X$Sex by 0 and 1
# we convert all the variables of X as numeric
for (i in 1:2){ # The other columns already are numeric
  X[,i] = as.numeric(as.character(X[,i])) 
}
#X = scale(X) # We scale the data

library(caTools)
library(e1071)

ccr_train = ccr_test = rep(0, 1000)
for (i in 1:1000){
  
  partition=sample.split(class,SplitRatio=0.75) 
  train_set=X[partition==TRUE,]
  train_class = class[partition == TRUE]
  test_set=X[partition==FALSE,]
  test_class = class[partition == FALSE]
  
  classifier=svm(x = train_set , y = train_class,
                 type='C-classification', kernel='radial') 
  
  predict_train = predict(classifier, newdata = train_set)
  ccr_train[i] = sum(predict_train == train_class)/length(train_class)
  
  predict_test=predict(classifier, newdata=test_set) 
  ccr_test[i] = sum(predict_test == test_class)/length(test_class)
}

# Saving the results
results4 = NULL
train = cbind.data.frame(rep('train', length(ccr_train)), ccr_train)
colnames(train) = c('set', 'accuracy')
test = cbind.data.frame(rep('test', length(ccr_test)), ccr_test) 
colnames(test) = c('set', 'accuracy')
results4 = rbind.data.frame(train, test)
results4 = cbind.data.frame(results4, rep('Support Vector Machine', 2000))
colnames(results4) = c('set', 'accuracy', 'algorithm')



## My Classifiers
library(ggplot2)

results = NULL
results = rbind.data.frame(results1, results2, results3, results4, results5)
ggplot(results) +
    aes(x = algorithm, y = accuracy, fill = algorithm) +
    geom_boxplot()  +
    facet_grid(~set) +
    theme(text = element_text(angle = 90))

