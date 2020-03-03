#install.packages("pastecs")         #Package required to use descriptive statistics functions
library(pastecs)                    #Library to invoke package for the use of descriptive statistics functions (eg- stas)

#install.packages("party")           # package required for decision tree related functions                 
library("party")                    #Library to invoke the party package for the use of decision tree related functions

#install.packages("rpart")           # Package required for Recursive partitioning for classification and regression trees
library(rpart)                      # Library to invoke rpart package for classification and regression trees

#install.packages("rpart.plot")      # Package required for plotting Recursive partitioning for drawing classification and regression trees
library(rpart.plot)                 # Library to invoke rpart plotting package for drawing classification and regression trees

#install.packages("Rcpp")
library(Rcpp)
#install.packages("ggplot2")         # inlcude this package as a pre-requisite for use of group ploting functions
library(ggplot2)                  # Invoke Library for the group ploting function

#install.packages("nnet")            # Package required to use multinom function for multinomial regression analysis
library(nnet)                       # Invoking library to use multinom function for multinomial regression analysis

#install.packages('GGally')
library(GGally) #for Visualization
library(e1071) #  for Support Vector Classification

#install.packages("psych")
library(psych)

#library(MASS)

#install.packages("devtools")
library(devtools)

#install_github('fawda123/ggord')
library(ggord)
library(MASS)
library(klaR)

#Loading data for LDA, Decision Tree and Regression analysis

myDataImport <- read.csv(file.choose(), header = TRUE)      #Using file choose function (file.chose() to load tha file and then assigning the file to a variable for further use)
myDataImport                                                #Command to get the data value imported from the CSV File.

myDataImport$NSP_Factor<- factor(myDataImport$NSP)          #Creating a new categorical variable to the dataset
str(myDataImport)                                           # To get the structure of the data

pairs.panels(myDataImport[1:3],
             gap=0, bg=c("red", "blue", "green")[myDataImport$NSP_Factor],
             pch=21)

# Data separation for training and test from the available data set
set.seed(1234)                 # setting the seed of random sampling for test and traning data                                
partitionData<- sample(2, nrow(myDataImport), replace = TRUE, prob = c(0.8,0.2)) # set the percentage of test and training data as partition, here training data is set as 80% and test as 20% 

trainingData<-myDataImport[partitionData==1,]       #defining training data of 80% from the dataset
testData<-myDataImport[partitionData==2,]           #defining test data of 20% from the dataset

######- Decision Tree Analysis----#####

myTrainDataTree <- ctree(NSP_Factor~LB+AC+FM, data=trainingData) # designing  the decision tree on training data with three parameters by utilizing the newly added catagorical variable
myTrainDataTree                                     #Display the tree output with numerical value          
plot(myTrainDataTree)                               #default function to display the tree plot

myTrainDataTree <- ctree(NSP_Factor~LB+AC+FM, data=trainingData, controls=ctree_control(mincriterion = 0.99, minsplit = 500)) #  contructing decision tree on training data with additional constraints - with 99% confidenc level, minsplit =500 means the branch will only divide when the min size of data is 500 
myTrainDataTree                                     #Display the tree output with numerical value          
plot(myTrainDataTree)                               #default function to display the tree plot

# predicting the probability on test dataset based on the decision tree model
predict(myTrainDataTree,trainingData, type="prob")    #Identify the probability of class for all Training data set 
predict(myTrainDataTree,trainingData)                 #Identify the class for all Training data set 

# Designing New Predection decision Tree model on train dataset

predictionTree<-rpart(NSP_Factor~LB+AC+FM, trainingData) # designing prediction tree on training data
rpart.plot(predictionTree)                    # ploting patients medical status
rpart.plot(predictionTree, extra = 1)                    # ploting patients medical status with count
rpart.plot(predictionTree, extra = 4)                    # ploting probability of the patients medical status

# predicting the probability on test dataset based on the new prediction decision tree model
predict(predictionTree,testData, type="prob")    #Identify the probability of class for all test data set (408 observation)
predict(predictionTree,testData)                 #Identify the class for all test data set (408 observation)

# Calculate misclassification Error using decision tree model- myTrainDataTree on trainingData
varianceDetailsTrainData<-table(predict(myTrainDataTree), trainingData$NSP_Factor)   #Estimating the difference between predection and actual outcome on training data
print(varianceDetailsTrainData)                                                      #Displaying the difference between predection and actual outcome on training data

varianceDetailsTrainPercentage<- 1-sum(diag(varianceDetailsTrainData))/sum(varianceDetailsTrainData)    #calculate the misclassification percentage
varianceDetailsTrainPercentage                                                          #Displaying the misclassification percentage

# Calculate misclassification Error using decision tree model- myTrainDataTree on testData
testDataPrediction<- predict(myTrainDataTree, testData)                      #Run decision tree model on Test Dataset and assiging the results of test data prediction for further analysis

varianceDetailsForTestData<-table(testDataPrediction, testData$NSP_Factor)   #Estimating the difference between predection and actual outcome on training data
print(varianceDetailsForTestData)                                                      #Displaying the difference between predection and actual outcome on training data

varianceDetailsTestPercentage<- 1-sum(diag(varianceDetailsForTestData))/sum(varianceDetailsForTestData)    #calculate the misclassification percentage
varianceDetailsTestPercentage                                                          #Displaying the misclassification percentage



######- Multinomial Logistic Regression Analysis----#####

myDataImport$output<- relevel(myDataImport$NSP_Factor, ref = "1")           #Creating a new categorical variable based on NSP_ factor (categorical variable) to the dataset, here ref=1 denotes normal, 2= suspected and 3 =pathelogical or affected

str(myDataImport)                                                           # display the new structure of the data


myDataRegressionModel<-multinom(output~LB+AC+FM, data =myDataImport)       # using multimodel regression model for creating the model on complete dataset
summary(myDataRegressionModel)                                             # Summary statistics for the regression model

myTrainDataRegPredection<-predict(myDataRegressionModel, trainingData)     # Applying the regession model on training data set
myTrainDataRegPredection                                                   #Display prediction result from the applied model on Training dataSet 

myTrainDataRegPredectionProbability<-predict(myDataRegressionModel, trainingData, type = 'prob') # Applying the regession model on training data set to calculate the probability of each element in the dataset
myTrainDataRegPredectionProbability                                        #Display PROBABILITY prediction  result from the applied model on Training dataSet                    

myTrainDataRegPredectionProbabilityFor<-predict(myDataRegressionModel, myDataImport[c(3, 230,340,450),], type = 'prob')         ## Applying the regession model on complete dataset to calculate the probability of given element or record no
myTrainDataRegPredectionProbabilityFor                                    #Display prediction result of specific no of records from the applied model on Complete dataset

myTestDataRegPredection<-predict(myDataRegressionModel, testData) # Applying the regession model on training data set to calculate the probability of each element in the dataset
myTestDataRegPredection                                       #Display PROBABILITY prediction  result from the applied model on Training dataSet                    

myTestDataRegPredectionProbability<-predict(myDataRegressionModel, testData, type = 'prob') ## Applying the regession model on test data set to calculate the prediction
myTestDataRegPredectionProbability                                                   # Display the prediction output

#Misclassification error claculation for regression model

regressionVarianceDetails<-table(predict(myDataRegressionModel), myDataImport$NSP_Factor)   #Estimating the difference between predection and actual outcome on complete dataset
print(regressionVarianceDetails)                                                      #Displaying the difference between predection and actual outcome of complete dataset

varianceDetailsPercentage<- 1-sum(diag(regressionVarianceDetails))/sum(regressionVarianceDetails)    #calculate the misclassification percentage
varianceDetailsPercentage                                                          #Displaying the misclassification percentage

regressionVarianceTestDetails<-table(predict(myDataRegressionModel, data=testData), myDataImport$NSP_Factor)   #Estimating the difference between predection and actual outcome on complete dataset
print(regressionVarianceTestDetails)                                                      #Displaying the difference between predection and actual outcome of complete dataset

varianceDetailsPercentage<- 1-sum(diag(regressionVarianceDetails))/sum(regressionVarianceDetails)    #calculate the misclassification percentage
varianceDetailsPercentage     


######- Liner Discriminant Analysis----#####

myTrainLDA <- lda(NSP_Factor~LB+AC+FM, data = trainingData) #LDA using all three identified parameters
myTrainLDA                              #Extract the LDA model values

myTrainLDA$prior                        #print the probability of all three categories using LDA
myTrainLDA$counts                       #print the estimated count of all three categories using LDA

myTrainPredict<- predict(myTrainLDA, trainingData) # estimate the predictio on training data set

ggord(myTrainLDA, trainingData$NSP_Factor) # plot for the coeffienect value of all used parameters in LDA

partimat(NSP_Factor~LB+AC+FM, data = trainingData, method="lda") #linear separation plot
partimat(NSP_Factor~LB+AC+FM, data = trainingData, method="qda") #quadratic separation plot

#Confusion Matrix for training Dataset

trainingPrediction<- predict(myTrainLDA, trainingData)$class
trainingTab<- table(predicted= trainingPrediction, actual= trainingData$NSP_Factor)

LDAaccuracyRateTrain<- sum(diag(trainingTab))/ sum(trainingTab)

#Confusion Matrix for test Dataset

testPrediction<- predict(myTrainLDA, testData)$class
testTab<- table(predicted= testPrediction, actual= testData$NSP_Factor)

LDAaccuracyRateTest<- sum(diag(testTab))/ sum(testTab)
LDAaccuracyRateTest
