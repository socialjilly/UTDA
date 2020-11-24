#LOAD PKG
library(caret)

#LOAD DATA 
CompleteResponses <-read.csv("C:\\Users\\jills\\Downloads\\CompleteResponses.csv")

#LOAD DATA (Survey Incomplete used for final predictions)
SurveyIncomplete <-read.csv("C:\\Users\\jills\\Downloads\\SurveyIncomplete.csv")

#EXPLORE DATA
summary(CompleteResponses)
str(CompleteResponses)
attributes(CompleteResponses)
names(CompleteResponses)

#PLOTTING...
#histogram
hist(CompleteResponses$brand)
#normal quantile plot - way to see if data normally ditributed
qqnorm(CompleteResponses$brand)


#CHANGE DATA TYPE
CompleteResponses$brand <-as.factor(CompleteResponses$brand)
str(CompleteResponses)

#incomplete dataset must be changed to factor too!
SurveyIncomplete$brand <-as.factor(SurveyIncomplete$brand)
str(SurveyIncomplete)

#MISSING VALUES
summary(CompleteResponses)
is.na(CompleteResponses)
#so no missing values, right?


save.image()

##CARET MODEL - AUTO TUNE and use C5.0
#dataset = CompleteResponses
#Dep (Y) Variable = brand

#SET SEED (almost forgot)
set.seed(123)

#define a 75%/25% TRAIN/TEST SPLIT of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

#10 fold CROSS VALIDATION (TRAIN CONTROL)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#call C50 package
library(C50)

#TRAIN C50 model with tune length 5
c50fit1 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 5)

#CHECK TRAIN RESULTS
c50fit1   
#c50fit1 final values- trials=40, model=tree, winnow=FALSE (cross val)
#accuracy 0.9172913   kappa 0.8252257


#VARIMP variable importance C50fit1
varImp(c50fit1)
# Overall
#age     100.0000
#salary  100.0000
#elevel   31.6762
#car       2.8043
#credit    0.5099
#zipcode   0.0000


#MORE MODEL INFO
c50fit1$finalModel$tuneValue   #parameter values trials/model/winnow
c50fit1$finalModel$tree      #whoa, in no way sure what to do w this info
summary(c50fit1)        #evaluation/attribute usage
plot(c50fit1)         #not quite sure how to use this info

save.image()


##CARET MODEL - MANUAL TUNE and use 5 diff mtry values (expand.grid)
#dataset = CompleteResponses
#Dep (Y) Variable = brand

#all prior data work relevant through fit control (Train Control)
#using same dataset and training splits 

##DATA FRAME FOR MANUAL TUNING OF MTRY 
rfGrid1 <- expand.grid(mtry=c(1,2,3,4,5))

#TRAIN Random Forest model
system.time(rfFitM1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid1))

#TRAINING RESULTS
rfFitM1
#rfFitM1 final values - mtry =3, 
#accuracy 0.9187790   kappa 0.8277629

#VARIMP variable importance rfFitM1
varImp(rfFitM1)
# Overall
#salary  100.000
#age      61.059
#credit    8.827
#car       3.176
#zipcode   1.502
#elevel    0.000

#MORE MODEL INFO ------ not sure any of these are relevant with this model type
rfFitM1$finalModel$tuneValue   #shows final mtry value
rfFitM1$finalModel$tree        #NULL
summary(rfFitM1)               #eval
plot(rfFitM1)                  #???



##COMPARE MODELS
resample_results <- resamples(list(RandomForest = rfFitM1, C5.0 = c50fit1))
resample_results
resample_results$values
summary(resample_results)
bwplot(resample_results)
diff_results <- diff(resample_results)
summary(diff_results, metric = "accuracy")
compare_models(c50fit1, rfFitM1)


##PREDICTIONS
#Predict with RandomForest (rfFitM1) on CompleteResponses Data
testPredrf1 <-predict(rfFitM1, testing)
summary(testPredrf1) 
postResample(testPredrf1, testing$brand)
confusionMatrix(testPredrf1, testing$brand)
#   Accuracy     Kappa 
#   0.9264349 0.8442613 


#### FINAL PREDICTION ON SURVEY INCOMPLETE DATA
finalPredrf1 <-predict(rfFitM1, SurveyIncomplete)

#Get Predicted preference totals (survey incomplete)
summary(finalPredrf1)
#    0    1 
#  1885 3115   0    1   Sony prefered brand over acer



#Get Known preference totals (complete responses data) Ground Truth?
summary(CompleteResponses$brand)
#   0    1     YASSSSSSSS spot on in line
# 3744 6154 


#postresample to asess
postResample(finalPredrf1, SurveyIncomplete$brand)
#NOPE that will not work. 
#    Accuracy      Kappa 
#   0.38800000 0.01274463 
#there is no Y value, no ground truth, data is corrupt/incorrectly caputured

summary(SurveyIncomplete$brand)
#    0    1 
#  4937   63   OOF that's awful, but we knew data incomplete so no surprise really


save.image()
