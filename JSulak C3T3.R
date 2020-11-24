#LOAD PKGS
library(readr)
library(caret)

#LOAD DATA
existingprods <-read.csv("C:\\Users\\jills\\Downloads\\existingproducts.csv")
newprods <-read.csv("C:\\Users\\jills\\Downloads\\newproducts.csv")


#EXPLORE DATA
summary(existingprods)
str(existingprods)
attributes(existingprods)
names(existingprods)

#PLOTTING... ? no won't work with mix data
#histogram
hist(existingprods$ProductType)
#scatterplot
plot(existingprods$Volume, existingprods$ProductType)
##nope won't work, data must be numeric.... not sure useful to plot anything else?




#DUMMIFY THE DATA
newDataFrame <- dummyVars(" ~ .", data = existingprods)
ExistProds1 <- data.frame(predict(newDataFrame, newdata = existingprods))


#CHECK DATA
str(ExistProds1)
summary(ExistProds1)
#15 NAs in Best Seller Rank - don't need that column at all, just delete...

#MISSING DATA - DELETE
ExistProds1$BestSellersRank <- NULL

summary(ExistProds1)


#CORRELATION MATRIX
corrData <- cor(ExistProds1)
corrData


#CORR HEAT MAP with corrplot
library(corrplot)
corrplot(corrData)

#POSSIBLY USEFUL CORREALTION NOTES - 
#corr strongest between volume and 5 star reviews- too good at perfect - remove?
#(correlation declines between volume and reviews as review stars decrease)
#some decent correlation between postive service reviews and volume 
#but neg service review sees much less corr with volume
#price offers no real seen corr with volume

save.image()


#REMOVE FEATURES - NOT NEEDED
#product number, profit margin, those are irrelevant)
# any thing else... ? (already removed best seller rank)
ExistProds1$ProductNum  <- NULL
ExistProds1$ProfitMargin <- NULL


str(ExistProds1) #looks good,(features removed, yay!)

save.image()


 ##MODELING

#set seed
set.seed(123)

#DEFINE 80/20 TRAIN/SET SPLIT OF DATASET
inTraining <- createDataPartition(ExistProds1$Volume, p = .80, list = FALSE)
training <- ExistProds1[inTraining,]
testing <- ExistProds1[-inTraining,]

#VIEW TEST/TRAIN SETS
head (training)
head (testing)

#CROSS VALIDATION (TRAIN CONTROL) 3 fold for small set
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)


##TRAINING...

#TRAIN LINEAR REGRESSION MODEL
lmFit1 <- train(Volume~., data = training, method = "lm", trControl=fitControl)
   #warning: prediction from rank-deficient fit may be misleading
   

#CHECK THE RESULTS (LM)
lmFit1
# RMSE          Rsquared  MAE         
# 1.876291e-12   1         7.055404e-13
  #OVERFIT -- parametric vs non parametric - use rank order methods

summary(lmFit1)
   #Multiple R-squared:      1,	Adjusted R-squared:      1 
   #F-statistic: 3.786e+31 on 24 and 40 DF,  p-value: < 2.2e-16
   #essentially perfect fit: summary may be unreliable


save.image()

#TRAIN SVM Model
svmFit1 <-  train(Volume~., data = training, method = "svmLinear2", trControl=fitControl)

  #uuuhhhh? 
  #Warning messages:
  # 1: In svm.default(x = as.matrix(x), y = y, kernel = "linear", cost = param$cost,  :
  # Variable(s) ‘ProductTypeGameConsole’ and ‘ProductTypeNetbook’ constant. Cannot scale data.
  #oooh all the zeros went to training set folds, no instances of 1, not much can be done for that
  #will happen with small data set, larger set may fix that or repeating the set have to make bigger

#RESULTS SVM
svmFit1
  # cost  RMSE       Rsquared   MAE      
  # 0.25  519.9702  0.8578056  228.5915

summary(svmFit1)

varImp(svmFit1)
  #                          Overall
  #x5StarReviews               100.0000
  #x4StarReviews                93.1784
  #PositiveServiceReview        87.3823
  #x2StarReviews                75.2360
  #x3StarReviews                55.2718
  #NegativeServiceReview        23.2514
  #x1StarReviews                21.6892


#TRAIN RANDOM FOREST Model
rfFit1 <-  train(Volume~., data = training, method = "rf", trControl=fitControl)

#RESULTS RF
rfFit1
  # mtry  RMSE      Rsquared   MAE     
  # 13    850.5325  0.7083061  236.2249

summary(rfFit1)
varImp(rfFit1)

#TRAIN GRADIENT BOOST Model (GBM)
gbmFit1 <-  train(Volume~., data = training, method = "gbm", trControl=fitControl)

gbmFit1
   #int.depth  n.trees  RMSE      Rsquared  MAE     
   # 1         50      1063.503   0.5464564  459.2115
#again a warning about no variation because small set and binary dummies 


#Not sure any of this is correct. should remove 5 star reviews and run all again? 
#Nope that doesnt help. Scratch that.
#Let's try RF with tune length also...

#TRAIN RF with tunelength 5
rfFit5 <- train(Volume~., data = training, method = "rf", trControl=fitControl, tuneLength = 5)

#RESULTS RF5
rfFit5
   #mtry  RMSE      Rsquared   MAE 
   #19    827.0555  0.7811489  232.1958

varImp(rfFit5)
  #                              Overall
  # x5StarReviews               1.000e+02
  # PositiveServiceReview       4.549e+01
  # x4StarReviews               3.690e+01
  # x2StarReviews               1.445e+01
  # x1StarReviews               1.174e+01
  # NegativeServiceReview       8.976e+00
  # x3StarReviews               7.914e+00


#that's getting better, maybe try tune length 10?...
#TRAIN RF with tune 10
rfFit10 <- train(Volume~., data = training, method = "rf", trControl=fitControl, tuneLength = 10)

#RESULTS RF10
rfFit10
  #mtry  RMSE       Rsquared   MAE 
  #9     931.4696  0.6727391  276.7251
      #Nope that got worse, stick w 5


##COMPARE MODELS
resample_results <- resamples(list(RandomForest = rfFit1, SVM = svmFit1, GradientBoost = gbmFit1, RandomForest5 = rfFit5))
resample_results
summary(resample_results)

#EXTRA MODEL COMPARES
resample_results$values
bwplot(resample_results)
diff_results <- diff(resample_results)
summary(diff_results, metric = "accuracy")
compare_models(rfFit1, svmFit1, gbmFit1, rfFit5)


##PREDICTIONS to test set models (still using existing products dataset)

#Predict with SVM Model (svmFit1)
testPredsvm1 <-predict(svmFit1, testing)
summary(testPredsvm1) 
testPredsvm1
postResample(testPredsvm1, testing$Volume)
#       RMSE    Rsquared         MAE 
#  254.4065072   0.9820231    155.1993533 


#Predict with RF Model (rfFit1)
testPredrf1 <-predict(rfFit1, testing)
summary(testPredrf1) 
testPredrf1
postResample(testPredrf1, testing$Volume)
   #RMSE         Rsquared          MAE 
   #1062.9357962    0.8332101  320.0517067 


#Predict with GB (gbmFit1)
testPredgbm1 <-predict(gbmFit1, testing)
summary(testPredgbm1) 
testPredgbm1
postResample(testPredgbm1, testing$Volume)
   #   RMSE        Rsquared      MAE 
   #1354.453338    0.442775  526.712552  



#Predict with RF Tune 5 (rfFit5)
testPredrf5 <-predict(rfFit5, testing)
summary(testPredrf5) 
testPredrf5
postResample(testPredrf5, testing$Volume)
  #   RMSE        Rsquared      MAE 
  # 918.5615862   0.8976338 273.9298578 


confusionMatrix(testPredrf5, testing$Volume)
  #Error: `data` and `reference` should be factors with the same levels.
 #HUH? 



##APPLY TO NEW PRODUCTS DATASET##

#PREPARE NEW TEST DATA SET - NEWPRODS
#dummyvar
newDataFrame2 <- dummyVars(" ~ .", data = newprods)
readyNewProds <- data.frame(predict(newDataFrame2, newdata = newprods))

#DELETE THE SAME FEATURES as previous dataset to match
readyNewProds$BestSellersRank <- NULL
readyNewProds$ProductNum  <- NULL
readyNewProds$ProfitMargin <- NULL

#YO CHECK IT
str(readyNewProds)
summary(readyNewProds)

#SVM metrics good, but predictions came out with negatives so no good
#use RF5 instead, next best metrics

#### FINAL PREDICTION ON NEW PRODUCTS DATA (using rf5 model)
finalPredRF5 <-predict(rfFit5, readyNewProds)

summary(finalPredRF5)
finalPredRF5



#OUTPUT DATASET AND PREDICTIONS
output <- newprods
output$predictions <- finalPredRF5

#CREAT OUTPUT CSV
write.csv(output, file="SulakC3T3output5.csv", row.names = TRUE)




save.image()
