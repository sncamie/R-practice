library(tidyverse)

library(mlr)

#laod data 
data("iris")
iristb<- as_tibble(iris)

summary(iristb)

irisTask<-makeClassifTask(data=iristb, target = "Species")

irisTask

knn2<-makeLearner("classif.knn", par.vals = list("k"=2))

knnModel2 <- train(knn2,irisTask)

knnPred2 <- predict(knnModel2, newdata = iristb)

performance(knnPred2, measures = list(mmce,acc))

#Cross validation

#Hold out
holdout2 <- makeResampleDesc(method = "Holdout", split=2/3, stratify = TRUE)

holdoutCV2 <- resample(learner = knn2, task = irisTask,
                       resampling = holdout2, measures = list(mmce, acc))
calculateConfusionMatrix(holdoutCV2$pred, relative = TRUE)

#K Fold

kFold2 <- makeResampleDesc(method = "RepCV", folds=10, reps=50, stratify = TRUE)

kFoldCV2 <- resample(learner = knn2, task = irisTask,
                     resampling = kFold2, measures = list(mmce,acc))
calculateConfusionMatrix(kFoldCV2$pred, relative = TRUE)

LOO2 <- makeResampleDesc(method = "LOO")

LOOCV2<- resample(learner = knn2, task = irisTask,
                     resampling = LOO2, measures = list(mmce,acc))

calculateConfusionMatrix(LOOCV2$pred, relative = TRUE)


knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:25))


gridSearch <- makeTuneControlGrid()

cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams("classif.knn", task = irisTask,
                     resampling = cvForTuning,
                     par.set = knnParamSpace,
                     control = gridSearch)
