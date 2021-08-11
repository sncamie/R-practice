library(mlr)
listLearners()$class

library(tidyverse)


data(diabetes, package = "mclust")
diabetesTib <- as_tibble(diabetes)
diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")
inner <- makeResampleDesc("CV")
outer <- makeResampleDesc("RepCV", folds = 10, reps = 5)

knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))
gridSearch <- makeTuneControlGrid()
knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner,
                              par.set = knnParamSpace,
                              control = gridSearch)
cvWithTuning <- resample(knnWrapper, diabetesTask, resampling = outer)
