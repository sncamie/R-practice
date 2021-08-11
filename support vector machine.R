library(mlr)
library(tidyverse)
data(spam, package = "kernlab")
spamTib <- as_tibble(spam)
spamTask <- makeClassifTask(data = spamTib, target = "type")
svm <- makeLearner("classif.svm")
kernels <- c("polynomial", "radial", "sigmoid")
svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", values = kernels),
  makeIntegerParam("degree", lower = 1, upper = 3),
  makeNumericParam("cost", lower = 0.1, upper = 10),
  makeNumericParam("gamma", lower = 0.1, 10))
randSearch <- makeTuneControlRandom(maxit = 20)
cvForTuning <- makeResampleDesc("Holdout", split = 2/3)


library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())
tunedSvmPars <- tuneParams("classif.svm", task = spamTask,
                           resampling = cvForTuning,
                           par.set = svmParamSpace,
                           control = randSearch)
parallelStop()


tunedSvmPars
tunedSvm <- setHyperPars(makeLearner("classif.svm"),
                         par.vals = tunedSvmPars$x)
tunedSvmModel <- train(tunedSvm, spamTask)

outer <- makeResampleDesc("CV", iters = 3)
svmWrapper <- makeTuneWrapper("classif.svm", resampling = cvForTuning,
                              par.set = svmParamSpace,
                              control = randSearch)
parallelStartSocket(cpus = detectCores())
cvWithTuning <- resample(svmWrapper, spamTask, resampling = outer)
parallelStop()
cvWithTuning

