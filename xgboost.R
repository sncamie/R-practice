library(mlr)
library(tidyverse)

data(Zoo, package = "mlbench")
zooTib <- as_tibble(Zoo)

zooTib <- mutate_if(zooTib, is.logical, as.factor)

zooXgb <- mutate_at(zooTib, .vars = vars(-type), .funs = as.numeric)
xgbTask <- makeClassifTask(data = zooXgb, target = "type")

xgb <- makeLearner("classif.xgboost")

xgbParamSpace <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 5),
  makeIntegerParam("max_depth", lower = 1, upper = 5),
  makeNumericParam("min_child_weight", lower = 1, upper = 10),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeIntegerParam("nrounds", lower = 20, upper = 20),
  makeDiscreteParam("eval_metric", values = c("merror", "mlogloss")))
randSearch <- makeTuneControlRandom(maxit = 1000)
cvForTuning <- makeResampleDesc("CV", iters = 5)
tunedXgbPars <- tuneParams(xgb, task = xgbTask,
                           resampling = cvForTuning,
                           par.set = xgbParamSpace,
                           control = randSearch)
tunedXgbPars

tunedXgb <- setHyperPars(xgb, par.vals = tunedXgbPars$x)
tunedXgbModel <- train(tunedXgb, xgbTask)

xgbModelData <- getLearnerModel(tunedXgbModel)
ggplot(xgbModelData$evaluation_log, aes(iter, train_mlogloss)) +
  geom_line() +
  geom_point()

outer <- makeResampleDesc("CV", iters = 3)
xgbWrapper <- makeTuneWrapper("classif.xgboost",
                              resampling = cvForTuning,
                              par.set = xgbParamSpace,
                              control = randSearch)
cvWithTuning <- resample(xgbWrapper, xgbTask, resampling = outer)
cvWithTuning
