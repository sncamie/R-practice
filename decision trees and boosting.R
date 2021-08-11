library(tidyverse)
library(mlr)

data(Zoo, package = "mlbench")

zooTib <- as_tibble(Zoo)

zooTib

zooTib <- mutate_all(zooTib, as.factor)

zooTib

zooTask <- makeClassifTask(data=zooTib, target = "type")

tree<- makeLearner("classif.rpart")

getParamSet(tree)

treeParamSpace <- makeParamSet(
  makeIntegerParam("minsplit", lower = 5, upper = 20),
  makeIntegerParam("minbucket", lower = 3, upper = 10),
  makeNumericParam("cp", lower = 0.01, upper = 0.1),
  makeIntegerParam("maxdepth", lower = 3, upper = 10))

randSearch <- makeTuneControlRandom(maxit = 200)
cvForTuning <- makeResampleDesc("CV", iters = 5)

library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())
tunedTreePars <- tuneParams(tree, task = zooTask,
                            resampling = cvForTuning,
                            par.set = treeParamSpace,
                            control = randSearch)
parallelStop()
tunedTreePars

tunedTree <- setHyperPars(tree, par.vals = tunedTreePars$x)
tunedTreeModel <- train(tunedTree, zooTask)


library(rpart.plot)
treeModelData <- getLearnerModel(tunedTreeModel)
rpart.plot(treeModelData, roundint = FALSE,
           box.palette = "BuBn",
           type = 5)

printcp(treeModelData, digits = 3)


outer <- makeResampleDesc("CV", iters = 5)

treeWrapper <- makeTuneWrapper("classif.rpart", resampling = cvForTuning,
                               par.set = treeParamSpace,
                               control = randSearch)
parallelStartSocket(cpus = detectCores())

cvWithTuning <- resample(treeWrapper, zooTask, resampling = outer)

parallelStop()

cvWithTuning

#ensemble methods 



forest <- makeLearner("classif.randomForest")

forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 300, upper = 300),
  makeIntegerParam("mtry", lower = 6, upper = 12),
  makeIntegerParam("nodesize", lower = 1, upper = 5),
  makeIntegerParam("maxnodes", lower = 5, upper = 20))
randSearch <- makeTuneControlRandom(maxit = 100)
cvForTuning <- makeResampleDesc("CV", iters = 5)
parallelStartSocket(cpus = detectCores())
tunedForestPars <- tuneParams(forest, task = zooTask,
                              resampling = cvForTuning,
                              par.set = forestParamSpace,
                              control = randSearch)
parallelStop()
tunedForestPars

tunedForest <- setHyperPars(forest, par.vals = tunedForestPars$x)
tunedForestModel <- train(tunedForest, zooTask)

forestModelData <- getLearnerModel(tunedForestModel)
species <- colnames(forestModelData$err.rate)
plot(forestModelData, col = 1:length(species), lty = 1:length(species))
legend("topright", species,
       col = 1:length(species),
       lty = 1:length(species))


outer <- makeResampleDesc("CV", iters = 5)
forestWrapper <- makeTuneWrapper("classif.randomForest",
                                 resampling = cvForTuning,
                                 par.set = forestParamSpace,
                                 control = randSearch)
parallelStartSocket(cpus = detectCores())
cvWithTuning <- resample(forestWrapper, zooTask, resampling = outer)
parallelStop()
cvWithTuning

#building an xgboost model 

xgb <- makeLearner("classif.xgboost")

zooXgb <- mutate_at(zooTib, .vars = vars(-type), .funs = as.numeric)
xgbTask <- makeClassifTask(data = zooXgb, target = "type")

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


xgboost::xgb.plot.tree(model = xgbModelData, trees = 1:5)

outer <- makeResampleDesc("CV", iters = 3)
xgbWrapper <- makeTuneWrapper("classif.xgboost",
                              resampling = cvForTuning,
                              par.set = xgbParamSpace,
                              control = randSearch)
cvWithTuning <- resample(xgbWrapper, xgbTask, resampling = outer)
cvWithTuning