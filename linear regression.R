library(tidyverse)
library(mlr)

data(Ozone, package="mlbench")

ozoneTib <- as_tibble(Ozone)
names(ozoneTib) <- c("Month", "Date", "Day", "Ozone", "Press_height",
                     "Wind", "Humid", "Temp_Sand", "Temp_Monte",
                     "Inv_height", "Press_grad", "Inv_temp", "Visib")
ozoneTib
#remove NA data from target variable 
ozoneClean <- mutate_all(ozoneTib, as.numeric) %>%
  filter(is.na(Ozone) == FALSE)

ozoneClean

#see how predictors vary with ozone 

ozoneUntidy <- gather(ozoneClean, key = "Variable",
                      value = "Value", -Ozone)
ggplot(ozoneUntidy, aes(Value, Ozone)) +
  facet_wrap(~ Variable, scale = "free_x") +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", col = "red") +
  theme_bw()

#filling in the missing values 

imputeMethod <- imputeLearner("regr.rpart")

ozoneImp <- impute(as.data.frame(ozoneClean),
                   classes = list(numeric = imputeMethod))

ozoneTask <- makeRegrTask(data = ozoneImp$data, target = "Ozone")

lin <- makeLearner("regr.lm")

#now we want features that have predictive power. So we filter

filterVals <- generateFilterValuesData(ozoneTask,
                                       method = "linear.correlation")
filterVals$data

plotFilterValues(filterVals) + theme_bw()

#we use wrapper methods

filterWrapper = makeFilterWrapper(learner = lin,
                                  fw.method = "linear.correlation")
lmParamSpace <- makeParamSet(
  makeIntegerParam("fw.abs", lower = 1, upper = 12)
)


gridSearch <- makeTuneControlGrid()
kFold <- makeResampleDesc("CV", iters = 10)
tunedFeats <- tuneParams(filterWrapper, task = ozoneTask, resampling = kFold,
                         par.set = lmParamSpace, control = gridSearch)
tunedFeats

filteredTask <- filterFeatures(ozoneTask, fval = filterVals,
                               abs = unlist(tunedFeats$x))
filteredModel <- train(lin, filteredTask)


#wrapper selection method for features 

featSelControl <- makeFeatSelControlSequential(method = "sfbs")

selFeats <- selectFeatures(learner = lin, task = ozoneTask,
                           resampling = kFold, control = featSelControl)
selFeats

ozoneSelFeat <- ozoneImp$data[, c("Ozone", selFeats$x)]
ozoneSelFeatTask <- makeRegrTask(data = ozoneSelFeat, target = "Ozone")
wrapperModel <- train(lin, ozoneSelFeatTask)

#Combining imputation and feature selection wrappers
imputeMethod <- imputeLearner("regr.rpart")
imputeWrapper <- makeImputeWrapper(lin,
                                   classes = list(numeric = imputeMethod))
featSelWrapper <- makeFeatSelWrapper(learner = imputeWrapper,
                                     resampling = kFold,
                                     control = featSelControl)

#cross validation 

library(parallel)
library(parallelMap)
ozoneTaskWithNAs <- makeRegrTask(data = ozoneClean, target = "Ozone")
kFold3 <- makeResampleDesc("CV", iters = 3)
parallelStartSocket(cpus = detectCores())
lmCV <- resample(featSelWrapper, ozoneTaskWithNAs, resampling = kFold3)
parallelStop()
lmCV

wrapperModelData <- getLearnerModel(wrapperModel)
summary(wrapperModelData)

#plot 

par(mfrow = c(2, 2))
plot(wrapperModelData)
par(mfrow = c(1, 1))