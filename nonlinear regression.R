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

ozoneForGam <- mutate(ozoneClean,
                      DayOfYear = as.numeric(interaction(Date, Month))) %>%
  select(c(-"Date", -"Month"))
ggplot(ozoneForGam, aes(DayOfYear, Ozone)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#define the tast and wrap it 

gamTask <- makeRegrTask(data = ozoneForGam, target = "Ozone")
imputeMethod <- imputeLearner("regr.rpart")
gamImputeWrapper <- makeImputeWrapper("regr.gamboost",
                                      classes = list(numeric = imputeMethod))
gamFeatSelControl <- makeFeatSelControlSequential(method = "sfbs")
kFold <- makeResampleDesc("CV", iters = 10)
gamFeatSelWrapper <- makeFeatSelWrapper(learner = gamImputeWrapper,
                                        resampling = kFold,
                                        control = gamFeatSelControl)
holdout <- makeResampleDesc("Holdout")
gamCV <- resample(gamFeatSelWrapper, gamTask, resampling = holdout)
gamCV

#training the model 
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())
gamModel <- train(gamFeatSelWrapper, gamTask)
parallelStop()
gamModelData <- getLearnerModel(gamModel, more.unwrap = TRUE)

par(mfrow = c(3, 3))
plot(gamModelData, type = "l")
plot(gamModelData$fitted(), resid(gamModelData))
qqnorm(resid(gamModelData))
qqline(resid(gamModelData))
par(mfrow = c(1, 1))


interaction(1:4, c("a", "b", "c", "d"))
