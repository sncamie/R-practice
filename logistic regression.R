library(mlr)
library(tidyverse)


data(titanic_train, package = "titanic")

titanicTib <- as_tibble(titanic_train)

titanicTib

fctrs <- c("Survived", "Sex", "Pclass")
titanicClean <- titanicTib %>%
  mutate_at(.vars = fctrs, .funs = factor) %>%
  mutate(FamSize = SibSp + Parch) %>%
  select(Survived, Pclass, Sex, Age, Fare, FamSize)

titanicClean
titanicUntidy <- gather(titanicClean, key = "Variable", value = "Value",
                        -Survived)

titanicUntidy
titanicUntidy %>%
  filter(Variable != "Pclass" & Variable != "Sex") %>%
  ggplot(aes(Survived, as.numeric(Value))) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw()

titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "fill") +
  theme_bw()

titanicTask <- makeClassifTask(data = titanicClean, target = "Survived")

logReg <- makeLearner("classif.logreg", predict.type = "prob")

logRegModel <- train(logReg, titanicTask)

#Dealing with missing data

#imputate the data by adding the mean 

imp <- impute(titanicClean, cols = list(Age = imputeMean()))

titanicTask <- makeClassifTask(data = imp$data, target = "Survived")

logRegModel <- train(logReg, titanicTask)
logRegWrapper <- makeImputeWrapper("classif.logreg",
                                   cols = list(Age = imputeMean()))
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50,
                          stratify = TRUE)
logRegwithImpute <- resample(logRegWrapper, titanicTask,
                             resampling = kFold,
                             measures = list(acc, fpr, fnr))
logRegwithImpute

logRegModelData <- getLearnerModel(logRegModel)
coef(logRegModelData)

exp(cbind(Odds_Ratio = coef(logRegModelData), confint(logRegModelData)))

#check model 

data(titanic_test, package = "titanic")

titanicNew <- as_tibble(titanic_test)
titanicNewClean <- titanicNew %>%
  mutate_at(.vars = c("Sex", "Pclass"), .funs = factor) %>%
  mutate(FamSize = SibSp + Parch) %>%
  select(Pclass, Sex, Age, Fare, FamSize)

predict(logRegModel, newdata = titanicNewClean)
