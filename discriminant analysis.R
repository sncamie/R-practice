library(tidyverse)
library(mlr)
data(wine, package = "HDclassif")

wineTib <- as_tibble(wine)

wineTib
names(wineTib) <- c("Class", "Alco", "Malic", "Ash", "Alk", "Mag",
                    "Phe", "Flav", "Non_flav", "Proan", "Col", "Hue",
                    "OD", "Prol")

wineTib$Class <- as.factor(wineTib$Class)
wineTib

wineUntidy <- gather(wineTib, "Variable", "Value", -Class)

ggplot(wineUntidy, aes(Class, Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_boxplot() +
  theme_bw()

#training the model 

wineTask <- makeClassifTask(data = wineTib, target = "Class")

lda <-makeLearner("classif.lda")

ldaModel <- train(lda, wineTask)

ldaModelData <- getLearnerModel(ldaModel)

ldapreds <- predict(ldaModelData)$x

head(ldapreds)

wineTib %>%
  mutate(LD1 = ldapreds[, 1],
         LD2 = ldapreds[, 2]) %>%
  ggplot(aes(LD1, LD2, col = Class)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()

#now for QDA 

qda<-makeLearner("classif.qda")



qdaModel <- train(qda, wineTask)

qdaModelData <- getLearnerModel(qdaModel)

qdapreds <- predict(qdaModelData)$x

head(qdapreds)

#cross validate models 

kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50,
                          stratify = TRUE)
ldaCV <- resample(learner = lda, task = wineTask, resampling = kFold,
                  measures = list(mmce, acc))
qdaCV <- resample(learner = qda, task = wineTask, resampling = kFold,
                  measures = list(mmce, acc))

ldaCV$aggr
qdaCV$aggr

#confusion matrix
calculateConfusionMatrix(ldaCV$pred, relative = TRUE)

calculateConfusionMatrix(qdaCV$pred, relative = TRUE)

poisoned <- tibble(Alco = 13, Malic = 2, Ash = 2.2, Alk = 19, Mag = 100,
                   Phe = 2.3, Flav = 2.5, Non_flav = 0.35, Proan = 1.7,
                   Col = 4, Hue = 1.1, OD = 3, Prol = 750)
predict(qdaModel, newdata = poisoned)
