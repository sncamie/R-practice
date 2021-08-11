library(mlr)
library(tidyverse)
data(GvHD, package = "mclust")
gvhdTib <- as_tibble(GvHD.control)
gvhdTib
gvhdScaled <- gvhdTib %>% scale()
library(GGally)
ggpairs(GvHD.control,
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = "densityDiag")) +
  theme_bw()

gvhdTask <- makeClusterTask(data = as.data.frame(gvhdScaled))
listLearners("cluster")$class
kMeans <- makeLearner("cluster.kmeans",
                      par.vals = list(iter.max = 100, nstart = 10))

kMeansParamSpace <- makeParamSet(
  makeDiscreteParam("centers", values = 3:8),
  makeDiscreteParam("algorithm",
                    values = c("Hartigan-Wong", "Lloyd", "MacQueen")))
gridSearch <- makeTuneControlGrid()
kFold <- makeResampleDesc("CV", iters = 10)

library(clusterSim)
tunedK <- tuneParams(kMeans, task = gvhdTask,
                     resampling = kFold,
                     par.set = kMeansParamSpace,
                     control = gridSearch,
                     measures = list(db,G1))
tunedK

kMeansTuningData <- generateHyperParsEffectData(tunedK)
kMeansTuningData$data

gatheredTuningData <- gather(kMeansTuningData$data,
                             key = "Metric",
                             value = "Value",
                             c(-centers, -iteration, -algorithm))
ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw()


tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x)
tunedKMeansModel <- train(tunedKMeans, gvhdTask)
kMeansModelData <- getLearnerModel(tunedKMeansModel)
kMeansModelData$iter

gvhdTib <- mutate(gvhdTib,
                  kMeansCluster = as.factor(kMeansModelData$cluster))
ggpairs(gvhdTib, aes(col = kMeansCluster),
        upper = list(continuous = "density")) +
  theme_bw()

newCell <- tibble(CD4 = 510,
                  CD8b = 26,
                  CD3 = 500,
                  CD8 = 122) %>%
  scale(center = attr(gvhdScaled, "scaled:center"),
        scale = attr(gvhdScaled, "scaled:scale")) %>%
  as_tibble()
predict(tunedKMeansModel, newdata = newCell)
