library(tidyverse)
data(banknote, package = "mclust")
head(banknote)
swissTib <- dplyr::select(banknote, -1) %>%
  as_tibble()
swissTib
library(GGally)
ggpairs(swissTib, upper = list(continuous = "density")) +
  theme_bw()
swissScaled <- swissTib %>% scale()
library(dbscan)
kNNdistplot(swissScaled, k = 5)
abline(h = c(1.2, 2.0))
dbsParamSpace <- expand.grid(eps = seq(1.2, 2.0, 0.1),
                             minPts = seq(1, 9, 1))
swissDbs <- pmap(dbsParamSpace, dbscan, x = swissScaled)
swissDbs[[5]]

clusterResults <- map_dfc(swissDbs, ~.$cluster)
clusterResults
swissClusters <- bind_cols(swissTib, clusterResults)
swissClusters
swissClustersGathered <- gather(swissClusters,
                                key = "Permutation", value = "Cluster",
                                -Length, -Left, -Right,
                                -Bottom, -Top, -Diagonal)
swissClustersGathered
ggplot(swissClustersGathered, aes(Right, Diagonal,
                                  col = as.factor(Cluster))) +
  facet_wrap(~ Permutation) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")

cluster_metrics <- function(data, clusters, dist_matrix) {
  list(db = clusterSim::index.DB(data, clusters)$DB,
       G1 = clusterSim::index.G1(data, clusters),
       dunn = clValid::dunn(dist_matrix, clusters),
       clusters = length(unique(clusters))
  )
}

swissBoot <- map(1:10, ~ {
  swissScaled %>%
    as_tibble() %>%
    sample_n(size = nrow(.), replace = TRUE)
})

metricsTib <- map_df(swissBoot, function(boot) {
  clusterResult <- pmap(dbsParamSpace, dbscan, x = boot)
  map_df(clusterResult, function(permutation) {
    clust <- as_tibble(permutation$cluster)
    filteredData <- bind_cols(boot, clust) %>%
      filter(value != 0)
    d <- dist(dplyr::select(filteredData, -value))
    cluster_metrics(dplyr::select(filteredData, -value),
                    clusters = filteredData$value,
                    dist_matrix = d)
  })
})


metricsTibSummary <- metricsTib %>%
  mutate(bootstrap = factor(rep(1:10, each = 81)),
         eps = factor(rep(dbsParamSpace$eps, times = 10)),
         minPts = factor(rep(dbsParamSpace$minPts, times = 10))) %>%
  gather(key = "metric", value = "value",
         -bootstrap, -eps, -minPts) %>%
  mutate_if(is.numeric, ~ na_if(., Inf)) %>%
  drop_na() %>%
  group_by(metric, eps, minPts) %>%
  summarise(meanValue = mean(value),
            num = n()) %>%
  group_by(metric) %>%
  mutate(meanValue = scale(meanValue)) %>%
  ungroup()


ggplot(metricsTibSummary, aes(eps, minPts,
                              fill = meanValue, alpha = num)) +
  facet_wrap(~ metric) +
  geom_tile(col = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank())

which(dbsParamSpace$eps == 1.2 & dbsParamSpace$minPts == 9)


filter(swissClustersGathered, Permutation == "V73") %>%
  dplyr::select(-Permutation) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  ggpairs(mapping = aes(col = Cluster),
          upper = list(continuous = "density")) +
  theme_bw()

filter(swissClustersGathered, Permutation == "V73", Cluster != 0) %>%
  dplyr::select(-Permutation) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  ggpairs(mapping = aes(col = Cluster),
          upper = list(continuous = "density")) +
  theme_bw()

library(fpc)
clustBoot <- clusterboot(swissScaled, B = 500,
                         clustermethod = dbscanCBI,
                         eps = 1.2, MinPts = 9,
                         showplots = FALSE)
clustBoot

swissOptics <- optics(swissScaled, minPts = 9)
plot(swissOptics)
swissOpticsXi <- extractXi(swissOptics, xi = 0.05)

swissTib %>%
  mutate(cluster = factor(swissOpticsXi$cluster)) %>%
  ggpairs(mapping = aes(col = cluster),
          upper = list(continuous = "points")) +
  theme_bw()