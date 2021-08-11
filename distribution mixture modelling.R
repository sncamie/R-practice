library(mclust)
library(tidyverse)
data(banknote, package = "mclust")

swissTib <- dplyr::select(banknote, -1) %>%
  as_tibble()
swissTib
swissMclust <- Mclust(swissTib)
plot(swissMclust)
