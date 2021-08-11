library(tidyverse)
data(banknote, package = "mclust")
swissTib <- as_tibble(banknote)
swissTib
library(GGally)
ggpairs(swissTib, mapping = aes(col = Status)) +
  theme_bw()
pca <- select(swissTib, -Status) %>%
  prcomp(center = TRUE, scale = TRUE)
pca
summary(pca)
map_dfc(1:6, ~pca$rotation[, .] * sqrt(pca$sdev ^ 2)[.])

library(factoextra)
pcaDat <- get_pca(pca)
fviz_pca_biplot(pca, label = "var")
fviz_pca_var(pca)
fviz_screeplot(pca, addlabels = TRUE, choice = "eigenvalue")
fviz_screeplot(pca, addlabels = TRUE, choice = "variance")


swissPca <- swissTib %>%
  mutate(PCA1 = pca$x[, 1], PCA2 = pca$x[, 2])
ggplot(swissPca, aes(PCA1, PCA2, col = Status)) +
  geom_point() +
  theme_bw()        


newBanknotes <- tibble(
  Length = c(214, 216),
  Left = c(130, 128),
  Right = c(132, 129),
  Bottom = c(12, 7),
  Top = c(12, 8),
  Diagonal = c(138, 142)
)

predict(pca, newBanknotes)        
