library(umap)
data(banknote, package = "mclust")
swissTib <- as_tibble(banknote)
swissTib
swissUmap <- select(swissTib, -Status) %>%
  as.matrix() %>%
  umap(n_neighbors = 7, min_dist = 0.1,
       metric = "manhattan", n_epochs = 200, verbose = TRUE)
swissTibUmap <- swissTib %>%
  mutate_if(.funs = scale, .predicate = is.numeric, scale = FALSE) %>%
  mutate(UMAP1 = swissUmap$layout[, 1], UMAP2 = swissUmap$layout[, 2]) %>%
  gather(key = "Variable", value = "Value", c(-UMAP1, -UMAP2, -Status))
ggplot(swissTibUmap, aes(UMAP1, UMAP2, col = Value, shape = Status)) +
  facet_wrap(~ Variable) +
  geom_point(size = 3) +
  scale_color_gradient(low = "dark blue", high = "cyan") +
  theme_bw()

newBanknotes <- tibble(
  Length = c(214, 216),
  Left = c(130, 128),
  Right = c(132, 129),
  Bottom = c(12, 7),
  Top = c(12, 8),
  Diagonal = c(138, 142)
)
predict(swissUmap, newBanknotes)
