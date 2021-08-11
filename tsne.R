library(Rtsne)

data(banknote, package = "mclust")
swissTib <- as_tibble(banknote)
swissTib
swissTsne <- select(swissTib, -Status) %>%
  Rtsne(perplexity = 30, theta = 0, max_iter = 5000, verbose = TRUE)
swissTibTsne <- swissTib %>%
  mutate_if(.funs = scale, .predicate = is.numeric, scale = FALSE) %>%
  mutate(tSNE1 = swissTsne$Y[, 1], tSNE2 = swissTsne$Y[, 2]) %>%
  gather(key = "Variable", value = "Value", c(-tSNE1, -tSNE2, -Status))
ggplot(swissTibTsne, aes(tSNE1, tSNE2, col = Value, shape = Status)) +
  facet_wrap(~ Variable) +
  geom_point(size = 3) +
  scale_color_gradient(low = "dark blue", high = "cyan") +
  theme_bw()
