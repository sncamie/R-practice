library(tidyverse)
library(GGally)

data(flea)
fleaTib <- as_tibble(flea)
fleaTib
ggpairs(flea, mapping = aes(col = species)) +
  theme_bw()
library(kohonen)
somGrid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal",
                   neighbourhood.fct = "bubble", toroidal = FALSE)
fleaScaled <- fleaTib %>%
  select(-species) %>%
  scale()
fleaSom <- som(fleaScaled, grid = somGrid, rlen = 5000,
               alpha = c(0.05, 0.01))

par(mfrow = c(2, 3))
plotTypes <- c("codes", "changes", "counts", "quality",
               "dist.neighbours", "mapping")
walk(plotTypes, ~plot(fleaSom, type = ., shape = "straight"))

getCodes(fleaSom) %>%
  as_tibble() %>%
  iwalk(~plot(fleaSom, type = "property", property = .,
              main = .y, shape = "straight"))
par(mfrow = c(1, 2))
nodeCols <- c("cyan3", "yellow", "purple")
plot(fleaSom, type = "mapping", pch = 21,
     bg = nodeCols[as.numeric(fleaTib$species)],
     shape = "straight", bgcol = "lightgrey")

newData <- tibble(tars1 = c(120, 200),
                  tars2 = c(125, 120),
                  head = c(52, 48),
                  aede1 = c(140, 128),
                  aede2 = c(12, 14),
                  aede3 = c(100, 85)) %>%
  scale(center = attr(fleaScaled, "scaled:center"),
        scale = attr(fleaScaled, "scaled:scale"))
predicted <- predict(fleaSom, newData)
par(mfrow = c(1, 1))
plot(fleaSom, type = "mapping", classif = predicted, shape = "round")
