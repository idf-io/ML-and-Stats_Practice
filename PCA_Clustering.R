###
# Ian Dirk Fichtner
###


#### Libraries

library(tidyverse)
library(ggbiplot)
library(magrittr)


#### Define input data
write.csv(iris, file = "data/dataset_iris.csv")


#### Use PCA and clustering to try to recover species and assess its prediction accuracy

# Exclude categorical variable to be recovered
iris.nospec <- iris %>% select(-Species)

# Calculate the PCA
iris.pca <- prcomp(iris.nospec, center = TRUE, scale = TRUE)

# Inspect new PCA object
(iris.pca.varprop <- iris.pca %>% summary() %>% pluck("importance") %>% extract("Proportion of Variance",))

# Plot the PCA
pca.fig <- ggplot(iris.pca$x %>% as.data.frame()) + 
  geom_point(aes(x = PC1, y = PC2)) +
  ggtitle("Iris PCA")

ggsave(filename = "figures/PCA/iris_PCA.png", 
       plot = pca.fig,
       width = 1000,
       height = 1000,
       units = "px"
)

# Plot the PCA with the individual feature loadigs in a biplot
biplot.fig <- ggbiplot(iris.pca) + # , labels=rownames(iris.nospec))
  ggtitle("PCA of mtcars dataset") +
  theme_minimal()

ggsave(filename = "figures/PCA/iris_PCA_biplot.png", 
       plot = biplot.fig,
       width = 1000,
       height = 1000,
       scale = 3,
       units = "px",
       bg = "white"
)
