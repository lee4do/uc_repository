# Packages
library(readr)
library(dplyr)

# Codes
seeds <- read_delim("seeds_dataset.txt", delim = "\t", col_names = FALSE)
seeds <- as.data.frame(seeds)
colnames(seeds) <- c("area", "perimeter", "compactness", "length_of_kernel",
                     "width_of_kernel", "asymmetry_coefficient", 
                     "length_of_kernel_groove", "variety")
head(seeds)


dim(seeds)
sum(is.na(seeds))
summary(seeds)
count(seeds, variety)
seeds %>%
  group_by(variety) %>%
  summarise_all(list(~mean(.)))


seeds2 <- seeds[, 1:7]
seeds2 <- scale(seeds2)
seeds2di <- dist(seeds2, method = "euclidean")
seeds2hc <- hclust(seeds2di, method = "complete")
seeds2as <- cutree(seeds2hc, k = 3)
seeds2 <- as.data.frame(seeds2)
seeds2 <- mutate(seeds2, cluster = seeds2as)

sum(seeds$variety != seeds2$cluster) / nrow(seeds)
count(seeds2, cluster)
seeds2 %>%
  group_by(cluster) %>%
  summarise_all(list(~mean(.)))


seeds3 <- seeds[, 1:7]
seeds3 <- scale(seeds3)
seeds3di <- dist(seeds3, method = 'euclidean')
seeds3hc <- hclust(seeds3di, method = 'single')
seeds3as <- cutree(seeds3hc, k = 3)
seeds3 <- as.data.frame(seeds3)
seeds3 <- mutate(seeds3, cluster = seeds3as)

sum(seeds$variety != seeds3$cluster) / nrow(seeds)
count(seeds3, cluster)
seeds3 %>%
  group_by(cluster) %>%
  summarise_all(list(~mean(.)))


seeds4 <- seeds[, 1:7]
seeds4 <- scale(seeds4)
seeds4di <- dist(seeds4, method = 'euclidean')
seeds4hc <- hclust(seeds4di, method = 'average')
seeds4as <- cutree(seeds4hc, k = 3)
seeds4 <- as.data.frame(seeds4)
seeds4 <- mutate(seeds4, cluster = seeds4as)

sum(seeds$variety != seeds4$cluster) / nrow(seeds)
count(seeds4, cluster)
seeds4 %>%
  group_by(cluster) %>%
  summarise_all(list(~mean(.)))

