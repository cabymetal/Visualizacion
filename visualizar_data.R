library(readr)
netflix_titles <- read_csv("C:/Users/user/Downloads/archive/netflix_titles.csv")

dist <- sample(c(1, 2, 3, 4), size = 6234, replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.05))
dist <- as.factor(dist)
levels(dist) <- c("TV", "PC", "Mobile", "Other")
netflix_titles$prefered_watch <- dist

write.csv(netflix_titles, "C:/Users/user/Downloads/archive/netflix_titles2.csv")