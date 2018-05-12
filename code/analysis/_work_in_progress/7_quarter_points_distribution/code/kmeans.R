set.seed(123)
x <- c(runif(100, min=1, max=2), runif(100, min=5, max=6))
y <- c(runif(100, min=1, max=2), runif(100, min=5, max=6))
df <- data.frame(x, y)
plot(df$x, df$y)

km <- kmeans(df, 1)
km$withinss

km <- kmeans(df, 2)
km$withinss


df$cluster <- km$cluster

library(factoextra)
fviz_nbclust(df[ , 1:2], kmeans, method='wss')


hist(df$x[df$cluster==1])
hist(df$y[df$cluster==2])
