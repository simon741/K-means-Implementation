data("iris")

find.cluster.true.class.binding <- function(data.set){
  
  t <- table(data.set$class,data.set$k.means.cluster)
  
  cluster.true.class.binding <- apply(t, 1, which.max)
  
  data.set$k.means.cluster <- apply(data.set, 1, function(x){ 
    names(cluster.true.class.binding[cluster.true.class.binding==x[length(x)]])
  })
  
  return(data.set)
}

compute.accuracy <- function(data.set){
  sum(data.set$class == data.set$k.means.cluster)/nrow(data.set)
}

#R refferal implementation
result1 <- kmeans(x = iris[,1:4],
                  centers = 3,
                  iter.max = 100,
                  nstart = 5,
                  algorithm = "Hartigan-Wong")
result1 <- data.frame(iris, k.means.cluster = as.factor(as.character(fitted(result1, method = "classes"))))
colnames(result1)[5] <- "class"
result1 <- find.cluster.true.class.binding(result1)

#our implementation
result2 <- k.means(data.set = iris[,1:4],
                   k = 3,
                   max.iter = 100, 
                   seed = 5)
result2$class <- iris$Species
result2$k.means.cluster <- result2$cluster
result2 <- find.cluster.true.class.binding(result2)

#comparison of accuracy
compute.accuracy(result1)
compute.accuracy(result2)


