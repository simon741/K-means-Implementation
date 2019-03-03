choose.init.centroid.instances <- function(data.set, k, seed){
  set.seed(seed)
  data.set$cluster <- -1 #-1 - instance is not assigned to any cluster
  data.set$cluster[sample(1:nrow(data.set),k)] <- 1:k
  return(data.set)
}

min.max.normalization <- function(x) (x - min(x)) / (max(x) - min(x))

euclidean.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

find.nearest.cluster <- function(instance,centroids){
  min.euc.dist <- which.min(apply(centroids[,-ncol(centroids)],1,function(x){
    euclidean.dist(x, instance[-length(instance)]) 
  })) 
  return(centroids[min.euc.dist,"cluster"]) 
}

assign.instancese.to.clusters <- function(data.set, centroids){
  data.set$cluster <- apply(data.set,1,find.nearest.cluster, centroids)
  return(data.set)
}

compute.new.centroids <- function(data.set, centroids){
  for(i in 1:nrow(centroids)){
    centroids[i,-ncol(centroids)] <- 
      sapply(data.set[data.set$cluster == centroids[i,"cluster"],-ncol(data.set)],mean)
  }
  return(centroids)
}

all.columns.numeric.check<- function(data.set){
  ifelse(sum(sapply(data.set,is.numeric)) == ncol(data.set),TRUE,FALSE)
}

k.means <- function(data.set, k, max.iter, seed){
  if(!all.columns.numeric.check(data.set)){
    stop("All columns in a data frame have to be numeric!")
  }
  if(sum(is.na(data.set)) > 0){
    stop("There can not be any missing values in the dataset")
  }
  
  data.set <- data.frame(sapply(data.set,min.max.normalization))
  
  repeat{
    data.set <- choose.init.centroid.instances(data.set, k, seed)
    centroids <- data.set[data.set$cluster != -1,]
    centroids <- centroids[order(centroids$cluster),]
    iter.count <- 0
    clusters.changed <- T
    
    while(clusters.changed & (iter.count < max.iter)){ 
      old.clusters <- data.set$cluster
      data.set <- assign.instancese.to.clusters(data.set, centroids)
      clusters.changed <- ifelse(sum(data.set$cluster != old.clusters) > 1 ,TRUE, FALSE)
      centroids <- compute.new.centroids(data.set, centroids)
      iter.count <- iter.count + 1
      
      #---INFO---
      print(paste0("Iteration ",iter.count,": Number of changes: ", sum(data.set$cluster != old.clusters)))
    }
    
    if(!clusters.changed){
      break
    }
    else{
      #---INFO---
      print(paste0("Previous run did not converge. Trying another seed"))
      seed <- seed + 1
    }
  }
  return(data.set)
}