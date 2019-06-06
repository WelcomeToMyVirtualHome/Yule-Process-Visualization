library(data.tree)
library(ape)

set.seed(2137)

initTree <- function(m=2){
  node.number <- 0
  root <- Node$new(node.number)
  for (add in 1:m){
    node.number <- node.number + 1
    root$AddChild(node.number)
  }
  return(list("root"=root,"node.number"=node.number))
}

addChildren <- function(tree, lambda=0.5, m=2){
  leaves <- unlist(tree$root$leaves, use.names = F)
  sapply(leaves, function(l) {
    if(runif(1, 0.0, 1.0) < lambda) {
      node <- FindNode(tree$root, as.numeric(l$name))
      for(add in 1:m){
        tree$node.number <<- tree$node.number + 1
        node$AddChild(tree$node.number)
      }      
    }
  })
  return(list("root"=tree$root,"node.number"=tree$node.number))
}

make.mov <- function(){
  path <- dirname(rstudioapi::getSourceEditorContext()$path)
  unlink(paste(path, "/anim/fooplot.gif", sep=""))
  system(paste("convert -delay 40 ", path, "/anim/foo*.jpg ", path ,"/anim/fooplot.gif", sep=""))
}

simulate <- function(lambda=0.05, m=2, maxleaves=40){
  path <- dirname(rstudioapi::getSourceEditorContext()$path)
  unlink(paste(path, "/anim/", sep=""), recursive=TRUE)
  dir.create(paste(path, "/anim/", sep=""))
  jpeg(paste(path, "/anim/foo%02d.jpg", sep=""), width=500, height=500)
  tree <- initTree(m)
  n <- numeric(maxleaves)
  t <- 0
  while(tree$root$leafCount <= maxleaves){
    t <- t + 1
    n[t] <- tree$root$leafCount
    plot(as.phylo(tree$root), no.margin=TRUE, edge.width=2)
    tree <- addChildren(tree, lambda=lambda, m=m)
  }
  dev.off()
  make.mov()
  
  pdf(paste(path, "/plot.pdf", sep=""))
  plot(1:t, n[1:t], t="l", xlab="t", ylab="N")
  lines(1:t, exp(lambda*1:t), col="red")
  dev.off()
}

simulate(lambda=0.1, m=2, maxleaves=50)
