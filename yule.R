library(data.tree)
library(ape)

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
  for(l in leaves){
    if(runif(1, 0.0, 1.0) < lambda) {
      node <- FindNode(tree$root,as.numeric(l$name))
      for(add in 1:m){
        tree$node.number <- tree$node.number + 1
        node$AddChild(tree$node.number)
      }      
    }
  }
  return(list("root"=tree$root,"node.number"=tree$node.number))
}

make.mov <- function(){
  path <- dirname(rstudioapi::getSourceEditorContext()$path)
  unlink(paste(path, "/anim/fooplot.gif", sep=""))
  system(paste("convert -delay 40 ", path, "/anim/foo*.jpg ", path ,"/anim/fooplot.gif", sep=""))
}

simulate <- function(tree, iterations, lambda=0.05, m=2, maxleaves=40){
  path <- dirname(rstudioapi::getSourceEditorContext()$path)
  unlink(paste(path, "/anim/", sep=""), recursive=TRUE)
  dir.create(paste(path, "/anim/", sep=""))
  jpeg(paste(path, "/anim/foo%02d.jpg", sep=""), width=500, height=500)
  n <- numeric(iterations)
  n.max <- iterations
  for(i in 1:iterations){
    n[i] <- tree$root$leafCount
    plot(as.phylo(tree$root), no.margin=TRUE, edge.width=2)
    tree <- addChildren(tree, lambda=lambda, m=m)
    if(tree$root$leafCount >= maxleaves){
      n.max <- i
      break
    }
  }
  dev.off()
  
  make.mov()
  
  pdf(paste(path, "/plot.pdf", sep=""))
  plot(1:n.max, n[1:n.max], t="l", xlab="t", ylab="n")
  dev.off()
}

set.seed(2137)
iterations <- 20
m <- 2
lambda <- 0.2

tree <- initTree(m)
simulate(tree, iterations, lambda=lambda, m=m, maxleaves=100)

