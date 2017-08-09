## Neural Network demo for 
# Tell a camel from a dromedary

ALPHA = 0.05 # learning parameter

# 5 inputs, 2 hidden layers, with 7 and 10 nodes , 1 output
nodes=c(5,8,5,1)
nlayers=length(nodes) - 1

# 3 sets of weights
net=list() # set up empty list

# net[[ j ]] holds weight matrix feeding nodes of layer j+1 from nodes in layer j

# make weights and fill with random numbers
for(j in 1:nlayers) 
  net[[ j ]] <- matrix(runif(nodes[ j ]*nodes[ j +1 ]),nodes[j+1],nodes[j])
  netsays <- function(x) { # Returns net output for some input vector x
    for(j in 1:nlayers) x <- 1/(1+exp(-net[[ j ]] %*% x))
    return(x)
}

# backprop <- function(layer,n1,n2,factor){ # recursive function used for back-propagation
#   if(layer>1) for(n in 1:nodes[layer-1])
#     backprop(layer-1,n2,n,factor*net[[layer]][n1,n2]*r[[layer]][n2]*(1-r[[layer]][n2]))
#   net[[layer]][n1,n2] <<- net[[layer]][n1,n2] - ALPHA*factor*r[[layer]][n2]
# }

# netlearns <- function(x,truth) { # like netsays but changes weights
#   r <<- list() # to contain the outputs of all nodes in all layers
#   r[[1]] <<- x # the input layer, t(x)
#   for(layer in 1:nlayers) r[[layer+1]] <<- as.matrix(1/(1+exp(-net[[layer]] %*% r[[layer]])))
#   u <- r[[nlayers+1]] # final answer, for convenience
#   for(n in 1:nodes[nlayers]) backprop(nlayers,1,n,(u-truth)*u*(1-u))
# }

backprop <- function(layer,n1,n2,f){ # recursive function used for back-propagation
  if(layer>1)
    for(n in 1:nodes[layer-1]){
      backprop(layer-1,n2,n,f*net[[layer]][n1,n2]*r[[layer]][n2]*(1-r[[layer]][n2]))
    }
  net[[layer]][n1,n2] <<- net[[layer]][n1,n2] - ALPHA*f*r[[layer]][n2]
}

netlearns <- function(x,truth) { # like netsays but changes weights
  r <<- list() # to contain the outputs of all nodes in all layers
  r[[1]] <<- t(x) # the input layer
  for(layer in 1:nlayers) r[[layer+1]] <<- as.matrix(1/(1+exp(-net[[layer]] %*% r[[layer]])))
  u <- r[[nlayers+1]] # final answer, for convenience
  for(n in 1:nodes[nlayers]) backprop(nlayers,1,n,(u-truth)*u*(1-u))
}

# script to process the data with function above
sample <- read.table("Sample2.txt", header=FALSE)
train <- sample[1:800,]
test <- sample[801:1000,]
#output <- sample[,1]
# y <- datf[,2:6]
# input <- data.frame(sample[,2:6])  # input data

# data for training
# input1 <- input[1:800]
# input2 <- input[801:1000]

# netsays(t(x))
# z <- netlearns(truth~V2+V3, datf, c(7,10))
# z <- netlearns(as.matrix(y), truth)

# training data
for(i in 1:800) {
  netlearns(train[i,-1], train[i,1])
}

# testing data
# for(i in 801:1000) {
#  netsays(test[,1]) 
#}

Nsample <- dim(sample)[1]
print(head(sample))
plot(c(0,1),c(0,1))
v <- netsays(t(sample[,-1]))
p <- sample[order(v),1]

nc <- sum(sample[,1]==0)
nd <- Nsample-nc
nnc <- nc
nnd <- nd
for (i in 1:length(p)) {
  if(p[i]==1) {
    nd <- nd-1
  } 
  else {
    nc <- nc-1
  }
  points(nc/nnc,nd/nnd,pch='.') 
}

vc <- rep(0,nnc)
vd <- rep(0,nnd)
nc <- 0
nd <- 0
for (i in 1:Nsample){
  itype <- sample[i,1]
  isay <- netsays(as.numeric(sample[i,-1]))
  if(itype==0) {
    nc <- nc+1;vc[nc] <- isay
  } 
  else {
    nd<- nd+1;vd[nd] <- isay
    }
}

hc <- hist(vc, breaks = seq(0,1,.05))
hd <- hist(vd, breaks = seq(0,1,.05))

# hist(v)

# length(est)
# est