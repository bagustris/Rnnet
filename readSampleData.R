# read the sample data
sample <- read.table("Sample1.txt",header=FALSE)
Nsample <- dim(sample)[1]
print(head(sample))
plot(c(0,1),c(0,1))
v <- netsays(t(sample[,-1]))
p <- sample[order(v),1]
nc <- sum(sample[,1]==0)
nd <- Nsample-nc
nd
nnc <- nc
nnc
nnd <- nd
nnd
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
if(itype==0) {nc <- nc+1;vc[nc] <- isay} else {nd<- nd+1;vd[nd] <- isay}
}
hc <- hist(vc, breaks =seq(0,1,.05))
hd <- hist(vd,breaks=seq(0,1,.05))
