# neural net demo using neuralnet library

library(neuralnet)
data = read.table("Sample1.txt", header=FALSE)
# truth <- data.frame(data[,1])

truth <- data[,1]
# input1 <- V2+V3+V4+V5+V6  
# input2 <- data
  
# df <- data.frame(truth,input1,input2)

#Very basic example
nnet<-neuralnet(truth~V2+V3+V4+V5+V6,data,c(4,5))

#less basic example
nnet<-neuralnet(truth~V2+V3+V4+V5+V6,data,c(8,5),
                lifesign='full',
                algorithm='backprop',
                learningrate=0.05,
                linear.output=FALSE
)

dev.copy(png,'myplot.png')
plot(nnet)
dev.off()

# Nice picture of net
test=compute(nnet,t(c(1,2,3,2,1)))

# how it’s used
test$net.result
