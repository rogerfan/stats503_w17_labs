
# Load data
train_x = read.table('./data/mnist_train_x.txt')
train_y = read.table('./data/mnist_train_y.txt')
test_x = read.table('./data/mnist_test_x.txt')
test_y = read.table('./data/mnist_test_y.txt')

# Look at vector
as.numeric(train_x[1,])

# Write function to visualize
plot_im = function(vec) {
    temp = matrix(as.numeric(vec), nrow=28)
    temp = temp[,28:1]
    image(1:28, 1:28, temp, col=gray((0:255)/255))
}

plot_im(train_x[224,])
train_y[224,]


# Google "neural net r"
#   Look through some of the pages
#   Package options: neuralnet, nnet, caret, etc.
#   Look at neuralnet documentation

install.packages('neuralnet')
require(neuralnet)

?neuralnet

# Read through some of the documentation
#   In particular note
#       data: data must come as a data frame
#       hidden: choosing hidden node structure
#       algorithm: choosing backprop vs other algorithms
#       linear.output: output nodes are regression or binary
#   Look at "See Also", note plot.nn, compute

train_y_ind = model.matrix(~factor(train_y$label)-1)
colnames(train_y_ind) = paste0('out', 0:9)

train = cbind(train_x, train_y_ind)
y_names = paste0('out', 0:9)
x_names = paste0('V', 1:784)

nn = neuralnet(
    paste(paste(y_names, collapse='+'),
          '~',
          paste(x_names, collapse='+')),
    train[1:10000,],
    hidden=c(30),
    linear.output=FALSE,
    lifesign='full', lifesign.step=100)


# Look at compute documentation

yhat = compute(nn, train_x[1:200,])$net.result
yhat = apply(yhat, 1, which.max)-1

pred = function(nn, dat) {
    yhat = compute(nn, dat)$net.result
    yhat = apply(yhat, 1, which.max)-1
    return(yhat)
}

mean(pred(nn, train_x[1:10000,]) != train_y[1:10000,])
mean(pred(nn, test_x) != test_y)





