
## Assignment and Operations

x <- 4
x = 4
y = 3.5
z = 'hello'

res = 3*x^2/y + 2


# Vectors

vec1 = c(1, 3, 5)
c('a', 'b', 'c')
c(vec1, 6, 7, c(8, 9))

rep(2, 5)
rep(c(1, 2), 3)
seq(1, 8, len=10)

vec2[2:6]
vec2[c(2, 3, 4])
vec2[-1:-5]


sqrt(vec1+x)
vec1 / c(2, 3, 4)
log(vec1) + exp(x)


length(vec1)
min(vec1)
mean(vec1)
sum(vec1)
cumsum(vec1)


# Logical operations

a = TRUE
b = c(TRUE, FALSE)
a
!a
!b

x = c(1, 3, 5)
y = c(2, 3, 4)
x == y
x != y
x > y
x >= y

c = c(TRUE, TRUE, FALSE)
d = c(TRUE, FALSE, FALSE)
c & d
c | d
(3>2) | (1 == 0)
(3>2) & (1 == 0)

vec2 = seq(1, 30, 3)
bool = rep(c(TRUE, FALSE), 5)
vec2[bool]


## Data Types

# Lists

l1 = list(1, 'hi', c(1, 3, 5))
l1[[3]]

l2 = list('a'=1, 'b'='hi', 'c'=c(1, 3, 5))
l2$c
l2[['c']]

l2$d = TRUE
length(l2)


# Matrices

a = matrix(c(1, 2, 3, 4, 5, 6), ncol=2, byrow=TRUE)
diag(3)
b = diag(c(2, 3))
matrix(0, nrow=5, ncol=3)
matrix(NA, nrow=5, ncol=3)

a*2
a %*% b
t(a) %*% diag(c(1, 2, 3))
crossprod(a, diag(c(1, 2, 3)))

a[2,]
a[1,2]
a[1:2,]
a[1:2,2]
a[-2,]
a[c(1, 3),]

dim(a)
solve(a[1:2,1:2])
colMeans(a)
rowSums(a)
scale(a)
eigen(a[1:2,1:2])


# Data Frames

df = data.frame(a=1, b=rep(c('t', 'f'), each=3), c=1:6)

df$c
df$b

as.numeric(df$b)
as.character(df$b)

as.matrix(df)
as.matrix(df[,c(1, 3)])

df$d = df$b == 't'

df[!df$d,]
df[df$c %% 2 == 0,]


df2 = data.frame(a=1, b=c('t', 'f'), c=c(8, 10), d=c(TRUE, FALSE))
rbind(df, df2)


## Flow Control

n = 0
for (i in 1:8) {
    if (i %% 2 == 0) {
        print(paste(i, 'even'))
    } else if (i > 4) {
        n = n + 1
    }
}
print(n)

prev = 1
curr = 1
while (curr < 20) {
    temp = curr
    curr = prev + curr
    prev = temp
}
print(curr)


# Functions

myfact = function(n) {
    res = 1
    for (i in 2:n) {
        res = res * i
    }
    return(res)
}

myfact = function(n, start=1) {
    res = 1
    for (i in start:n) {
        res = res * i
    }
    return(res)
}
myfact(4)
myfact(4, start=3)

myfunc = function(vec) {
    mean = mean(vec)
    sum = sum(vec)
    list('mean'=mean, 'sum'=sum)
}
res = myfunc(c(1, 3, 5, 7, 9))


## Using Data

data = read.csv('data/salaries.csv', header=TRUE, sep=',')

summary(data)
mean(data$salary)
mean(data$salary, na.rm=TRUE)

data = data[!is.na(data$salary),]
data = na.omit(data)
#data$salary[is.na(data$salary),] = 0
summary(data)

mean(data$salary[data$sex == 'Male' & data$rank == 'AsstProf'])
mean(data$salary[data$sex == 'Female' & data$rank == 'AsstProf'])


# Regression and plotting

ols = lm(salary ~ rank + discipline + yrs.since.phd +
                  yrs.service + sex, data=data)
summary(ols)

names(ols)
names(summary(ols))

plot(data$yrs.since.phd, data$salary, col=data$discipline)

#install.packages('ggplot2')
require(ggplot2)
qplot(yrs.since.phd, salary, data=data, colour=discipline, shape=rank)


## Apply functions

a = matrix(c(1, 2, 3, 4, 5, 6), ncol=2, byrow=TRUE)

res = rep(NA, dim(a)[1])
for (i in 1:dim(a)[1]) {
    res[i] = max(a[i,])
}
res

apply(a, 1, max)
apply(a, 2, max)
#vapply, sapply, lapply, etc.

require(matrixStats)
rowMaxs(a)


## Simulation Example

set.seed(14516)

n = 2000
p = 100

beta = rep(0, p)
beta[1:10] = 0.1
X = matrix(rnorm(n*p), ncol=p)
Y = X %*% beta + rnorm(n, sd=0.5)

Y_train = Y[1:1000]
X_train = X[1:1000,]
Y_test = Y[1001:2000]
X_test = X[1001:2000,]


ols = lm(Y_train ~ 0 + X_train)
coef_ols = as.numeric(coef(ols))

#install.packages('glmnet')
require(glmnet)

cvfit = cv.glmnet(X_train, Y_train, intercept=FALSE)
coef_lasso = coef(cvfit, s="lambda.min")[-1]


sumsq = function(x) {
    sum(x^2)
}

trainerr_ols = sumsq(residuals(ols))
testerr_ols = sumsq(Y_test - X_test %*% coef_ols)

trainerr_lasso = sumsq(Y_train - predict(cvfit, X_train, s='lambda.min'))
testerr_lasso = sumsq(Y_test - predict(cvfit, X_test, s='lambda.min'))

trainerr_ols
trainerr_lasso
testerr_ols
testerr_lasso



