K = 2
L = array(0, dim = c(6, K))
L[1:4, 1] = 1
L[3:6, 2] = 1
Psi = diag(c(2,2,1,1,1,1))*0.5

N = 200
F = array(rnorm(K*N), dim = c(K, N))
library(mvtnorm)
U = t(rmvnorm(N,rep(0,6), sigma = Psi ))
X = t(L %*% F + U)
factanal(X, factors = 2)

factanal(X, factors = 1)
factanal(X, factors = 3)
factanal(X, factors = 4)

factanal(X, factors = 1, rotation = "promax")

fa = factanal(X, factors = 2, scores = "regression")
scores = fa$scores


df = data.frame(Factor1=scores[,1], Factor2=
                  scores[,2], Num = 1:nrow(X))
ggplot(df,aes(x=Factor1,y=Factor2))+geom_text(aes(label=Num))+
  theme_bw() + ggtitle("Factor Analysis Scores")

diss = dist(X, method = "manhattan")
mds1 = cmdscale(diss, k=2)
q1 = qplot(x=mds1[,1], y = mds1[,2], label=1:nrow(X), geom="text", main = "Manhattan")

diss = dist(X, method = "maximum")
mds2 = cmdscale(diss, k=2)
q2 = qplot(x=mds2[,1], y = mds2[,2], label=1:nrow(X),geom="text", main = "Max")

diss = dist(X, method = "euclidean")
mds3 = cmdscale(diss, k=2)
q3 = qplot(x=mds3[,1], y = mds3[,2], label=1:nrow(X), geom="text", main = "Euclidean")

grid.arrange(q1, q2, q3, ncol = 2)

