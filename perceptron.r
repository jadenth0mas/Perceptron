library(ggplot2)
library(geometry)

create_seperable <- function(n, seed=NULL) { # add dim and consider returning coefs
  if (!is.null(seed)) {
    set.seed(seed)
  }
  coefs <- c(runif(1,-1,1), runif(1,-1,1))
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  df <- data.frame(x1, x2)
  
  df$y <- as.factor(ifelse(x2-(coefs[1]*x1)>coefs[2], 1, -1))
  return(df)
}

# Consider plotting function for seperable data

perceptronTrain <- function(x, y, lr, max_iters) {
  x$bias <- 1 
  #adds the 1 for the bias term
  # so equation for dot product would be x_b[1] * bias coefficient + x_b[2] * first
  # variable coefficeint +...
  weights <- c(rep(0, ncol(x)))
  any_misclassified <- T
  j <- 0
  while (any_misclassified && j < max_iters) {
    any_misclassified <- F
    for (i in 1:nrow(x)) {
      # print(dim(t(unlist(x[i,]))))
      # print(dim(weights))
      val <- unlist(x[i,]) %*% weights
      pred <- ifelse(val>0, 1, -1)
      if (y[i]-pred!=0) {
        any_misclassified <- T # Consider adding an accepted error rate instead of needing to classify
        # everything
      }
      weights <- weights + (lr*(y[i]-pred))*(unlist(x[i,]))
    }
    j <- i + 1
  }
  return(list("weights"=weights, "epochs"=i))
}

perceptronPredict <- function(x, weights) {
  x$bias <- 1
  y_pred <- c()
  for (i in 1:nrow(x)) {
    y_pred[i] <-  ifelse((unlist(x[i,]) %*% weights) > 0, 1, -1)
  }
  return(y_pred)
}

correct_rate <- function(y_pred, y_true) {
  return(mean(y_pred==y_true))
}

g <- c(rep(c(0), 8))
g[3]
g <- as.vector(g)
typeof(g)
df1 <- create_seperable(100)
ggplot(df1, aes(x1, x2, col=y)) +
  geom_point()
train <- sample(c(TRUE, FALSE), nrow(df1), replace=T, prob=c(0.8,0.2))
test <- (!train)
X.train <- df1[train,-3]
X.test <- df1[test,-3]
Y.train <- df1[train,3]
Y.test <- df1[test,3]
perc <- perceptronTrain(X.train, as.numeric(Y.train), .1, 100)
perc$epochs
y_pred <- perceptronPredict(X.test, perc$weights)
View(y_pred)
mean(y_pred==Y.test)
summary(y_pred)
