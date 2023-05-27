# Load auxiliary library
library(MASS)

# hpfilt(y, lambda)
# Computes the Hodrick-Prescott (HP) trend component of series y for some lambda value, namely:
#   lambda = 6.25 or 100 for yearly data;
#   lambda = 1600 for quarterly data (default value);
#   lambda = 14400 for monthly data.
# ... following the computational strategy of Kim et al. (2009).
# It returns a time series object with the original data y filtered.
hpfilt <- function(y, lambda = 1600){
  n <- length(y)
  V <- rep(0, n)
  V[1] <- 1
  V[2] <- -2
  V[3] <- 1
  T <- toeplitz(V)
  T[lower.tri(T)] <- 0
  m <- n-2
  D <- T[1:m,]
  D2 <- crossprod(D)
  A <- solve( diag(n) + lambda * D2 )
  x <- A %*% y
  filtered_series <- ts(data = x, start = 1, end = n)
  # Calculate error
  error <- y - x
  # Calculate Mean Absolute Error (MAE)
  mae <- sum(abs(error)) / n
  # Calculate Mean Squared Error (MSE)
  mse <- sum((error)^2) / n
  # Calculate Root Mean Squared Error (RMSE)
  rmse <- sqrt(mse)
  # Return filtered series, MAE, MSE and RMSE as a list
  out <- list(filtered_series = filtered_series, MAE = mae, MSE = mse, RMSE = rmse)
  return(out)
}


# AR1(y)
# One-step look ahead forecast computed with a simple auto-regressive model of order 1.
AR1 <- function(y){
  n <- length(y)
  y0 <- y[1:(n-1)]
  y1 <- y[2:n]
  mod <- lm(y1 ~ y0)
  yhat <- mod$coefficients[1] + mod$coefficients[2]*y[n]
}


# GTD2(ts,p,d,w,beta,c)
# Simple "gradient temporal difference learning, version 2" algorithm with linear function approximation and quadratic polynomial basis (Szepesvari, 2010, p. 26).
# For each transition between x and y, the algorithm computes:
#
#   u     <- y - x
#   delta <- p * x + w - 0.5 d * u^2 + r1 * (beta * y - x) + 0.5 r2 * (beta * y^2 - x^2)
#   a0    <- w0
#   a1    <- w1 * x
#   a2    <- w2 * x^2
#   r0    <- r0 + alpha * a0
#   r1    <- r1 + alpha * a1 * (x - beta * y)
#   r2    <- r2 + alpha * a2 * (x^2 - beta * y^2)
#   w0    <- w0 + alpha * (delta - a0)
#   w1    <- w1 + alpha * (delta - a1) * x
#   w2    <- w2 + alpha * (delta - a2) * x^2
#
# starting from the initial state x = ts[1].
# It returns a bundle (list) with: 
#   - the estimated parameters r0, r1 and r2 of the value function V(x);
#   - the linear coefficients a and b of the one-step look-ahead policy mu(x) = a + b * x.

GTD2 <- function(ts,p=1,d=1,w=0,beta=0.96,c=0.0000001){
  n <- length(ts)
  r0 <- 0
  r1 <- 0
  r2 <- 0
  w0 <- 0
  w1 <- 0
  w2 <- 0
  if (length(w) == 1) w <- rnorm(n,0,w)
  # Value function V(x)
  for (i in 1:(n-1)){
    x <- ts[i]
    y <- ts[i+1]
    u <- y - x
    delta <- p * x + w[i] - d/2 * u^2 + r1 * (beta * y - x) + r2/2 * (beta * y^2 - x^2)
    a0    <- w0
    a1    <- w1 * x
    a2    <- w2 * x^2
    alpha <- c / i
    r0 <- r0 + alpha * a0
    r1 <- r1 + alpha * a1 * (x - beta * y)
    r2 <- r2 + alpha * a2 * (x^2 - beta * y^2)
    w0    <- w0 + alpha * (delta - a0)
    w1    <- w1 + alpha * (delta - a1) * x
    w2    <- w2 + alpha * (delta - a2) * x^2
  }
  # Policy function mu(x)
  a <- beta * r1 / (d - beta * r2)
  b <- beta * r2 / (d - beta * r2)
  list("r0" = r0, "r1" = r1, "r2" = r2, "a" = a, "b" = b)
}


# td0LFA(ts,p,d,w,beta,c)
# Simple temporal difference TD(0) algorithm with linear function approximation and quadratic polynomial basis (Szepesvari, 2010, pp. 12, 18).
# For a single real-valued state x, it estimates from data the linear value or reward-to-go function V(x) with quadratic polynomial basis phi(x), given:
#   - a time series ts of n real-valued numbers, where each observation represents the state of the system (e.g. stock of capital) at time t = 1, ..., n;
#   - an immediate reward p associated with the last state x (p = 1 by default);
#   - a quadratic adjustment cost d associated with the transition from the last state x to the next state y, that is, with u = y - x (e.g. investment);
#   - a sequence w of n random "disturbances", N(0,1) by default, or the given values for an exogenous (not controlled) state (e.g. depreciation with w < 0);
#   - a discount factor beta (= 0.96 by default);
#   - a parameter c > 0 for the step-size sequence coefficient alpha = c/t (c = 1 by default).
# For each transition between x and y, the algorithm computes:
#   u     <- y - x
#   delta <- p * x + w - 0.5 d * u^2 + r1 * (beta * y - x) + 0.5 r2 * (beta * y^2 - x^2)
#   r0 <- r0 + alpha * delta
#   r1 <- r1 + alpha * delta * x
#   r2 <- r2 + alpha * delta * x^2
# starting from the initial state x = ts[1].
# It returns a bundle (list) with: 
#   - the estimated parameters r0, r1 and r2 of the value function V(x);
#   - the linear coefficients a and b of the one-step look-ahead policy mu(x) = a + b * x.
td0LFA <- function(ts,p=1,d=1,w=0,beta=0.96,c=0.0000001){
  n <- length(ts)
  r0 <- 0
  r1 <- 0
  r2 <- 0
  if (length(w) == 1) w <- rnorm(n,0,w)
  # Value function V(x)
  for (i in 1:(n-1)){
    x <- ts[i]
    y <- ts[i+1]
    u <- y - x
    delta <- p * x + w[i] - d/2 * u^2 + r1 * (beta * y - x) + r2/2 * (beta * y^2 - x^2)
    alpha <- c / i
    r0 <- r0 + alpha * delta
    r1 <- r1 + alpha * delta * x
    r2 <- r2 + alpha * delta * x^2
  }
  # Policy function mu(x)
  a <- beta * r1 / (d - beta * r2)
  b <- beta * r2 / (d - beta * r2)
  list("r0" = r0, "r1" = r1, "r2" = r2, "a" = a, "b" = b)
}


# tdLambdaLFA(ts,p,d,w,beta,c,lambda)
# Simple temporal difference TD(lambda) algorithm with linear function approximation and quadratic polynomial basis (Szepesvari, 2010, pp. 16-18, 22).
# For each transition between x and y, the algorithm computes:
#   u     <- y - x
#   delta <- p * x + w - 0.5 d * u^2 + r1 * (beta * y - x) + 0.5 r2 * (beta * y^2 - x^2)
#   z  <- x + beta * lambda * z
#   r0 <- r0 + alpha * delta
#   r1 <- r1 + alpha * delta * z
#   r2 <- r2 + alpha * delta * z^2
# starting from the initial state x = ts[1].
# It returns a bundle (list) with: 
#   - the estimated parameters r0, r1 and r2 of the value function V(x);
#   - the linear coefficients a and b of the one-step look-ahead policy mu(x) = a + b * x.
tdLambdaLFA <- function(ts,p=1,d=1,w=0,beta=0.96,c=0.0000001,lambda=0.5){
  n <- length(ts)
  r0 <- 0
  r1 <- 0
  r2 <- 0
  z <- 0
  if (length(w) == 1) w <- rnorm(n,0,w)
  # Value function V(x)
  for (i in 1:(n-1)){
    x <- ts[i]
    y <- ts[i+1]
    u <- y - x
    delta <- p * x + w[i] - d/2 * u^2 + r1 * (beta * y - x) + r2/2 * (beta * y^2 - x^2)
    z <- x + beta * lambda * z
    alpha <- c / i
    r0 <- r0 + alpha * delta
    r1 <- r1 + alpha * delta * z
    r2 <- r2 + alpha * delta * z^2
  }
  # Policy function mu(x)
  a <- beta * r1 / (d - beta * r2)
  b <- beta * r2 / (d - beta * r2)
  list("r0" = r0, "r1" = r1, "r2" = r2, "a" = a, "b" = b)
}

# cv.td0LFA(y,p,w,beta,c,k)
# Simple k-fold cross validation for a temporal difference TD(0) algorithm with linear function approximation and quadratic polynomial basis.
cv.td0LFA <- function(y,p=1,w=0,beta=0.96,c=0.00001,k=5){
  n <- length(y)
  m <- n - k
  nw <- length(w)
  y <- as.matrix(y)
  pred  <- rep(0,k)
  error <- rep(0,k)
  mae_aux <- 1000000000
  for (d in 1:20){
    for (i in 1:k){
      ytrain <- y[i:(m+i-1),1]
      wtrain <- ifelse(nw == 1, w, w[i:(m+i-1),1])
      mod <- td0LFA(ytrain,p,d/10,wtrain,beta,c)
      pred[i]  <- mod$a + (1 + mod$b) * y[m+i-1]
      error[i] <- y[m+i] - pred[i]
    }
    mae <- sum(abs(error))/k
    mse <- sum(error*error)/k
    if (mae <= mae_aux){
        pred_aux <- pred
        mae_aux  <- mae
        mse_aux  <- mse
        d_aux    <- d/10
    }
  }
  list("prediction" = pred_aux, "MAE" = mae_aux, "MSE" = mse_aux, "cost" = d_aux)
}


# cv.tdLambdaLFA(y,p,w,beta,c,lambda,k)
# Simple k-fold cross validation for a temporal difference TD(lambda) algorithm with linear function approximation and quadratic polynomial basis.
cv.tdLambdaLFA <- function(y,p=1,w=0,beta=0.96,c=0.0000001,lambda=0.5,k=5){
  n <- length(y)
  m <- n - k
  nw <- length(w)
  y <- as.matrix(y)
  pred  <- rep(0,k)
  error <- rep(0,k)
  mae_aux <- 1000000000
  for (d in 1:20){
    for (i in 1:k){
      ytrain <- y[i:(m+i-1),1]
      wtrain <- ifelse(nw == 1, w, w[i:(m+i-1),1])
      mod <- tdLambdaLFA(ytrain,p,d/10,wtrain,beta,c,lambda)
      pred[i]  <- mod$a + (1 + mod$b) * y[m+i-1]
      error[i] <- y[m+i] - pred[i]
    }
    mae <- sum(abs(error))/k
    mse <- sum(error*error)/k
    if (mae <= mae_aux){
        pred_aux <- pred
        mae_aux  <- mae
        mse_aux  <- mse
        d_aux    <- d/10
    }
  }
  list("prediction" = pred_aux, "MAE" = mae_aux, "MSE" = mse_aux, "cost" = d_aux)
}


# cv.GTD2(y,p,w,beta,c,k)
# Simple k-fold cross validation for a "gradient temporal difference learning, version 2" algorithm with linear function approximation and quadratic polynomial basis.
# TIP: calibrate c to give a reasonable min(MAE).
cv.GTD2 <- function(y,p=1,w=0,beta=0.96,c=0.00001,k=5){
  n <- length(y)
  m <- n - k
  nw <- length(w)
  y <- as.matrix(y)
  pred  <- rep(0,k)
  error <- rep(0,k)
  mae_aux <- 1000000000
  for (d in 1:20){
    for (i in 1:k){
      ytrain <- y[i:(m+i-1),1]
      wtrain <- ifelse(nw == 1, w, w[i:(m+i-1),1])
      mod <- GTD2(ytrain,p,d/10,wtrain,beta,c)
      pred[i]  <- mod$a + (1 + mod$b) * y[m+i-1]
      error[i] <- y[m+i] - pred[i]
    }
    mae <- sum(abs(error))/k
    mse <- sum(error*error)/k
    if (mae <= mae_aux){
        pred_aux <- pred
        mae_aux  <- mae
        mse_aux  <- mse
        d_aux    <- d/10
    }
  }
  list("prediction" = pred_aux, "MAE" = mae_aux, "MSE" = mse_aux, "cost" = d_aux)
}


# cv.hp(y, lambda, k)
# Simple k-fold cross validation for an univariate time series model with the trend computed by the HP filter.
# Returns a list with k OOS predictions and the associated errors (MAE and MSE) and AIC (Greene approach).
cv.hp <- function(y, lambda=1600, k=4){
    n <- length(y)
    m <- n - k
    y <- as.matrix(y)
    pred  <- rep(0,k)
    error <- rep(0,k)
    for (i in 1:k){
        ytrain <- y[i:(m+i-1),1]
        trend <- hpfilt(ytrain, lambda)
        pred[i] <- y[m+i-1] + tail(trend$filtered_series, 1) - tail(trend$filtered_series, 2)
        
        error[i] <- y[m+i] - pred[i]
    }
  mae <- sum(abs(error))/k
  mse <- sum(error*error)/k
  aic <- log(sum(error*error)/k) + 2/k
  list("prediction" = pred, "MAE" = mae, "MSE" = mse, "AIC" = aic)
}


# cv.AR1(y, k)
# Simple k-fold cross validation with a sliding window for the AR(1) model.
# Returns a list with k OOS predictions and the associated errors (MAE and MSE) and AIC (Greene approach).
cv.AR1 <- function(y, k=4){
  y <- as.matrix(y)
  n <- length(y)
  m <- n - k
  pred  <- rep(0,k)
  error <- rep(0,k)
  for (i in 1:k){
    ytrain <- y[i:(m+i-1),1]
    pred[i]  <- AR1(ytrain)
    error[i] <- y[m+i] - pred[i]
  }
  mae <- sum(abs(error))/k
  mse <- sum(error*error)/k
  rmse <- sqrt(mse)
  aic <- log(sum(error*error)/k) + 2/k
  list("prediction" = pred, "MAE" = mae, "MSE" = mse, "RMSE" = rmse, "AIC" = aic)
}


