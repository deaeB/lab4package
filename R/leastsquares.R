testfunction <- function(formula, data){
  f <-  formula
  return(f)
}



leastsquares <- function(formula, data){
  X <- model.matrix(formula, data = data)
  # matrix X (independent variables)
  y <- as.matrix(data[all.vars(formula)[1]])
  # dependent variable y
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y 
  # Regressions coefficients: coef()
  y_hat <- X %*% beta_hat
  # The fitted values pred()
  e_hat <- y - y_hat
  # The residuals: resid()
  n <- nrow(X)
  # the number of observations 
  # X has to be vertical
  p <- length(all.vars(formula)) 
  # the number of parameters 
  df <- n - p
  # The degrees of freedom:
  sigma_hat_sqr <- (t(e_hat) %*% e_hat) / df
  # The residual variance:
  var_beta_hat <- as.numeric(sigma_hat_sqr) * solve(t(X) %*% X)
  # The variance of the regression coefficients:
  t_beta <- beta_hat / sqrt(var_beta_hat)
  # The t-values for each coefficient:
  p_beta <- pt(beta_hat, df)
  # the p-values for each regression coefficient
}