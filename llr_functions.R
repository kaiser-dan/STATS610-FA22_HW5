llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

make_weight_matrix = function(z,x,omega) {
  x.shifted <- (1/omega)*abs(x-z)
  x.shifted.sub_unit_norm <- x.shifted < 1
  W.diag <- x.shifted.sub_unit_norm * (1-x.shifted^3)^3
  W <- diag(W.diag)
  return(W)
}

make_predictor_matrix = function(x) {
  X <- matrix(1, nrow=length(x), ncol=2)
  X[,2] <- x
  return(X)
}