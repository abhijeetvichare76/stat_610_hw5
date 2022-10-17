llr = function(x,y,z,omega){
  fits = sapply(z,compute_f_hat,x,y,omega)
  return(fits)
}compute_f_hat = function(z,x,y,omega){
  Wz = make_weight_matrix(z,x,omega)
  X = make_predictor_matrix(x)
  #f_hat = c(1,z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  f_hat = c(1,z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return (f_hat)
}

make_weight_matrix = function(z,x,omega){
#  Wz = diag(abs(x -z) /omega)
  Wz = c(abs(x-z)/omega)
  return (Wz)
}

make_predictor_matrix = function(x){
  ones = matrix(data = 1,nrow = length(x))
  X = cbind(ones,x)
  return (X)
}

