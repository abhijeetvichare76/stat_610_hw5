llr = function(x,y,z,omega){
  fits = sapply(z,compute_f_hat,x,y,omega)
  return(fits)
}

compute_f_hat = function(z,x,y,omega){
  Wz = make_weight_matrix(z,x,omega)
  X = make_predictor_matrix(x)
  f_hat = c(1,z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return (f_hat)
}

cal_W = function(xi,z,omega){
  r = abs(xi-z)/omega
  if (abs(r)<1){
    W_r = (1 - (abs(r)^3))^3
    return (W_r)
  }
  return( as.double(rep(1e7,each = length(z))))
}
make_weight_matrix = function(z,x,omega){
  vector = lapply(x,cal_W,z=z,omega = omega)
  Wz = diag(vector)
  #Wz = c(abs(x-z)/omega)
  return (Wz)
}

make_predictor_matrix = function(x){
  ones = matrix(data = 1,nrow = length(x))
  X = cbind(ones,x)
  return (X)
}