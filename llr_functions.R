llr = function(x,y,z,omega){
  fits = sapply(z,compute_f_hat,x,y,omega)
  return(fits)
}

compute_f_hat = function(z,x,y,omega){
  Wz <- make_weight_matrix(z,x,omega)
  X = make_predictor_matrix(x)
  Y = as.matrix(y)
  f_hat = c(1,z) %*% solve(t(X) %*% sweep(X,2,Wz,'*')) %*% t(X) %*% apply(Y,2,'*',Wz)
  return (f_hat)
}

cal_W = function(xi,z,omega){
  r = abs(xi-z)/omega
  if (abs(r)<1){
    W_r = (1 - (abs(r)^3))^3
    return (W_r)
  }
  return( as.double(rep(1e-7,each = length(z))))
}
make_weight_matrix = function(z,x,omega){
  vector = lapply(x,cal_W,z=z,omega = omega)
  return (as.numeric(vector))
}

make_predictor_matrix = function(x){
  ones = matrix(data = 1,nrow = length(x))
  X = cbind(ones,x)
  return (X)
}