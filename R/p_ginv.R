p_ginv <-
function(X,p){
  X.svd = svd(X);
  X.rank = sum(X.svd$d>1e-8);
  X.value = X.svd$d[1:X.rank]^(-1*p);
  if (length(X.value)==1){
    D = as.matrix(X.value);
  }else{
    D = diag(X.value);
  }
  
  rlt = X.svd$v[,1:X.rank] %*% D %*% t(X.svd$u[,1:X.rank]);
  return(rlt);
}
