qlr <-
function(X,thres){
  X.svd = svd(X);
  d = X.svd$d - thres;
  k = sum(d > 1e-8);
  if (k==1){
    D = as.matrix(d[1]);
  }else{
    D = diag(d[1:k]);
  }
  X.lr = X.svd$u[ ,1:k] %*% D %*% t(X.svd$v[ ,1:k]);
  rownames(X.lr) = rownames(X);
  colnames(X.lr) = colnames(X);
  return(list("lr"=X.lr,"rank"=k));
}
