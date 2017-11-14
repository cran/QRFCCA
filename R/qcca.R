qcca <-
function(A,A_thres,B,B_thres,Z=NULL){
  if(class(Z)!= "NULL"){A = lm.rm(A,Z)}
  A_lr = qlr(A,A_thres);
  B_lr = qlr(B,B_thres);
  n = nrow(A);
  p = A_lr$rank;
  q = B_lr$rank;
  if (p <= q){
    X = A_lr$lr;
    Y = B_lr$lr;
  }else{
    X = B_lr$lr;
    Y = A_lr$lr;
  }
  R = p_ginv_sq(cov(Y),0.5) %*% cov(Y,X) %*% p_ginv_sq(cov(X),1) %*% cov(X,Y) %*% p_ginv_sq(cov(Y),0.5);
  d = Re(eigen(R)$values);
  k = min(p,q);
  rho = d[1:k]^(0.5);
  rho[rho >= 0.9999999]=0.9999999;
  chisq_p = CCA_chisq_test(rho,n,p,q);
  return(list("rho"=rho,"chisq_p"=chisq_p));
}
