qcca_p <-
function(A,A_prop,B,B_prop,Z=NULL){
  if(class(Z)!= "NULL"){A = lm.rm(A,Z)}
  A_sv = svd(A)$d;
  B_sv = svd(B)$d;
  if(A_prop==1){
    A_thres = 0;
  }else{
    A_thres = A_sv[which((cumsum(A_sv)/sum(A_sv)) > A_prop)[1]];
  }
  if(abs(A_thres-A_sv[1])<1e-6){A_thres = floor(A_thres)}
  if(B_prop==1){
    B_thres = 0;
  }else{
    B_thres = B_sv[which((cumsum(B_sv)/sum(B_sv)) > B_prop)[1]];
  }  
  if(abs(B_thres-B_sv[1])<1e-6){B_thres = floor(B_thres)}
  qout = qcca(A,A_thres,B,B_thres);
  rlt = list();
  rlt$rho = qout$rho;
  rlt$chisq_p = qout$chisq_p;
  rlt$A_thres = A_thres;
  rlt$B_thres = B_thres;
  return(rlt);
}
