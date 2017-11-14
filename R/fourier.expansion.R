fourier.expansion <-
function(x,n_of_basis,pos){
  frange <- c(pos[1], pos[length(pos)])
  rlt=list();
  rlt$fbasis<-create.fourier.basis(frange,nbasis=n_of_basis)
  rlt$phi = eval.basis(pos,rlt$fbasis);
  rlt$coef<-myinv(t(rlt$phi)%*%rlt$phi)%*%t(rlt$phi)%*%t(x)
  return(rlt)
}
