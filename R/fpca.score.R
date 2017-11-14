fpca.score <-
function(x,pos=NULL,gename,percentage,nbasis){
  ninds <- dim(x)[1]
  nsnps <- dim(x)[2]
  if ( is.null(pos) ){
    pos <- (0:( nsnps-1) )/(nsnps-1)
  }else {
    idx<-order(pos)
    x<-x[,idx]
    pos<-pos[idx]
    pos<- (pos-pos[1])/(pos[nsnps]-pos[1])
  }
  
  expanded<- fourier.expansion.smoothed(x,nbasis,pos,0.1);
  
  coef<-t(expanded$coef-rowMeans(expanded$coef))/sqrt(ninds)
  pca.rlt<-prcomp(coef)
  pca.rlt$scores<-coef%*%pca.rlt$rotation
  ksi<-expanded$phi%*%pca.rlt$rotation
  v0<-diag(var(pca.rlt$scores))
  prop<-cumsum(v0)/sum(v0)
  x_sn<-as.vector(which(prop>percentage)[1])###number of scores that can explain variance larger than setted percentage
  names(x_sn)<-gename
  x_score<-pca.rlt$scores[,1:x_sn]
  x_score<-as.matrix(x_score)
  x_prop<-prop[1:x_sn]
  x_ksi<-ksi[,1:x_sn]
  x_ksi<-as.matrix(x_ksi)
  if(x_sn==1){
    colnames(x_score)<-gename
    names(x_prop)<-gename
    colnames(x_ksi)<-gename
  }
  else{
    colnames(x_score)<-paste(gename,1:x_sn,sep="_")
    names(x_prop)<-paste(gename,1:x_sn,sep="_")
    colnames(x_ksi)<-paste(gename,1:x_sn,sep="_")
  }
  rlt<-list()
  rlt$score<- as.matrix(x_score);
  rlt$prop<-x_prop
  #rlt$ksi<-x_ksi
  #rlt$xsn<-x_sn
  rlt$eigen <- v0;
  return(rlt)
}
