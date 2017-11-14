lm.rm <-
function(Y,X){
    rlt = as.matrix(lm(as.matrix(Y)~as.matrix(X))$residuals);
    return(rlt);	
}
