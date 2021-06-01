#MLEs
dpdf = function(x,alp,lam,the,log = FALSE){
  loglik = log(alp*lam) - lam*(x) + (alp-1)*log(1 - exp(-lam*x)) + 
    log(1+the*(1-(1 - exp(-lam*x))^alp))- 
    the*((1-exp(-lam*x))^alp)
  
  if (log == FALSE)
    density <- exp(loglik)
  else density <- loglik
  return(density)
}
est = maxlogL(x=data,dist = "dpdf",link = list(over=c("alp","lam","the"),fun=c("log_link","log_link","log_link")),start = c(1,1,1))
summary(est)
logLik(est)


#K-S Distance and p-value
pnew = function(x,alp,lam,the){
  1 - ((1-(1-exp(-lam*x))^alp)/(exp(the*((1-exp(-lam*x))^alp))))
}
v = ks.test(data,"pnew",est$fit$par[1],est$fit$par[2],est$fit$par[3])
v