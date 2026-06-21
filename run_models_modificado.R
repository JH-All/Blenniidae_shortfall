
run_models <- function(data){
# Lu & He model, k=tax.per(a+b*sp.per)

  start <- coef(
    nls(
      log(sp.per) ~ tax.per * (a + b * sp.per) * (S.tot - sp.cum),
      data = data,
      start = list(
        a = 0.01,
        b = 0.001,
        S.tot = max(data$sp.cum) * 1.2
      ),
      control = nls.control(maxiter = 200)
    )
  )

names(start) <- NULL
new.model <- gnls(sp.per ~ tax.per * (a + b * sp.per) * (S.tot - sp.cum), 
                  data = data,
                                start=list(S.tot = start[3] , a = start[1] , b = start[2]), 
                                weights=varPower(),verbose=T, # use Power function to link variance with mean
                                control=gnlsControl(returnObject=T,minScale=1e-500, # smaller step size for convergence             
                                                    tolerance=0.001,nlsMaxIter=3)) # default setting is more computational costly

#Joppa's model, k=tax.per(a+b*time)

names(start) <- NULL
joppa.model <- gnls(sp.per~tax.per*(a+b*time)*(S.tot-sp.cum),
                    data= data,
                    start=list(S.tot=start[3],a=start[1],b=start[2]),
                    weights=varPower(),verbose=T,
                    control=gnlsControl(returnObject=T,minScale=1e-500,
                                        tolerance=0.001,nlsMaxIter=3))

#logistic model,k=a+b*sp.cum

logis.model <- gnls(sp.per ~ (a + b * sp.cum) * (S.tot - sp.cum),
                    data = data,
                    start = list(S.tot = start[3],a=start[1],b=start[2]),
                    weights = varPower(),verbose=T,
                    control = gnlsControl(returnObject=T,minScale=1e-500,
                                          tolerance=0.001,nlsMaxIter=3, maxIter = 200))

# negative exponential model,k=a

negexp.model <- gnls(sp.per~a*(S.tot-sp.cum),
                     data = data,
                     start=list(S.tot=start[3],a=2e-05),
                     weights=varPower(),verbose=T,
                     control=gnlsControl(returnObject=T,minScale=1e-500,
                                         tolerance=0.001,nlsMaxIter=3, maxIter = 200))

return(list(new.model = new.model, joppa = joppa.model, logis = logis.model, negexp = negexp.model))

}

