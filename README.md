output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec1Total ~ factor(Treatment)*Time*Edu +  Edu + Gender + Age + Race + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
