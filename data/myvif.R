## myvif function to calculate VIF for a set of independent variables
myvif <- function(x) {
  nvars = dim(x)[2]
  vif = rep(NA, nvars)
  names(vif) = names(x)
  for (i in 1:nvars) {
    myy = x[,i]
    myx = x[,-i]
    fit = lm(myy ~ ., myx)
    myr2 = summary(fit)$r.squared
    vif[i] = 1 / (1 - myr2)

  }
  return(vif)
}