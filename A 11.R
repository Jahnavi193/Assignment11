tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { outlier.vec[i] <- all(outliers[i,]) }
  return(outlier.vec)
}

a <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
debug(tukey_multiple)
tukey_multiple(a)

