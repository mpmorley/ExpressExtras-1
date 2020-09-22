#' Runs sva batch correction
#' converts logFC to FC
#' @param dat : matrix of cpms
#' @param mmi : model matrix for fitting the data
#' @param mm0 : null model being compared when fitting the data
#' @param n.sv : number of significant surrogate variables
#' @return Matrix of batch corrected cpms
#' @import sva
#' @export
#' @examples
#' mod <- model.matrix(~as.factor(Disease), data=pData)
#' mod0 <- model.matrix(~1,data=pData)
#' n.sv = num.sv (data, mod,method = "leek")
#' cpms.corr <- svaBatchCor(cpms,mod,mod0,n.sv=n.sv)$corrected

svaBatchCor <- function(dat, mmi, mm0,n.sv=NULL){
  dat <- as.matrix(dat)
  Y <- t(dat)
  #library(sva)
  if(is.null(n.sv))   n.sv <- num.sv(dat,mmi,method="leek")
  o <- svaseq(dat,mmi,mm0,n.sv=n.sv)
  W <- o$sv
  alpha <- solve(t(W) %*% W) %*% t(W) %*% Y
  o$corrected <- t(Y - W %*% alpha)
  return(o)
}
