
# nicked from the redress github page and adapted to use sparse matrices
pearsonres <- function(model, cond = TRUE){
  
  browser()
  
  X = model@pp$X
  n <- dim(X)[1]
  re_df <- as.data.frame(lme4::VarCorr(model))
  diag_n <- Matrix::sparseMatrix(1:n, 1:n, x = 1)
  R <- (subset(re_df, re_df$grp == "Residual")$sdcor)^2 * diag_n
  
  # Compute the conditional Pearson residuals
  if (cond == TRUE){
    
    res <- rawres(model, cond = TRUE) / sqrt(diag(R))
    
    # Compute the marginal Pearson residuals
  } else {
    
    Z = t(model@pp$Zt)
    p <- dim(X)[2]
    q <- dim(Z)[2]
    diag_q <- Matrix::sparseMatrix(1:q, 1:q, x = 1)
    G <- (subset(re_df, re_df$grp != "Residual")$sdcor)^2 * diag_q
    V = Z %*% G %*% t(Z) + R
    res <- as.vector(rawres(model, cond = FALSE) / sqrt(diag(V)))
    
  }
  
  # Return the computed residuals
  return(res)
  
}