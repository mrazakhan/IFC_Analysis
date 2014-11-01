#MMClassifierHelper Functions


regressionGLM <- function(y,data){
  
  library(plyr)
  
  combos <- combn(ncol(data),1)
  
  models <- list()
  
  #colnos <- seq(1,ncol(data),by=1) 
  
  adply(combos, 2, function(x) {
    print(paste('Variable',colnames(data)[x[1]]))
    
    print ('Calculating Fit')
    fit <- glm(y ~ data[, x[1]],family='binomial')
    
    c(list, c = fit)
    print ('Calculating Summary')
    descriptors <- summary(fit)
    print ('Calculating Corr')
    
    Rsquared=1-fit$deviance/fit$null.deviance
    
    out <- data.frame("feature" = colnames(data)[x[1]]
                      
                      , "r squared" = Rsquared
                      ,"RMSE" =rmse(fit$residuals)
                      
    )
    return(out)
    
  })
  
  
}


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
