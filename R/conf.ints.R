#' Helper function to construct confidence intervals
#' @description Helper function that builds out user-specified confidence intervals.
#'
#' @param model Model takes the object name of your linear regression model. Example - if you run the
#' following command: m1 <- lm(y~x), then enter m1 for this argument.
#'
#' @param level User-specified confidence interval. Default = 0.95
#' @param vars Variable selection (default all)
#' @param var.labels Variable names if not default
#' @examples
#' # Estimate a regression using 'mtcars' data:
#' m <- lm(mpg~disp+hp+wt+am,data=mtcars)
#' conf.ints(m,level=0.99)
#'





conf.ints <- function(model=model, level=0.95, vars=NULL, var.labels=NULL){
  # Extract coefficient infromation from model
  if(is.null(vars)) vars <- variable.names(model)

  res <- tryCatch(summary(model)$coefficients[rownames(summary(model)$coefficients) %in% vars,],
                  error=function(e) print('ERROR - You selected variables not in your original model!'))

  tryCatch(if(!is.null(var.labels)) rownames(res) <- var.labels,
           error=function(e) print('ERROR - Your variables and labels do not match up.'))

  # Estimate confidence intervals (95% are default)
  a <- (1 - level)/2
  a <- c(a,1-a)

  crit <- qt(a, model$df.residual)

  coefs <- data.frame('Var'   = rownames(res),
                      'b'     = res[,1],
                      'b_se'  = res[,1],
                      'b_lci' = res[,1] + crit[1]*res[,2],
                      'b_uci' = res[,1] + crit[2]*res[,2])

  return(coefs)

}
