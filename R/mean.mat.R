#' Helper function to construct matrix for effects plot
#' @description Helper function that constructs a matrix of select variable with all others held at their means. Will inheret
#' call options from parent function unless called independently.
#'
#' @param model Model takes the object name of your linear regression model. Example - if you run the
#' following command: m1 <- lm(y~x), then enter m1 for this argument.
#'
#' @param var The variable to plot change in predicted values of 'y.' Your 'x' variable. Will inheret from
#' parent function except when called independently.
#'
#' @examples
#' #' # Estimate a regression using 'mtcars' data:
#' m <- lm(mpg~disp+hp+wt+am,data=mtcars)
#' mean.mat(m,var='wt')



mean.mat <- function(model,var) {
  x <- as.numeric(model.matrix(model)[,var])

  d <- colMeans(model.matrix(model))

  d <- data.frame(as.matrix(t(d),nrow=1))
  colnames(d) <- variable.names(model)

  d <- d[rep(seq_len(nrow(d)), each=length(x)),]
  rownames(d) <- 1:nrow(d)
  d[var] <- x

  return(d)
}





