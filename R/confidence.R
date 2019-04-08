#' Plot confidence intervals for variables from your linear regression model output
#' @description This function takes the linear regression model you estimate with lm() for your final
#' projects and returns a coefficient plot appropriate for you to use in your poster or final paper.
#'
#' @param model Model takes the object name of your linear regression model. Example - if you run the
#' following command: m1 <- lm(y~x), then enter m1 for this argument.
#'
#' @param var.labels New variable names for the plot to be entered as characters here.
#' Note, this must be the same length as the variables in your model. If you have four variables, you must
#' enter four names. See example code below.
#'
#' @param grid Include vertical grid lines in the plot. Default is True.
#' @param grid.int Spacing interval for grid lines. Default value is 5.
#'
#' @param mai Change plot margins (inches).
#' @param ps Change text size in plot. This will be useful for your posters
#' (30 would be good for a poster, 12 for a paper).
#' @param pt.type Point style of regression parameter point estimate. Default is 16, circle. See ?points
#' for information on different styles.
#' @param pt.size Side of point estimate, default is 1.5.
#' @param pt.color Color of point estimate, default is 'red', Any color argument will work here.
#' @param lwd Line width for confidence intervals, default is 1.5.
#' @param lty Line type for confidence intervals, default is 2 (dashed line). See the lty argument
#' in ?par for additional details on line types.
#'
#' @examples
#' # Estimate a regression using 'mtcars' data:
#' m <- lm(mpg~disp+hp+wt,data=mtcars)
#' confidence(m)
#'
#' # Plot without intercept:
#' confidence(m,vars=c('disp','hp','wt'))
#'
#' # Plot with intercept and one covariate:
#' confidence(m,vars=c('(Intercept)','wt'))
#'
#' # Plot with two model variables AND new variable names:
#' confidence(m,vars=c('disp','wt'),var.labels=c('Displacement','Weight'))
#'
#' # Plot with higher confidence interval and legend:
#' confidence(m,level=0.99,legend=TRUE)
#'
#' # Dynamic margins for longer variable names:
#' confidence(m, vars=c('hp','wt'),var.labels=c('A really long name for variable: Horse Power','Weight'))
#' @export





confidence <- function(model,vars=NULL,var.labels = NULL, level=0.95,grid=T,grid.int=5,
                       mai=NULL,ps=12,
                       pt.type=16,pt.size=1.5,pt.color='red',
                       lwd=1.5,lty=2,
                       legend=F){


  #################################################


  # Extract coefficient infromation from model
  if(is.null(vars)) vars <- variable.names(model)

  try(if(any(vars %in% variable.names(model)==F)) stop(paste(cat('\14'),'You have selected variables not in your original model!',
                                                             'Make sure you have typed the variable names as they appear in the',
                                                             'regression output from summary(YourModel)',sep=' '),call.=F))

  res <- summary(model)$coefficients[rownames(summary(model)$coefficients) %in% vars,]
  res <- res[match(vars, rownames(res)),]

  tryCatch(if(!is.null(var.labels)) rownames(res) <- var.labels,
           error=function(e) paste(cat('\14'),'ERROR - Your variables and labels do not match up.'))

  # Estimate confidence intervals (95% are default)
  a <- (1 - level)/2
  a <- c(a,1-a)

  crit <- qt(a, model$df.residual)

  coefs <- data.frame('Var'   = rownames(res),
                      'b'     = res[,1],
                      'b_se'  = res[,1],
                      'b_lci' = res[,1] + crit[1]*res[,2],
                      'b_uci' = res[,1] + crit[2]*res[,2])


  #################################################


  floor_dec   <- function(x, level=1) min(round(x - 5*10^(-level-1), level))
  ceiling_dec <- function(x, level=1) max(round(x + 5*10^(-level-1), level))

  xlim <- c(floor_dec(c(coefs$b_lci,coefs$b_uci),3),ceiling_dec(c(coefs$b_lci,coefs$b_uci),3))

  # Set plotting space and font size
  linch <- max(strwidth(coefs$Var, "inch")+0.4, na.rm = TRUE)
  if(is.null(mai)) mai <- c(1.02,linch,0.82,0.42)

  par(mai=mai,ps=ps)


  #################################################


  # Add a blank base plot with appropriate labels
  plot('',
       xlim   = xlim,
       ylim   =  c(0,nrow(coefs)+1),
       ylab   = '',
       yaxt   = 'n',
       main   = expression(paste('Distribution of regression parameters, ',hat(beta))),
       xlab   = expression(paste(hat(beta), ' estimate')))
  axis(side   = 2,
       las    = 1,
       at     = seq(1,nrow(coefs),1),
       labels = coefs$Var)

  # Grid lines:
  if(grid==T){
    grid(NULL,NA,
         col      = 'lightgray',
         lty      = 'dotted',
         lwd      = 1)}

  # Add confidence intervals and end caps
  segments(x0 = coefs$b_lci,
           y0 = seq(1,nrow(coefs)),
           x1 = coefs$b_uci,
           y1 = seq(1,nrow(coefs)),
           lty= lty,lwd = lwd,lend = 'square')

  segments(x0 = coefs$b_lci,
           y0 = seq(1,nrow(coefs),1)-0.1,
           x1 = coefs$b_lci,
           y1 = seq(1,nrow(coefs),1)+0.1,
           lty= 1,lwd = lwd)

  segments(x0 = coefs$b_uci,
           y0 = seq(1,nrow(coefs),1)-0.1,
           x1 = coefs$b_uci,
           y1 = seq(1,nrow(coefs),1)+0.1,
           lty= 1,lwd = lwd)

  # Add parameter point estimates
  points(x = coefs$b,
         y = seq(1,nrow(coefs),1),
         pch=pt.type,
         cex=pt.size,
         col=pt.color)

  # Add legend
  if(legend==T){
    legend('topright',
           legend=c('Point Estimate',paste(level*100,'% CI')),
           pch=c(pt.type,NA),pt.cex=c(pt.size,NA),
           lty=c(NA,2),lwd=c(NA,3),
           col=c(pt.color,'black'),xpd=T)}

}

###################################################################################################
