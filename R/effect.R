#' Plot effect for one variable from your linear regression model output
#' @description This function takes the linear regression model you estimate with lm() for your final
#' projects and returns a plot of an effect for a selected variable.
#'
#' @param model The object name of your linear regression model. Example - if you run the
#' following command: model1 <- lm(y~x), then enter 'model1' for this argument.
#'
#' @param var The variable to plot change in predicted values of 'y.' Your 'x' variable.
#'
#' @param level Confidence level, .95 is the default.
#'
#' @param dummyIV Indicator for whether the plotted IV is a dummy variable, default is FALSE
#'
#' @param x.label Option to change x-axis label in plot
#' @param y.label Option to change y-axis label in plot
#'
#' @param grid Option to plot with grid lines, default is FALSE
#'
#' @param rug Option to plot with rug to represent data density in plot, default is FALSE
#'
#' @param color TO FINISH -- option to change polygon colors
#' @param line.width TO FINISH -- option to change fit regression line thickness
#' @param title TO FINISH -- option to add plot title
#'
#'
#' @examples
#' # Estimate a regression using 'mtcars' data:
#' m <- lm(mpg~disp+hp+wt+am,data=mtcars)
#'
#' # Plot effect of 'wt' variable
#' effect(m,'wt')
#'
#' # Plot effect of dummy variable, 'am'
#' effect(m,'am',dummyIV=TRUE)
#'
#' # Plot with user-defined x-axis label
#' effect(m,'wt',x.label='Vehicle Weight')
#'
#' # Plot with grid lines and rug
#' effect(m,'wt',x.label='Vehicle Weight', grid=TRUE,rug=TRUE)


effect <- function(model,var,level=0.95,dummyIV=F,
                   x.label=NULL,y.label=NULL,title=NULL,
                   grid=F,rug=F) {

  # Model values
  b <- model$coefficients
  n <- nrow(model.matrix(model))
  k <- length(b)

  # Model data
  d <- tryCatch( {mean.mat(model,var)}, error=function(x) {stop(paste('You have selected a variable not in your original model. Check your spelling.'),call.=F)} )
  x <- d[var][,1]
  y <- model.frame(model)[,1]

  # Calculate critical t-value based on user defined level
  t.val <- qt(((1-level)/2)+level, n - k)



  # REGRESSION QUANTITIES OF INTEREST ###############################

  # SSE and MSE
  sse <- sum((y - model$fitted.values)^2)
  mse <- sse / (n - k)

  # Predicted values
  y.fit <- as.numeric(as.matrix(d) %*% b)

  # Standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - k)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))


  # Calculate lower and upper bounds of predicted values
  slope.upper <- y.fit + t.val * se
  slope.lower <- y.fit - t.val * se


  # Store predicted values and ses in ordered data frame
  d <- data.frame('x'=x,
                  'y.fit'=y.fit,
                  'y.lci'=slope.lower,
                  'y.uci'=slope.upper)

  d <- d[order(x), ]



  # PLOT ############################################################

  # Set up plot environment

  if(dummyIV==F){
    with(d,plot(x='',y='',
                xlim=c(min(x),max(x)),
                ylim=c(min(y.lci),max(y.uci)),
                xlab=ifelse(!is.null(x.label),x.label,'IV'),
                ylab=ifelse(!is.null(y.label),y.label,'DV'),
                main=ifelse(!is.null(title),title,''),
                yaxt='n'))
    axis(2,las=1)
    if(grid==T) grid()
    if(rug==T)  rug(x)

    # Add confidence intervals
    with(d,polygon(c(x,rev(x)),c(y.lci,rev(y.uci)),col="lightgray",border=NA))

    # Add predicted values line
    with(d,lines(x,y.fit))
  }

  else{
    d <- unique(d)

    with(d, plot(x='',y='',
                 xlim=c(-0.5,1.5),
                 ylim=c(min(y.lci),max(y.uci)),
                 xlab=ifelse(!is.null(x.label),x.label,'IV'),
                 ylab=ifelse(!is.null(y.label),y.label,'DV'),
                 xaxt='n',yaxt='n'))
    axis(1,at=c(0,1));axis(2,las=1)

    if(grid==T) grid()

    with(d,segments(x0=x,y0=y.lci,
                    x1=x,y1=y.uci,
                    lty=1,lwd=2))

    with(d,segments(x0=x-0.05,y0=y.lci,
                    x1=x+0.05,y1=y.lci,
                    lty=1,lwd=2))

    with(d,segments(x0=x-0.05,y0=y.uci,
                    x1=x+0.05,y1=y.uci,
                    lty=1,lwd=2))

    with(d,points(x=x,y=y.fit,pch=19,col='red',cex=2))

  }
}


