## This is an attempt to port the Kidmap graph type from ConstructMap
## to the eRm package.
## RGA
library(eRm)

## extracts a full theta table along with standard errors for all of
## the items.  Basically, add a theta column to the ppar$theta.table
## argument.
theta <- function(model) {
  lvar <- person.parameter(model)
  result <- lvar$theta.table
  names(result)[1] <- "theta"           #It is a latent variable, not
                                        #a parameter!
  max.se <- do.call(max,c(lapply(lvar$se.theta,max,na.rm=TRUE),
                          na.rm=TRUE))
  result$se <- rep(max.se,nrow(result))
  for (mpat in names(lvar$se.theta)) {
    result[names(lvar$se.theta[[mpat]]),"se"] <- lvar$se.theta[[mpat]]
  }
  result
}

## This recasts a PC data matrix as a matrix of threshold indicators,
## i.e., it expands to a vector of 0/1 indicators corresponding to the
## non-zero levels.  In other words, a 0,1,2 partial credit itme
## will expand to 2 columns and a 0,1,2,3 partial credit to 3 columns
## &c.
extendX <- function (X) {
  do.call("cbind",
          lapply(as.data.frame(X),
                 function (col) {
                   ma <- max(col,na.rm=TRUE)
                   matrix(as.numeric(outer(col,1:ma,">=")),
                          ncol=ma)      #Why does as.numeric drop the
                                        #dim attribute?
                 }))
}

## eRm "helpfully" adds beta to the front of the name of every
## parameter.  Need to undo this.
debeta <- function (pnames) sub("beta ","",pnames)


## This draws a kidmap, a map showing items right and wrong for a
## particular student.
##  itemDat -- raw person by item matrix
## model -- output of RM or similar command
## kid -- an expression which selects a single row of the itemDat
## matrix.
## itemSubset -- an expression which selects a number of coeficients
##  Note that for RSM and PCM, this might have slightly unexpected
##  results (especially PCM, with unequal number of categories)
## itemlabs -- a character vector of the same length selected
## coefficients giving display names for items.
## kidlab -- label for the theta value.
## rlcol, crcol -- color for the reference (theta) line and region.
## pcol, pch -- ploting symbols for the points.
## xlab, ... additional arguments for the plot function.

kidmap <- function (model,kid,itemSubset=1:ncol(model$X),
                    itemlabs=debeta(names(coef(model))[itemSubset]),
                    kidlab="Theta",xlab="",
                    rlcol="slateblue4", crcol = "slateblue1",
                    pcol="black",pch=1,
                    ...) {
  ## calculate 95% interval for score
  kidid <- row.names(model$X)[kid]
  thetas <- theta(model)
  theta.mean <- thetas[kid,"theta"]
  theta.lb <- theta.mean - 2*thetas[kid,"se"]
  theta.ub <- theta.mean + 2*thetas[kid,"se"]

  ## extract the beta parameters
  beta <- -coef(model)
  ## Extend the X matrix to give 0/1 indicators for each level.
  exX <- extendX(model$X)
  colnames(exX) <- names(beta)
  ## Select the subset we will use
  beta <- beta[itemSubset]
  datvec <- exX[kid,itemSubset]

  ## Graph Frame
  plot(datvec,beta,type="n",xlim=c(-2,3),xaxt="n",xlab=xlab,...)
  axis(1,at=c(-1,2),c("Not achieved","Achieved"),tick=FALSE)

  ## reference line for person
  polygon(c(-3,4,4,-3),c(theta.lb,theta.lb,theta.ub,theta.ub),
          col=crcol,border=NA)
  abline(h=theta.mean,col=rlcol)
  text(-1.5,theta.mean,kidlab,pos=3,col=rlcol)

  ## Data points and labels
  if (length(pcol)==2) {
    pcol <- pcol[datvec+1]
  }
  points(datvec,beta,pch=pch,col=pcol)
  text(datvec,beta,itemlabs,col=pcol,
       pos=2+2*datvec,adj=c(0,-1+2*datvec))

  invisible(datvec)

}


