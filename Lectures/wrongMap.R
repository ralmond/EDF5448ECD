## WrongMap.R
## Russell Almond, FSU  almond@acm.org; ralmond@fsu.edu
## Copyright 2015 Artistic License 2.0

## This is a light weight version of the Wright Map (also known as the
## Person-Item Map) which does not use the IRT assumption.  Just a
## rough normal assumption for abilities.  It probably works best for
## 26-99 observations (more than that, it is probably ok to do
## scaling, fewer, and we don't really have enough data points for a
## stable estimate of the population distribution or the item
## difficulties.

## In particular, person abilities are calculated by simply
## normalizing the total score.  The item difficulties are calculated
## by the transformation qnorm(1-p), where p is the proportion of
## persons recieving that score.  For partial credit items, the
## difficulties are bases on p1, p2, p3 ... the (cumulative) probability of
## achieving each of score points.

## x is assumed to be a numeric matrix with columns represneting items
## and rows persons.  The values should be the scores, so that
## rowSums(x) gives the score on the assessment.


wrongMap <- function (x, item.subset =1:ncol(x), scores=rowSums(x),
                      itemLabs=NULL, abilLab="Ability",
                      cex.gen=0.7, xrange=NULL, prop = .04,
                      histOpt=list(col="red"),
                      itemPch=19, itemCol="black",
                      statePch=1, stateCol="black",
                      ygrid=5,xgrid=.5,...) {
  if (is.null(itemLabs)) {
    if (is.null(colnames(x))) {
      itemLabs <- paste("Q",item.subset)
    } else {
      itemLabs <- colnames(x)[item.subset]
    }
  }

  ## Compute ability distribution
  z <- (scores-mean(scores))/sd(scores)

  ## Compute item difficulties
  J <- length(item.subset)
  diffs <- vector("list",J)
  Snames <- vector("list",J)
  Idiffs <- rep(NA,J)
  Nstates <- rep(NA,J)
  names(diffs) <- itemLabs
  names(Idiffs) <- itemLabs
  for (item in item.subset) {
    tab <- cumsum(rev(table(as.ordered(x[,item]))))
    ms <- max(tab)
    qtab <- qnorm((ms-tab)/ms)
    diffs[[item]] <- qtab[is.finite(qtab)]
    Snames[[item]] <- names(qtab)[is.finite(qtab)]
    Nstates[item] <- sum(is.finite(qtab))
    Idiffs[item] <- qnorm(1-mean(x[,item],na.rm=TRUE)/max(x[,item],na.rm=TRUE))
  }
  alldiffs <- do.call("c",diffs)
  allitems <- rep(1:J,Nstates)
  allnames <- do.call("c",Snames)

  ## Remove all right/all wrong items.
  Items <- (1:J)[is.finite(Idiffs)]
  Idiffs <- Idiffs[is.finite(Idiffs)]

  diff.frame <- data.frame(Difficulty=c(Idiffs,alldiffs),
                           number = J-c(Items,allitems),
                           Item=itemLabs[c(Items,allitems)],
                           pch=rep(c(itemPch,statePch),
                                   c(length(Items),length(allitems))),
                           col=rep(c(itemCol,stateCol),
                                   c(length(Items),length(allitems))))

  head(diff.frame)
  ## Now start on the plot
  if (is.null(xrange)) {
    xrange <- range(c(z,alldiffs,Idiffs),na.rm=TRUE)
  }
  print(do.call("histogram", c(list(x=z,endpoints=xrange,
                                    xlab=abilLab,xlim=xrange*(1+prop)),
                               histOpt)),
        position=c(0,.68,1,1),more=TRUE,newpage=TRUE)

  ## xy <- xyplot(item~Ability,diff.frame,
  ##              panel=function(...) {
  ##                panel.xyplot(...,pch=diff.frame$pch,col=diff.frame$col)
  ##                JJ <- seq(J,0,-ygrid)
  ##                lsegments(rep(xrange[1],length(JJ)),JJ,
  ##                          rep(xrange[2],length(JJ)),JJ,
  ##                          col="lightGray")
  ##                XX <- seq(xrange[1],xrange[2],xgrid)
  ##                lsegments(XX,rep(-2,length(XX)),
  ##                          XX,rep(J+2,length(XX)),
  ##                          col="lightGray")
  ##              }, xlim=xrange,xlab=abilLab,...)
  xy <- dotplot(Item~Difficulty,diff.frame,
                pch=diff.frame$pch,col=diff.frame$col,
                xlim=xrange*(1+prop),...)
  print(xy,position=c(0,0,1,.7))
}



