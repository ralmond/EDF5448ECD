library(SSX)
library(vcd) # Kappa & weighted Kappa

StatShop <- Sys.getenv("STATSHOP")
setwd(paste(StatShop,"TestScripts","LanguageExport",sep=.Platform$file.sep))


LangSim1.cpd <- read.cpd.csv("Sim1000T16Scored.csv","LanguageDataDescription.pd.xml")
LangSim2.cpd <- read.cpd.csv("Sim2T16Scored.csv","LanguageDataDescription.pd.xml")
LangSim1 <- data.frame(LangSim1.cpd$demo,LangSim1.cpd$stat)
names(LangSim1)[1:4] <- c("Reading.True","Listening.True","Writing.True",
                          "Speaking.True")
LangSim2 <- data.frame(LangSim2.cpd$demo,LangSim2.cpd$stat)
names(LangSim2)[1:4] <- c("Reading.True","Listening.True","Writing.True",
                          "Speaking.True")


Reading.Acc <- table(LangSim1[,c("Reading.True","Reading.Mode")])
Writing.Acc <- table(LangSim1[,c("Writing.True","Writing.Mode")])
Listening.Acc <- table(LangSim1[,c("Listening.True","Listening.Mode")])

### Thee mode was never calculated for Speaking  Need to fix this. 
Speaking.P <- LangSim1[,18:20]
LangSim1[,"Speaking.Mode"] <- ordered(apply(Speaking.P,1,which.max),
                                      labels=c("Novice","Intermediate","Advanced"))

Speaking.Acc <- table(LangSim1[,c("Speaking.True","Speaking.Mode")])


gkLambda <- function (tab) {
  pmax <- max(apply(tab,1,sum))
  lambda <- (sum(diag(tab)) - pmax)/(sum(tab)-pmax)
  return(lambda)
}
gkLambda(Reading.Acc)
Kappa(Reading.Acc)
gkLambda(Writing.Acc)
Kappa(Writing.Acc)

ExpAcc <- function(truth,probs) {
  dnames <- levels(truth)
  probs <- as.matrix(probs)
  result <- matrix(0,length(dnames),length(dnames),
                   dimnames=list(Truth=dnames,Estimate=dnames))
  for (i in 1:nrow(probs)) {
    result[truth[i],] <- result[truth[i],] + probs[i,]
  }
  return(result)
}

Reading.ExpAcc <- ExpAcc(LangSim1[,1],LangSim1[,9:11])
Writing.ExpAcc <- ExpAcc(LangSim1[,3],LangSim1[,15:17])

Reading.ExpAcc
Writing.ExpAcc
gkLambda(Reading.ExpAcc)
Kappa(Reading.ExpAcc)
gkLambda(Writing.ExpAcc)
Kappa(Writing.ExpAcc)

library(xtable)
xtable(Reading.Acc/1000,digits=3)
xtable(Writing.Acc/1000,digits=3)

sum(diag(Reading.Acc))
apply(Reading.Acc,1,sum)
apply(Reading.Acc,2,sum)
sum(apply(Reading.Acc,1,sum)/1000*apply(Reading.Acc,2,sum)/1000)


xtable(Reading.ExpAcc/1000,digits=3)
xtable(Writing.ExpAcc/1000,digits=3)

xtable(Speaking.Acc/1000,digits=3)
xtable(Listening.Acc/1000,digits=3)

