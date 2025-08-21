## ----installPropertee, eval=FALSE,include=FALSE-------------------------------
# ### this is for Adam, to remember how to do this each time
# ### LMK if it causes problems, I'll remove
# devtools::install_github('benbhansen-stats/propertee', auth_token=GITHUB_PAT)

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(propertee)

## ----loadData-----------------------------------------------------------------
data(lsoSynth)

## ----rddPlots,fig.width=7-----------------------------------------------------

figDat <- aggregate(lsoSynth[,c('nextGPA','lhsgrade_pct')],by=list(R=lsoSynth$R),
                    FUN=mean,na.rm=TRUE)
figDat$n <- as.vector(table(lsoSynth$R))

with(figDat,
     plot(R,nextGPA,cex=log(n+1)/3,main='Average Subsequent GPA\n as a function of 1st-Year GPA,\n Centered at AP Cutoff'))
abline(v=0,lty=2)

## ----defineUnitID-------------------------------------------------------------
lsoSynth$id <- rownames(lsoSynth)

## ----rdDesign-----------------------------------------------------------------
lsoSynth$Z <- lsoSynth$R<0

lsoSynthW <- subset(lsoSynth,abs(R)<=0.5)

#lsoSynthW$id <- 1:nrow(lsoSynthW)
spec <- rd_spec(Z ~ forcing(R) + unitid(id), data=lsoSynth, subset=abs(lsoSynth$R) <= 0.5)

## ----olsGmod1, eval=FALSE,include=TRUE----------------------------------------
# ### this doesn't work:
# g1 <- lm(nextGPA ~ R + Z, data = lsoSynth, weights = ett(spec)

## ----olsGmod2-----------------------------------------------------------------
##this works, but it's annoying to enter in the subset expression a 2nd time:
g1 <- lm(nextGPA ~ R + Z, data = lsoSynth, subset = abs(R) <= 0.5)


## ----robGmod------------------------------------------------------------------
g2 <-
if(requireNamespace("robustbase", quietly = TRUE)){
  robustbase::lmrob(nextGPA~poly(R,5)+Z,data=lsoSynthW)
} else g1

## ----yhat---------------------------------------------------------------------

yhat1 <- predict(g1,data.frame(R=forcings(spec)[[1]],Z=FALSE))
yhat2 <- predict(g2,data.frame(R=forcings(spec)[[1]],Z=FALSE))

plot(yhat1,yhat2)

## ----estEff1------------------------------------------------------------------
### method 1:

mean(lsoSynthW$nextGPA[lsoSynthW$Z]-yhat1[lsoSynthW$Z])-
  mean(lsoSynthW$nextGPA[!lsoSynthW$Z]-yhat1[!lsoSynthW$Z])

#### method 2:
coef(lm(nextGPA~Z, offset=yhat1,data=lsoSynthW))['ZTRUE']



## ----estEff2------------------------------------------------------------------
### method 1:

mean(lsoSynthW$nextGPA[lsoSynthW$Z]-yhat2[lsoSynthW$Z])-
  mean(lsoSynthW$nextGPA[!lsoSynthW$Z]-yhat2[!lsoSynthW$Z])

#### method 2:
coef(lm(nextGPA~Z, offset=yhat2,data=lsoSynthW))['ZTRUE']



