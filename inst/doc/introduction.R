## ----load, echo=F, warning=FALSE----------------------------------------------
library("microsynth")
library("knitr")
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.height=3.1, fig.width=7.2, fig.show = "hold")

# For fast vignette-compiling (needed for CRAN), declare function to save reduced microsynth object.
# (Microsynth chunks with long run-times are set to eval=TRUE to create objects for the first time,
# but these chunks are set to eval=FALSE in the package for fast vignette compiling.)
# This reduces microsynth files to <1MB instead of >10 MB

saveReducedMicrosynth <- function(msobject, filename) {
  
  # Proceed only if object exists (so it is always safe to run the code)
  if(exists(deparse(substitute(msobject)))) {
    
    # Strip data-heavy objects and save
    msobject$w$Weights <- NULL
    msobject$w$Intervention <- NULL
    saveRDS(msobject, filename)
  }
}

## ----ex0_load-----------------------------------------------------------------
colnames(seattledmi)
set.seed(99199)

## ----ex0_variables------------------------------------------------------------
cov.var <- c("TotalPop", "BLACK", "HISPANIC", "Males_1521", "HOUSEHOLDS", 
             "FAMILYHOUS", "FEMALE_HOU", "RENTER_HOU", "VACANT_HOU")
match.out <- c("i_felony", "i_misdemea", "i_drugs", "any_crime")

## ----ex1, eval = TRUE, echo=TRUE----------------------------------------------
sea1 <- microsynth(seattledmi, 
                   idvar="ID", timevar="time", intvar="Intervention", 
                   start.pre=1, end.pre=12, end.post=16, 
                   match.out=match.out, match.covar=cov.var, 
                   result.var=match.out, omnibus.var=match.out,
                   test="lower",
                   n.cores = min(parallel::detectCores(), 2))
sea1
summary(sea1)

## ----ex1b, eval = TRUE--------------------------------------------------------
plot_microsynth(sea1)

## ----ex2, eval = FALSE, echo=TRUE---------------------------------------------
#  sea2 <- microsynth(seattledmi,
#                     idvar="ID", timevar="time", intvar="Intervention",
#                     start.pre=1, end.pre=12, end.post=c(14, 16),
#                     match.out=match.out, match.covar=cov.var,
#                     result.var=match.out, omnibus.var=match.out,
#                     test="lower",
#                     perm=250, jack=TRUE,
#                     n.cores = min(parallel::detectCores(), 2))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
saveReducedMicrosynth(sea2, "../inst/extdata/sea2.rds")
sea2 <- readRDS("../inst/extdata/sea2.rds")
sea2

## -----------------------------------------------------------------------------
plot_microsynth(sea2)

## ----ex3_vars-----------------------------------------------------------------
match.out <- c("i_robbery", "i_aggassau", "i_burglary", "i_larceny", "i_felony", 
               "i_misdemea", "i_drugsale", "i_drugposs", "any_crime")

## ----ex3, eval = FALSE, echo=TRUE---------------------------------------------
#  sea3 <- microsynth(seattledmi,
#                     idvar="ID", timevar="time", intvar="Intervention",
#                     end.pre=12,
#                     match.out=match.out, match.covar=cov.var,
#                     result.var=match.out, perm=250, jack=0,
#                     test="lower", check.feas=TRUE, use.backup = TRUE,
#                     n.cores = min(parallel::detectCores(), 2))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
saveReducedMicrosynth(sea3, "../inst/extdata/sea3.rds")
sea3 <- readRDS("../inst/extdata/sea3.rds")
summary(sea3)

## -----------------------------------------------------------------------------
plot_microsynth(sea3)

## ----ex4, eval = FALSE, echo=TRUE---------------------------------------------
#  match.out <- list( 'i_robbery'=rep(2, 6),
#                     'i_aggassau'=rep(2, 6),
#                     'i_burglary'=rep(1, 12),
#                     'i_larceny'=rep(1, 12),
#                     'i_felony'=rep(2, 6),
#                     'i_misdemea'=rep(2, 6),
#                     'i_drugsale'=rep(4, 3),
#                     'i_drugposs'=rep(4, 3),
#                     'any_crime'=rep(1, 12))
#  
#  sea4 <- microsynth(seattledmi,
#                     idvar="ID", timevar="time",intvar="Intervention",
#                     match.out=match.out, match.covar=cov.var,
#                     result.var=names(match.out), omnibus.var=names(match.out),
#                     end.pre=12,
#                     perm=250, jack = TRUE,
#                     test="lower",
#                     n.cores = min(parallel::detectCores(), 2))
#  

## ----ex4load, eval = TRUE, echo = FALSE---------------------------------------
saveReducedMicrosynth(sea4, "../inst/extdata/sea4.rds")
sea4 <- readRDS("../inst/extdata/sea4.rds")
summary(sea4)

## ----ex4plot------------------------------------------------------------------
plot_microsynth(sea4)

## ----ex5, eval = TRUE, echo=TRUE, results='hide'------------------------------
match.out <- c("i_felony", "i_misdemea", "i_drugs", "any_crime")

sea5 <- microsynth(seattledmi, 
                   idvar="ID", timevar="time",intvar="Intervention",
                   end.pre=12,
                   match.out=match.out, match.covar=cov.var,
                   result.var=FALSE, perm=0, jack=FALSE,
                   n.cores = min(parallel::detectCores(), 2))

summary(sea5)

## ----ex6, eval = FALSE, echo=TRUE---------------------------------------------
#  sea6 <- microsynth(seattledmi,
#                     idvar="ID", timevar="time", intvar="Intervention",
#                     end.pre=12, end.post=c(14, 16),
#                     result.var=match.out,
#                     test="lower",
#                     w=sea5$w,
#                     n.cores = min(parallel::detectCores(), 2))
#  
#  sea6

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
saveReducedMicrosynth(sea6, "../inst/extdata/sea6.rds")
sea6 <- readRDS("../inst/extdata/sea6.rds")
sea6

## ----ex7, eval = TRUE, echo=TRUE----------------------------------------------
plot_microsynth(sea6, plot.var=match.out[1:2])

## ----ex8prep, eval = TRUE, echo=TRUE------------------------------------------
set.seed(86872)
ids.t <- names(table(seattledmi$ID[seattledmi$Intervention==1]))
ids.c <- names(table(seattledmi$ID[seattledmi$Intervention==0]))
ids.synth <- c(sample(ids.t, 1), sample(ids.c, 100))
seattledmi.one <- seattledmi[is.element(seattledmi$ID, as.numeric(ids.synth)), ]

## ----ex8, eval = FALSE, echo=TRUE---------------------------------------------
#  sea8 <- microsynth(seattledmi.one,
#                     idvar="ID", timevar="time", intvar="Intervention",
#                     match.out=match.out[4], match.covar=cov.var,
#                     result.var=match.out[4],
#                     test="lower", perm=250, jack=FALSE,
#                     check.feas=TRUE, use.backup=TRUE,
#                     n.cores = min(parallel::detectCores(), 2))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
saveReducedMicrosynth(sea8, "../inst/extdata/sea8.rds")
sea8 <- readRDS("../inst/extdata/sea8.rds")
plot_microsynth(sea8)
summary(sea8)

## ----ex9, eval = TRUE, echo=TRUE----------------------------------------------
seattledmi.cross <- seattledmi[seattledmi$time==16, colnames(seattledmi)!="time"]

## ----ex9results, eval = FALSE, echo=TRUE--------------------------------------
#  sea9 <- microsynth(seattledmi.cross,
#                     idvar="ID", intvar="Intervention",
#                     match.out=FALSE, match.covar=cov.var,
#                     result.var=NULL,
#                     test="lower",
#                     perm=250, jack=TRUE,
#                     n.cores = min(parallel::detectCores(), 2))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
saveReducedMicrosynth(sea9, "../inst/extdata/sea9.rds")
sea9 <- readRDS("../inst/extdata/sea9.rds")
sea9

