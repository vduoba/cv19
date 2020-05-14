# Models outputs from hhh4_DataPrep

### Section 3 Basic Model Fitting
### MeaslesModel_basic <- list(.....)
library(ggplot2)
library(forecast) # For ggAcf
library(R0)
covid19Model_basic<-list(
  end        = list(f = ~1+t),
  offset     = population(covidNZ),
  ar         = list(f= ~1),
  ne         = list(f= ~1, weights=neighbourhood(covidNZ)==1),
  family="NegBin1" )
#
### Check adjacencies in the preceding formula
neighbourhood(covidNZ)==1
#
### MeaslesFit_basic <- hh4(stsObj)
covid19Fit_basic <- hhh4(stsObj=covidNZ, control=covid19Model_basic)
### Summary:
### summary(measlesFit_basic ...)
summary(covid19Fit_basic,
        idx2Exp=TRUE,
        amplitudeShift=FALSE,
        maxEV=TRUE )
### 6april data ##################################################
# Call:
#   hhh4(stsObj = covidNZ, control = covid19Model_basic)
#
# Coefficients:
#             Estimate  Std. Error
# exp(ar.1)   0.802201  0.069331
# exp(ne.1)   0.005455  0.010990
# exp(end.1)  0.025461  0.007137
# exp(end.t)  1.107864  0.011964
# overdisp    0.700287  0.109541
#
# Epidemic dominant eigenvalue:  0.82
#
# Log-likelihood:   -815.14
# AIC:              1640.29
# BIC:              1663.58
#
# Number of units:        20
# Number of time points:  39
### exp(end.1) is the rate at which the basic epidemic incidence increases per day.
#
### 16april data ###############################################
# Call:
#     hhh4(stsObj = covidNZ, control = covid19Model_basic)
#
# Coefficients:
#     Estimate  Std. Error
# exp(ar.1)   0.766395  0.064878
# exp(ne.1)   0.047719  0.011434
# exp(end.1)  0.056493  0.013972
# exp(end.t)  1.038308  0.007495
# overdisp    0.980477  0.128128
#
# Epidemic dominant eigenvalue:  0.96
#
# Log-likelihood:   -1062.11
# AIC:              2134.22
# BIC:              2158.76
#
# Number of units:        20
# Number of time points:  50
####################################################################
### No plot(measlesFit_basic, type="season",...) as series is a short daily series.
### Relative importance of the 3 model components (subset to > 40 total counts):
### districts2plot <- which( etc)
dhbs_to_plot <- which(colSums(observed(covidNZ))>40) # Name and index code
par(mfrow=c(ceiling(length(dhbs_to_plot)/3),3), mar=c(3,5,2,2), las=1)
plot(covid19Fit_basic, type="fitted", units=dhbs_to_plot,
     hide0s=TRUE, par.settings=NULL, legend=1)
plot(covid19Fit_basic, type="fitted", total=TRUE,
     hide0s=TRUE, par.settings=NULL, legend=FALSE) -> fitted_components
par(mfrow=c(1,1))
### Orange is spatiotemporal component
### Blue is autoregressive (dominant)
### Light grey is endemic component (at base of each plot, and very small
### except at begiining of ts when it is dominant.)
#
### Ignore the [20:22] in the paper as we have only a few time periods
fitted_components$Overall # Our model does not have time-varying population fractions
###########################################################################
# Try some ts plotting and smoothing:
ts_dat <- fitted_components$Overall[,"epidemic"]
ggAcf(ts_dat)
plot(ts_dat, type="l")
ndx<-1:length(ts_dat)
loessMod <- loess(ts_dat~ ndx, span=0.40)
smoothed <- predict(loessMod)
plot(ts_dat, x=ndx, type="l",
     main="Loess Smoothed Cov19 Cases",
     xlab="DayNbr", ylab="Cov19 Cases",
     lwd=2)
lines(smoothed, x=ndx, col="blue", lwd = 2)
# What is rate of increase at most worrying stage?
ts_worrying  <-ts_dat[16:28]
ndx_worrying <- ndx[16:28]
model_worrying <- lm(ts_worrying~1+ndx_worrying)
summary(model_worrying) # 3.8852 for rate of change over steepest deterioration
#########################################################################
### Try the R0 package to calculate R0 (time-dependent or during epidemic acceleration)

epid.count <- as.integer(ts_dat) # For now, use the predicted vals from loess smooth
# Create the generation time, gamma distn with mean 2.6 time units and sd 1
GT.covid19 <- generation.time("gamma", c(5.04, 1))
## Reproduction number estimate using  Exponential Growth  method.
res.R_EG <- estimate.R(epid.count, GT=GT.covid19,
                       methods=c("EG"))
plot(res.R_EG)
plotfit(res.R_EG)
#
## Reproduction number estimate using  Time-Dependent  method.
res.R_TD <- estimate.R(epid.count+1, GT=GT.covid19,
                       methods=c("TD"))
plot(res.R_TD)
plotfit(res.R_TD)
#
## Reproduction number estimate using  Sequential Bayesian  method.
res.R_SB <- estimate.R(epid.count+1, GT=GT.covid19,
                       methods=c("SB"), nsim=100)
plot(res.R_SB)
plotfit(res.R_SB)
#
## Reproduction number estimate using  Maximum Likelihood  method.
## Have to add a number to each obs to make it work
res.R_ML <- estimate.R(epid.count[10:51]+1, GT=GT.covid19,
                       methods=c("ML"))
plot(res.R_ML)
plotfit(res.R_ML)
#
## Reproduction number estimate using  Attack Rate  method.
res.R_AR <- estimate.R(epid.count, GT=GT.covid19,
                       methods=c("AR"), AR=0.999,
                       pop.size=5000000)
plot(res.R_AR)
plotfit(res.R_AR)
#########################################################################
### Time-averaged proportions of the means explained by
### the different components
colSums(fitted_components$Overall)[3:5]/sum(fitted_components$Overall[,1])
###  endemic          epi.own epi.neighbours
### 0.24648895     0.73952867     0.01398238
###
### NegBin1 under/overdispersion parameter
confint(covid19Fit_basic, parm="overdisp")
#               2.5 %   97.5 %
#   overdisp 0.544855 1.033349
### Looks like underdispersion (<1.0). Is the 0.75 midpoint very low?
### Might Poisson model might be OK?
### Compare with Poisson. (We can even compare non-nested models using AIC)
AIC(covid19Fit_basic, update(covid19Fit_basic, family="Poisson"))
# covid19Fit_basic                              4 1722.409
# update(covid19Fit_basic, family = "Poisson")  3 1946.842
# (??? Note: Explain the df! )
# But there is a difference in AIC of 223 with 1 df
### Look at reference below some day
#
############################################################################
# http://faculty.washington.edu/skalski/classes/QERM597/papers_xtra/Burnham%20and%20Anderson.pdf
