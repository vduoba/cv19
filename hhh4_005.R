### hhh4_005.R
### Section 3 Basic Model Fitting
### Assumes hh4_001-4.R results available
### MeaslesModel_basic <- list(.....)
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
### Looks familiar at 2.48
#
### No plot(measlesFit_basic, type="season",...) as series is a short daily series.
### Relative importance of the 3 model components (subset to > 40 total counts):
### districts2plot <- which( etc)
dhbs_to_plot <- which(colSums(observed(covidNZ))>40) # Name and index code
par(mfrow=c(ceiling(length(dhbs_to_plot)/3),3), mar=c(3,5,2,2), las=1)
plot(covid19Fit_basic, type="fitted", units=dhbs_to_plot,
     hide0s=TRUE, par.settings=NULL, legend=1)
plot(covid19Fit_basic, type="fitted", total=TRUE,
     hide0s=TRUE, par.settings=NULL, legend=FALSE) -> fitted_components
### Orange is spatiotemporal component
### Blue is autoregressive (dominant)
### Light grey is endemic component (at base of each plot, and very small
### except at begiining of ts when it is dominant.)
#
### Ignore the [20:22] in the paper as we have only a few time periods
fitted_components$Overall # Our model does not have time-varying population fractions
#
### Time-averaged proportions of the means explianed by
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
#