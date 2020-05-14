# Plots outputs from hhh4_DataPrep

### plot(measlesWeserEms(, type=...))
plot(covidNZ, type=observed~time) # All regions combined.
# NZ map with count numbers chloropleth
plot(covidNZ, type=observed~unit,
     population=population_mRepeated / 100000,
     labels=list(font=1, cex=0.6), colorkey=list(space="right"),
     sp.layout=layout.scalebar(covidNZ@map,corner=c(0.75,0.05),
                               scale=50000,
                               labels=c("0","50 km"),
                               cex=0.5,
                               height=0.05))
#
# Are there any 0 plots? (No!)
autoplot.sts(covidNZ, unit=which(colSums(observed(covidNZ))>0)) # Get all 20 plots!!!
#
# Below depends on magick as well as animation
# Takes a long time (5 mins) to prepare the ultimate html that runs the animation
#library(animation)
# animation::saveHTML(animate(covidNZ, tps=1:nrow(nz_counts_t), total.args=list()),
#                     title="Progress of Covid19 in NZ, 2020",
#                     ani.width=500, ani.height=600)
#
####################################################################################
