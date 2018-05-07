setwd("~/Dropbox/3_turb facilitation/turb density mac/R")
#this is the script to make the figure for my 2014 caging and density manipulation in 2014

caged = read.csv("caged.csv", header = TRUE)
attach(caged)
uncaged =read.csv("uncaged.csv", header = TRUE)
attach(uncaged)

##linear models for the caged and uncgaed data sets
uncagedlm <- lm(percentcoverU~DensityU) 
anova(uncagedlm)

cagedlm <- lm(percentcoverC~DensityC)
anova(cagedlm)

##Plot of points and linear model 
tiff('cagingexp.tiff', width = 7, height = 7, units = 'in', res = 300)
plot(DensityC, jitter(percentcoverC), pch=16, xlab = expression("# of thalli per 0.0625" ~ m^{2}), cex.lab="1.5", cex.axis="1.5", ylab="Epiphyte % Cover per Thallus")
legend("bottomright", inset = c(.05,.05), bty="n", c("-H", "+H"), cex=1, 
       col=c("black","red"), pch=16)
abline(uncagedlm, col="red", lwd=2)
points(DensityU, jitter(percentcoverU), pch=16, col="red")
abline(cagedlm, lwd=2)
dev.off()



####attempting to plot confidence intervals plotCI(0,intercC["Estimate"],intercC["Std. Error"],col="red",add=TRUE)
#intercU <- summary(uncagedlm)$coef[1,] 
#intercC <- summary(uncagedlm)$coef[1,] 

#######Wanting to sort from the original data frame, but ended up splitting them
#levels(densityexp$Caging)
#sorted <- densityexp[order(Caging, Density),]
#caging <- within(sorted, Caging <- relevel(Caging, ref = " +C"))
#lmwithcaging <- lm(percent.coverage....[" +C"]~Density[" +C"])

