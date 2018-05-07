setwd("~/Dropbox/3_turb facilitation/turb density mac")
turbdensity <- c(2,
                 31,
                 27,
                 30,
                 26,
                 14,
                 24,
                 22,
                 18,
                 19,
                 13,
                 32,
                 36,
                 16,
                 20,
                 23,
                 14,
                 8,
                 9,
                 22,
                 11,
                 36,
                 20,
                 26,
                 17,
                 34,
                 7,
                 19,
                 13,
                 5)
tiff(filename = 'survey.tiff', width = 6, height = 5, units = 'in', res = 300)
hist(turbdensity, col="grey80", right=TRUE, xlab = expression("# of thalli per 0.0625" ~ m^{2}), cex.lab="1.5", cex.axis="1.5", main= "Turbinaria Density")    
dev.off()
mean <- mean(turbdensity)
standarddev <- sd(turbdensity)
standerr <- standarddev/ sqrt(30)
length(which(turbdensity>30))
