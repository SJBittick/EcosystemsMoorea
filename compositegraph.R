setwd("~/Dropbox/3_turb facilitation/turb density mac")
#frequency distribution
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
turbdensity_epi <- c(
3.0000,
3.0000,
3.0000,
3.0000,
7.0000,
7.0000,
7.0000,
10.000,
10.000,
10.000,
15.000,
15.000,
15.000,
20.000,
20.000,
20.000,
25.000,
25.000,
25.000,
30.000,
30.000,
30.000)

epiload <- 
c(51.100,
52.700,
28.200,
42.600,
51.100,
55.200,
49.600,
59.000,
64.400,
55.700,
58.100,
71.800,
67.600,
57.100,
63.600,
57.200,
44.300,
72.300,
66.000,
59.700,
61.800,
58.600)

#adding quadratic equation fit to data in jmp
a<- 37.54
b<-2.50
c <- -0.06
x <- seq(0,30, by=1)
d <- c * x^2
quadeq <- a + b * x + d

#graph without a fit
#pdf('1_epiloadnoline.pdf')
#plot(turbdensity_epi,epiload, xlab= "Density / 0.0625 m^2", ylab="Epiphyte % Cover per Thallus", type="p", pch=19, cex.lab="1.5", cex.axis="1.25")
#dev.off()

#graph with a quad fit only
#tiff('1_epiload_quad.pdf')
#plot(turbdensity_epi,epiload, xlab = "Density (#thalli / 0.0625 m^2)", ylab="Epiphyte % Cover per Thallus", type="p", pch=19, cex.lab="1.5", cex.axis="1.25")
#lines(x,quadeq, col='red', lwd=4)
#dev.off()

#alternatively, could follow a logistic fit 
e<- 0.3211279
g<--0.053362
h <- 60.077962
x <- seq(0,30, by=1)
logiseq <- h/(1+exp(-e*(x-g)))
#pdf('1_epiload_log.pdf')
#plot(turbdensity_epi,epiload, xlab="Turbinaria Density per Plot", ylab="Epiphyte % Cover per Thallus", type="p", pch=19, cex.lab="1.5", cex.axis="1.25")
#lines(x,logiseq, col='blue', lwd=4)
#dev.off()
turbhist <- qplot(turbdensity, geom="histogram", binwidth=5, fill=I("gray80"), col=I("black")) 
turbdistribution <- turbhist +
  theme_classic()+
  labs(y = "Frequency", x=expression("Turbinaria density "  ("thalli/0.0625" ~ m^{2})))+
  theme(text = element_text(size=15), axis.line = element_line(size = 1, color = "grey"), 
        axis.title=element_text(size = rel(1))) +
  scale_y_continuous(name="Frequency", limits=c(0, 7), breaks=seq(0,7, 1))+
  scale_x_continuous(limits=c(0, 40), breaks=seq(0,40, 5))

#both fits together
tiff('figure2.tiff', width = 4.5, height =11, units = 'in', res = 300)
par( mfrow = c(3,1) ) 
par(mar=c(4.5, 5.2, 2, 2))
#histogram was moved to figure with the fish
#hist(turbdensity, col="grey80", right=TRUE, xlab = expression("# of thalli per 0.0625" ~ m^{2}), cex.lab="1.5", cex.axis="1.5", main= "Turbinaria Density")    
#mtext("Figure 1", side=3, line=3, at=0)

  
#mtext("A", side=3, font=2, cex=1.5, line=0.5, at=-.1)
plot(turbdensity_epi,epiload, xlab=expression("Turbinaria density"  ("thalli/0.0625" ~ m^{2})) , ylab="Epiphyte % Cover per Thallus", type="p", pch=19, cex.lab="1.5", cex.axis="1.5", frame.plot = FALSE)

lines(x,quadeq, col='black', lwd=4)
#lines(x,logiseq, col='blue', lwd=4)
mtext("A", font=2, cex=1.5, side=3, line=0.5, at=-.1)
##macroalgal growth
turbdensity_mac <- c(
  0,
  3,
  3,
  3,
  7,
  7,
  10,
  15,
  15,
  15,
  20,
  20,
  20,
  25,
  25,
  25,
  30,
  30,
  30)

macgrowth <- 
  c(-5,
    0,
    5,
    5,
    5,
    0,
    25,
    30,
    5,
    25,
    25,
    15,
    20,
    10,
    10,
    15,
    5,
    5,
    -5)

a<--5.930211
b<-3.3810327
c <- -0.104972
x <- seq(0,30, by=1)

quadeq <- a + b*x + c*(x^2)

plot(turbdensity_mac,macgrowth, xlab=expression("Turbinaria density"  ("thalli/0.0625" ~ m^{2})), ylab="% Change Biomass/48 hours", type="p", pch=19, cex.lab="1.5", cex.axis="1.5", frame.plot = FALSE)
lines(x, quadeq, col='black', lwd=4)
mtext("B", font=2, cex=1.5, side=3, line=0.5, at=-.1)

##log fish bites

turbdensity_fish<- c(0,
                     3,
                     3,
                     3,
                     3,
                     7,
                     10,
                     10,
                     20,
                     20,
                     20,
                     25,
                     25,
                     25,
                     30,
                     30,
                     30)

log_bites <- c(0.425968732,
               0.522878745,
               0.318758763,
               0.425968732,
               0,
               0.970036777,
               0.865301426,
               1.014240439,
               0.636822098,
               0.985276743,
               0.684246748,
               1.255272505,
               0.801632346,
               1.392110465,
               0.602059991,
               0.884606581,
               1.204119983)

m1<-1.1803567
m2<-5.2859923
x <- seq(0,30, by=1)
mmeq <- (m1*x)/(m2+x)
plot(turbdensity_fish,log_bites, xlab= expression("Turbinaria density"  ("thalli/0.0625" ~ m^{2})), ylab="log(# of bites/10 mins)", type="p", pch=19, cex.lab="1.5", cex.axis="1.5", frame.plot = FALSE)
lines(x, mmeq, col='black', lwd=4)
mtext("C", font=2, cex=1.5, side=3, line=0.5, at=-.1)
dev.off()
