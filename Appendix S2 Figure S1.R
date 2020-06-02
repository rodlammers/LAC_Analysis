##Oleksy et al. 2020, Ecology
##Appendix S2, Figure S1

#Observed and Fitted Values for the Regional, Long-Term, and Intra-Seasonal BRT Models

##Raw Observed Values
Regional=read.csv("LACdataset180507_2015_2016.csv")
LongTerm=read.csv("LACdataset180507_GL.csv")
IntraSeasonal=read.csv("LACdataset180517_LS.csv")

##Transformed Observed Values
RegionalObs=Regional[,"chl_a"]
LogRegionalObs=log(RegionalObs)

LongTermObs=LongTerm[,"chl_a"]
LogLongTermObs=log(LongTermObs)

IntraSeasonalObs=IntraSeasonal[,"chl_a"]
LogIntraSeasonalObs=log(IntraSeasonalObs)

##Raw Predicted (Fitted) Values
RegionalPred=read.csv("All_Lakes/Climate_no-ndep/Less4/Fitted Chl_a.txt", header=FALSE)
LongTermPred=read.csv("Green Lakes/No year/Less3/Fitted Chl_a.txt", header=FALSE)
IntraSeasonalPred=read.csv("Loch_Sky/No Ndep/Less2/Fitted Chl_a.txt", header=FALSE)

##Transformed Predicted (Fitted) Values
RegionalPred_Trans=RegionalPred
for(i in 1:length(RegionalPred)){
  RegionalPred_Trans[i]=exp(RegionalPred[i])
}

LongTermPred_Trans=LongTermPred
for(i in 1:length(LongTermPred)){
  LongTermPred_Trans[i]=exp(LongTermPred[i])
}

IntraSeasonalPred_Trans=IntraSeasonalPred
for(i in 1:length(IntraSeasonalPred)){
  IntraSeasonalPred_Trans[i]=exp(IntraSeasonalPred[i])
}

#Plots of the Observed versus Fitted Values for the Regional, Long-Term, and Intra-Seasonal BRT Models

##Setting Axis Values
y_vals <- c(0.01, 0.1, 1, 10)
y_vals2 <- as.numeric(sapply(y_vals, function(x){x * 2:9}))


##Creating and Saving Figure S1
png(filename="FigS1.png", res=300, width=30, height=10, units='cm')

par(mfrow=c(1,3), mar=c(5.1, 4.5, 4.1, 2.1))

plot(RegionalPred$V1, LogRegionalObs, main="Regional Climate Model", font.main=1, cex.main=2, cex.axis=1.5, xlab=bquote('Pred Chlorophyll'~italic(a)~mu*g~L^-1), ylab=bquote('Obs Chlorophyll'~italic(a)~mu*g~L^-1), cex.lab=1.5, 
     pch = 16, col = adjustcolor("gray60", 0.5), yaxt="n", xaxt="n", ylim=c(-1, 3.5), xlim=c(-1, 3.5))
text(-0.85, 3.4, labels=bquote('('*bold(a)*')'), cex=1.5)
axis(side = 2, at = log(y_vals), labels = y_vals, las = 1, cex.axis=1.5, tck=0.02)
axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = -0.2, tck=0.02)
axis(side = 1, at = log(y_vals), labels = y_vals, las = 1, cex.axis=1.5, tck=0.02)
axis(side = 1, at = log(y_vals2), labels = FALSE, tcl = -0.2, tck=0.02)
abline(lm(LogRegionalObs~RegionalPred$V1))

plot(LongTermPred$V1, LogLongTermObs, main="Long-Term Model", font.main=1,cex.main=2, cex.axis=1.5, xlab=bquote('Pred Chlorophyll'~italic(a)~mu*g~L^-1),
     ylab=bquote('Obs Chlorophyll'~italic(a)~mu*g~L^-1), cex.lab=1.5, pch = 16, col = adjustcolor("gray60", 0.5), yaxt="n", xaxt="n", ylim=c(-1, 3.5), xlim=c(-1, 3.5))
text(-0.85, 3.4, labels=bquote('('*bold(b)*')'), cex=1.5)
axis(side = 2, at = log(y_vals), labels = y_vals, las = 1, cex.axis=1.5, tck=0.02)
axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = -0.2, tck=0.02)
axis(side = 1, at = log(y_vals), labels = y_vals, las = 1, cex.axis=1.5, tck=0.02)
axis(side = 1, at = log(y_vals2), labels = FALSE, tcl = -0.2, tck=0.02)
abline(lm(LogLongTermObs~LongTermPred$V1))

plot(IntraSeasonalPred$V1, LogIntraSeasonalObs, main="Intra-Seasonal Model", font.main=1, cex.main=2, cex.axis=1.5, xlab=bquote('Pred Chlorophyll'~italic(a)~mu*g~L^-1), ylab=bquote('Obs Chlorophyll'~italic(a)~mu*g~L^-1), cex.lab=1.5, 
     pch = 16, col = adjustcolor("gray60", 0.5), yaxt="n", xaxt="n", xlim=c(-1, 3.5), ylim=c(-1, 3.5))
axis(side = 2, at = log(y_vals), labels = y_vals, las = 1, cex.axis=1.5, tck=0.02)
axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = -0.2, tck=0.02)
axis(side = 1, at = log(y_vals), labels = y_vals, las = 1, cex.axis=1.5, tck=0.02)
axis(side = 1, at = log(y_vals2), labels = FALSE, tcl = -0.2, tck=0.02)
text(-0.85, 3.4, labels=bquote('('*bold(c)*')'), cex=1.5)
abline(lm(LogIntraSeasonalObs~IntraSeasonalPred$V1))

dev.off()