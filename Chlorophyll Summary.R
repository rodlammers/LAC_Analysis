#Boxplot summarizing chl_a for each model
source("Plot Functions.R")
all <- read.csv("LACdataset180507_2015_2016.csv")
loch <- read.csv("LACdataset180517_LS.csv")
green <- read.csv("LACdataset180507_GL.csv")

color <- c("gray40", rgb(51, 160, 44, maxColorValue = 255), 
           rgb(31, 120, 180, maxColorValue = 255))

png("Chlorophyll boxplots.png", type = "cairo", units = "in",
    height = 3, width = 4.2, res = 500)
par(mar = c(4, 4, 0.5, 0.5), mgp = c(2, 0.3, 0), tcl = 0.5)
rodplot(all$chl_a, green$chl_a, loch$chl_a, col = color, las = 1,
        names = c("Regional", "GLV", "LVWS"),
        ylab = expression("Chlorophyll a ["*mu*g~L^-1*"]"), log = "y",
        yaxt = "n", ylim = c(0.005, 30))
axis(1, at = 1:3, labels = c(nrow(all), nrow(loch), nrow(green)), line = 1.5, tick = FALSE,
     xpd = NA, cex.axis = 0.9)
axis(1, at = 0.5, labels = "n =", line = 1.5, tick = FALSE, xpd = NA, cex.axis = 0.9)
y_vals <- c(0.001, 0.01, 0.1, 1, 10)
y_vals2 <- as.numeric(sapply(y_vals, function(x){x*2:9}))
axis(2, at = y_vals, labels = y_vals, las = 1)
axis(2, at = y_vals2, labels = FALSE, tcl = 0.2)
dev.off()

wilcox.test(loch$chl_a, green$chl_a)
wilcox.test(all$chl_a, green$chl_a)
wilcox.test(loch$chl_a, all$chl_a)
