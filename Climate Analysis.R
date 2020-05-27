
all <- read.csv("LACdataset180507_2015_2016.csv")
green <- read.csv("LACdataset180507_GL.csv")
loch <- read.csv("LACdataset180517_LS.csv")

colors <- c("gray40", rgb(51, 160, 44, maxColorValue = 255), 
            rgb(31, 120, 180, maxColorValue = 255))
names <- c("Regional", "GLV", "LVWS")

#Plot - temp and precip
png("PRISM Boxplot.png", type = "cairo", units = "in",
    height = 3, width = 6.5, res = 500)
par(mar = c(2.5, 2.5, 1.5, 0.5), mfrow = c(1, 2), oma = c(0, 0.5, 0, 0), mgp = c(2, 0.3, 0), tcl = 0.5)
#ylim <- c(0, 200)
rodplot(unique(all$ppt_percent_normal * 100), unique(green$ppt_percent_normal * 100),
        unique(loch$ppt_percent_normal * 100),  
        names = names, ylab = "", las = 1,
        col = colors, main = "Monthly Precip",
        ylim = c(20, 180))
abline(h = 100, lwd = 2, lty = 2)
points(jitter(rep(1, length(unique(all$ppt_percent_normal))), amount = 0.1), unique(all$ppt_percent_normal) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(2, length(unique(green$ppt_percent_normal))), amount = 0.1), unique(green$ppt_percent_normal) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(3, length(unique(loch$ppt_percent_normal))), amount = 0.1), unique(loch$ppt_percent_normal) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
add_label(-0.01, 0.06, expression(bold("(a)")))

rodplot(unique(all$tmean_percent_normal * 100), unique(green$tmean_percent_normal * 100), 
        unique(loch$tmean_percent_normal * 100),
        names = names, ylab = "", las = 1,
        col = colors, ylim = c(20, 180), main = "Monthly Temp")
points(jitter(rep(1, length(unique(all$tmean_percent_normal))), amount = 0.1), unique(all$tmean_percent_normal) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(2, length(unique(green$tmean_percent_normal))), amount = 0.1), unique(green$tmean_percent_normal) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(3, length(unique(loch$tmean_percent_normal))), amount = 0.1), unique(loch$tmean_percent_normal) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
add_label(-0.01, 0.06, expression(bold("(b)")))

abline(h = 100, lwd = 2, lty = 2)

mtext("Percent of Normal", side = 2, line = -0.5, outer = TRUE)
dev.off()

#PRISM through time
green$date_sampled <- as.Date(green$date_sampled, format = "%m/%d/%Y")
plot(ppt_percent_normal ~ date_sampled, green, type = "b")

loch$date_sampled <- as.Date(loch$date_sampled, format = "%m/%d/%Y")
loch$month_normal <- ifelse(loch$day >= 15, loch$month, loch$month - 1)
plot(ppt_percent_normal ~ date_sampled, loch, col = month_normal)

#Plot - SNOTEL

snow_percent <- data$Max.SWE / data$Normal.Max.SWE
date_diff <- as.numeric(data$Date.Snow.Free - data$Normal.Snow.Free)


png("SNOTEL Boxplot 2.png", type = "cairo", units = "in",
    height = 3, width = 6.5, res = 500)
par(mfrow = c(1, 2), mar = c(2.5, 3.5, 1.5, 0.5), mgp = c(2, 0.3, 0), tcl = 0.5)

rodplot(unique(all$SWE_pernorm) * 100, unique(green$SWE_pernorm) * 100, 
        unique(loch$SWE_pernorm) * 100, col = colors, las = 1, 
        ylab = "Percent of Normal", names = names, main = "Max SWE",
        ylim = c(45, 155))
abline(h = 100, lwd = 2, lty = 2)
points(jitter(rep(1, length(unique(all$SWE_pernorm))), amount = 0.1), unique(all$SWE_pernorm) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(2, length(unique(green$SWE_pernorm))), amount = 0.1), unique(green$SWE_pernorm) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(3, length(unique(loch$SWE_pernorm))), amount = 0.1), unique(loch$SWE_pernorm) * 100, pch = 16,
       col = rgb(0,0,0,0.5))
add_label(-0.01, 0.06, expression(bold("(a)")))

rodplot(unique(all$Difference_SnowFree), unique(green$Difference_SnowFree), 
        unique(loch$Difference_SnowFree), col = colors, las = 1,
        ylab = "Days Different from Normal", names = names, main = "Snow Free Date",
        ylim = c(-15, 15))
abline(h = 0, lwd = 2, lty = 2)
points(jitter(rep(1, length(unique(all$Difference_SnowFree))), amount = 0.1), unique(all$Difference_SnowFree), pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(2, length(unique(green$Difference_SnowFree))), amount = 0.1), unique(green$Difference_SnowFree), pch = 16,
       col = rgb(0,0,0,0.5))
points(jitter(rep(3, length(unique(loch$Difference_SnowFree))), amount = 0.1), unique(loch$Difference_SnowFree), pch = 16,
       col = rgb(0,0,0,0.5))
add_label(-0.01, 0.06, expression(bold("(b)")))
dev.off()


