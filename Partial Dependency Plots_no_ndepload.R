source("Plot Functions.R")
library(dplyr)

#ALL LAKES MODEL
path <- "ALL_Lakes/Climate+LU_no-ndep/Less3"
data <- read.csv(file.path(path, "LACdataset180507_2015_2016.csv"))
R2 <- read.csv(file.path(path, "/R2 Values.txt"))

dependency <- read.csv(file.path(path, "Dependency Plot Results.txt"))
var_importance <- read.csv(file.path(path, "Variable Importance.txt"))

plot_vars <- var_importance %>%
  filter(rel.inf >= 4.95) %>%
  select(var)

plot_vars <- as.character(plot_vars$var)

n_vars <- length(plot_vars)

plot_data <- dependency %>%
  filter(Var %in% plot_vars) %>%
  left_join(var_importance, by = c("Var" = "var")) %>%
  arrange(desc(rel.inf)) %>%
  rowwise() %>%
  mutate(level = which(levels(Var) == Var)) %>%
  ungroup() %>%
  mutate(Var = factor(Var, levels(Var)[unique(level)])) %>%
  rowwise() %>%
  mutate(letter = letters[which(levels(Var) == Var)],
         index = which(levels(Var) == Var))

#Plot dependency plots
png(file.path(path, "Regional Climate and Watershed Partial Dependency.png"), type = "cairo", units = "in",
    height = 6, width = 6.5, res = 500)
colors <- plot_colors()
xmin <- c(0, 0, 5.5, 0, 0, 0, 150, -4, 75, 0)
xmax <- c(27, 10, 16.5, 15, 45, 130, 280, 0, 175, 80)
units <- c("Weekly precip. [mm]",
           "Drainage ratio [%]",
           expression("Daily mean temp. ["*degree*C*"]"),
           expression("Monthly mean temp. ["*degree*C*"]"),
           "Max lake depth [m]",
           "Precip. % normal [%]",
           "DOY [day]",
           expression(Delta * "Snow cover ('92-'11) [%]"),
           "Mean temp. % normal [%]",
           "Snow cover (2011) [%]")
y_range <- range(log(data$chl_a))
par(mfrow = c(3, 4), mar = c(3.5, 1.5, 1, 0.5), 
    oma = c(0, 2, 2.5, 0), mgp = c(2, 0.3, 0), tcl = 0.5)
plyr::d_ply(plot_data, "Var", function(x, data, y_range, colors, log, points){
  subset <- data[,as.character(x$Var[1])]
  if (x$index[1] == 6 | x$index[1] == 9 | x$index[1] == 8 | x$index[1] == 10){
    subset <- subset * 100
    x$Value <- x$Value * 100
  }
  if (log){
    y <- log(data$chl_a)
  }else{
    y <- data$chl_a
    y_range <- exp(y_range)
  }
  plot(NA, ylim = y_range, las = 1,
       xlab = "", ylab = "",
       main = paste0(round(x$rel.inf[1], 1), " %"),
       yaxt = "n",
       xlim = c(xmin[x$index[1]], xmax[x$index[2]]),
       cex.main = 1.2)
  if (points){
    points(subset, y, pch = 16, col = adjustcolor("gray60", 0.5), cex = 0.9)
  }else{
    axis(side = 1, at = subset, labels = FALSE, col.ticks = "gray40",
         tcl = 0.3, lwd.ticks = 0.5)
    axis(side = 2, at = y, labels = FALSE, col.ticks = "gray40",
         tcl = 0.3, lwd.ticks = 0.5)
  }
  if (log){
    lines(Chl_a ~ Value, data = x, lwd = 3, lty = 1)
    y_vals <- c(0.1, 1, 10)
    y_vals2 <- as.numeric(sapply(y_vals, function(x){x * 2:9}))
    axis(side = 2, at = log(y_vals), labels = y_vals, las = 1)
    axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = 0.2)
    fit <- loess(Chl_a ~ Value, data = x)
    # test <- data.frame(x_vals = subset, chl_a = y)
    # print(str(test))
    # fit <- loess(chl_a ~ x_vals, data = test, span = 1)
    xvals <- seq(min(x$Value), max(x$Value), length.out = 30)
    pred <- predict(fit, data.frame(Value = xvals))
    #pred <- predict(fit, data.frame(x_vals = xvals))
    lines(xvals, pred, col = colors["vermillion"], lwd = 2, lty = 6)
  }else{
    lines(I(exp(Chl_a)) ~ Value, data = x, lwd = 3, col = colors["vermillion"])
    axis(side = 2, at = seq(0, 20, 5), las = 1)
    fit <- loess(I(exp(Chl_a)) ~ Value, data = x)
    xvals <- seq(min(x$Value), max(x$Value), length.out = 30)
    pred <- predict(fit, data.frame(Value = xvals))
    lines(xvals, pred, lty = 2)
  }
  
  mtext(side = 1, units[x$index[1]], line = 2, cex = 0.7)
  legend("topleft", legend = bquote(bold("("*.(letters[x$index[1]])*")")), pch = NA,
         bty = "n", inset = c(-0.11, -0.05))
}, data, y_range, colors, TRUE, TRUE)

mtext(side = 2, expression("Chlorophyll" ~ italic(a) ~ "["*mu*g~L^-1*"]"), outer = TRUE, line = 0)
mtext(side = 3, bquote("Regional Climate & Watershed Model (C.V. " * R^2 ~ "=" ~ .(a) * ")", list(a = round(R2$CVR2, 2))), outer = TRUE,
      line = 0.5)
dev.off()

################################################
#All Lakes Best Climate only
#ALL LAKES MODEL
path <- "ALL_Lakes/Climate_no-ndep/Less4"
data <- read.csv(file.path(path, "LACdataset180507_2015_2016.csv"))
R2 <- read.csv(file.path(path, "/R2 Values.txt"))

dependency <- read.csv(file.path(path, "Dependency Plot Results.txt"))
var_importance <- read.csv(file.path(path, "Variable Importance.txt"))

plot_vars <- var_importance %>%
  filter(rel.inf >= 4.9) %>%
  select(var)

plot_vars <- as.character(plot_vars$var)

n_vars <- length(plot_vars)

plot_data <- dependency %>%
  filter(Var %in% plot_vars) %>%
  left_join(var_importance, by = c("Var" = "var")) %>%
  arrange(desc(rel.inf)) %>%
  rowwise() %>%
  mutate(level = which(levels(Var) == Var)) %>%
  ungroup() %>%
  mutate(Var = factor(Var, levels(Var)[unique(level)])) %>%
  rowwise() %>%
  mutate(letter = letters[which(levels(Var) == Var)],
         index = which(levels(Var) == Var))


#Plot dependency plots
png(file.path(path, "Regional Climate Partial Dependency.png"), type = "cairo", units = "in",
    height = 4, width = 6.5, res = 500)
colors <- plot_colors()
palette(adjustcolor(colors[1:2], alpha.f = 0.5))
xmin <- c(0, 2.3, 5.8, 150, 85, 23, 0, 0)
xmax <- c(27, 15, 16.5, 265, 178, 124, 7, 25)
axis_min <- c(0, 3, 6, 3, 150, 25, 90, 0)
axis_max <- c(25, 15, 14, 4.5, 250, 125, 180, 7)
axis_n <- c(5, 4, 2, 3, 2, 4, 3, 7)
units <- c("Weekly precip. [mm]",
           expression("Monthly mean temp. ["*degree*C*"]"),
           expression("Daily mean temp. ["*degree*C*"]"),
           "DOY [day]",
           "Mean temp. % normal [%]",
           "Precip. % normal [%]",
           "Daily precip. [mm]",
           "Max SWE [in]")
y_range <- range(log(data$chl_a))
par(mfrow = c(2, 4), mar = c(3.5, 1.5, 1, 0.5), 
    oma = c(0, 2, 2.5, 0), mgp = c(2, 0.3, 0), tcl = 0.5)
plyr::d_ply(plot_data, "Var", function(x, data, y_range, colors, log, points){
  subset <- data[,as.character(x$Var[1])]
  if (x$index[1] == 5 | x$index[1] == 6){
    subset <- subset * 100
    x$Value <- x$Value * 100
  }
  if (x$index[1] == 2 | x$index[1] == 6){
    xaxp <- c(axis_min[x$index[1]], axis_max[x$index[1]], axis_n[x$index[1]])
  }else{
    xaxp <- NULL
  }
  
  if (log){
    y <- log(data$chl_a)
  }else{
    y <- data$chl_a
    y_range <- exp(y_range)
  }
  plot(NA, ylim = y_range, las = 1,
       xlab = "", ylab = "",
       main = paste0(round(x$rel.inf[1], 1), " %"),
       yaxt = "n",
       xlim = c(xmin[x$index[1]], xmax[x$index[2]]),
       xaxp = xaxp,
       cex.main = 1.2, cex.axis = 1)
  if (points){
    points(subset, y, pch = 16, col = adjustcolor("gray60", 0.5), cex = 0.9)
  }else{
    axis(side = 1, at = subset, labels = FALSE, col.ticks = "gray40",
         tcl = 0.3, lwd.ticks = 0.5)
    axis(side = 2, at = y, labels = FALSE, col.ticks = "gray40",
         tcl = 0.3, lwd.ticks = 0.5)
  }
  if (log){
    lines(Chl_a ~ Value, data = x, lwd = 3, lty = 1)
    y_vals <- c(0.1, 1, 10)
    y_vals2 <- as.numeric(sapply(y_vals, function(x){x * 2:9}))
    axis(side = 2, at = log(y_vals), labels = y_vals, las = 1)
    axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = 0.2)
    fit <- loess(Chl_a ~ Value, data = x)
    xvals <- seq(min(x$Value), max(x$Value), length.out = 30)
    pred <- predict(fit, data.frame(Value = xvals))
    lines(xvals, pred, col = colors["vermillion"], lwd = 2, lty = 6)
  }else{
    lines(I(exp(Chl_a)) ~ Value, data = x, lwd = 3, col = colors["vermillion"])
    axis(side = 2, at = seq(0, 20, 5), las = 1)
    fit <- loess(I(exp(Chl_a)) ~ Value, data = x)
    xvals <- seq(min(x$Value), max(x$Value), length.out = 30)
    pred <- predict(fit, data.frame(Value = xvals))
    lines(xvals, pred, lty = 2)
  }
  
  mtext(side = 1, units[x$index[1]], line = 2, cex = 0.7)
  legend("topleft", legend = bquote(bold("("*.(letters[x$index[1]])*")")), pch = NA,
         bty = "n", inset = c(-0.11, -0.05))
}, data, y_range, colors, TRUE, TRUE)

mtext(side = 2, expression("Chlorophyll" ~ italic(a) ~ "["*mu*g~L^-1*"]"), outer = TRUE, line = 0)
mtext(side = 3, bquote("Regional Climate Model (C.V. " * R^2 ~ "=" ~ .(a) * ")", list(a = round(R2$CVR2, 2))), outer = TRUE,
      line = 0.5)
# legend("bottomleft", legend = levels(data$site_type), pch = 21, cex = 1.1, 
#        pt.bg = 1:2, bty = "n", horiz = TRUE, xpd = NA, inset = c(-2, -0.5))
dev.off()
##############################################3
#Loch-Sky MODEL
path <- "Loch_Sky/No Ndep/Less2"
data <- read.csv(file.path(path, "LACdataset180517_LS.csv"))
R2 <- read.csv(file.path(path, "/R2 Values.txt"))

dependency <- read.csv(file.path(path, "Dependency Plot Results.txt"))
var_importance <- read.csv(file.path(path, "Variable Importance.txt"))

plot_vars <- var_importance %>%
  filter(rel.inf >= 5) %>%
  select(var)

plot_vars <- as.character(plot_vars$var)

n_vars <- length(plot_vars)

plot_data <- dependency %>%
  filter(Var %in% plot_vars) %>%
  left_join(var_importance, by = c("Var" = "var")) %>%
  arrange(desc(rel.inf)) %>%
  rowwise() %>%
  mutate(level = which(levels(Var) == Var)) %>%
  ungroup() %>%
  mutate(Var = factor(Var, levels(Var)[unique(level)])) %>%
  rowwise() %>%
  mutate(letter = letters[which(levels(Var) == Var)],
         index = which(levels(Var) == Var))

png(file.path(path, "Loch Sky Partial Dependency.png"), type = "cairo", units = "in",
    height = 4.5, width = 6.5, res = 500)
colors <- plot_colors()
palette(adjustcolor(colors[c(1, 5)], alpha.f = 0.5))
xmin <- c(23, 3, 120, 0, 3, 25, 0, 0)
xmax <- c(125, 15, 270, 200, 15, 190, 900, 30)
axis_min <- c(30, 3, NA, NA, 3, NA, 0, 0)
axis_max <- c(120, 15, NA, NA, 15, NA, 900, 30)
axis_n <- c(3, 4, NA, NA, 4, NA, 3, 3)
units <- c("DIN:TP [mol:mol]",
           expression("Water temp. ["*degree*C*"]"),
           "DOY [day]",
           "Mothly precip. [mm]",
           expression("Weekly mean temp. ["*degree*C*"]"),
           "Precip. % normal [%]",
           "DIN:TDP [mol:mol]",
           "Weekly precip. [mm]")
y_range <- range(log(data$chl_a))
par(mfrow = c(2, 4), mar = c(3.5, 1.5, 1, 0.5), 
    oma = c(2, 2, 3.5, 0), mgp = c(2, 0.3, 0), tcl = 0.5)
plyr::d_ply(plot_data, "Var", function(x, data, y_range, colors){
  subset <- data[,as.character(x$Var[1])]
  if (x$index[1] == 6){
    subset <- subset * 100
    x$Value <- x$Value * 100
  }
  if (x$index[1] %in% c(1, 2, 5, 7, 8)){
    xaxp <- c(axis_min[x$index[1]], axis_max[x$index[1]], axis_n[x$index[1]])
  }else{
    xaxp <- NULL
  }
  plot(subset, log(data$chl_a), ylim = y_range, las = 1,
       xlab = "", ylab = "", pch = c(16, 17)[as.numeric(data$name)], 
       col = data$name, cex = 0.9,
       main = paste0(round(x$rel.inf[1], 1), " %"),
       yaxt = "n", cex.main = 1.2,
       xlim = c(xmin[x$index[1]], xmax[x$index[1]]), cex.main = 1.2,
       xaxp = xaxp)
  lines(Chl_a ~ Value, data = x, lwd = 3)
  y_vals <- c(0.1, 1, 10)
  y_vals2 <- as.numeric(sapply(y_vals, function(x){x * 2:9}))
  axis(side = 2, at = log(y_vals), labels = y_vals, las = 1)
  axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = 0.2)
  fit <- loess(Chl_a ~ Value, data = x)
  xvals <- seq(min(x$Value), max(x$Value), length.out = 30)
  pred <- predict(fit, data.frame(Value = xvals))
  lines(xvals, pred, col = colors["vermillion"], lwd = 2, lty = 6)
  
  mtext(side = 1, units[x$index[1]], line = 2, cex = 0.7)
  legend("topleft", legend = bquote(bold("("*.(letters[x$index[1]])*")")), pch = NA,
         bty = "n", inset = c(-0.11, -0.05))
}, data, y_range, colors)

mtext(side = 2, expression("Chlorophyll" ~ italic(a) ~ "["*mu*g~L^-1*"]"), outer = TRUE, line = 0)
mtext(side = 3, bquote("Intra-Seasonal Model (C.V. " * R^2 ~ "=" ~ .(a) * ")", 
                       list(a = round(R2$CVR2, 2))), outer = TRUE,
      line = 1.5)
mtext(side = 3, "Loch Vale Watershed Dataset", outer = TRUE, line = 0.5, cex = 0.8)
legend("bottomleft", legend = c("The Loch", "Sky Pond"), pch = c(17, 16), cex = 1.1, 
       col = rev(unique(data$name)), bty = "n", horiz = TRUE, xpd = NA, inset = c(-2.1, -0.5))
dev.off()

##############################################3
#Green Lakes MODEL
#Green Lakes MODEL
path <- "Green Lakes/No year/Less3"
data <- read.csv("LACdataset180507_GL.csv")
R2 <- read.csv(file.path(path, "/R2 Values.txt"))

dependency <- read.csv(file.path(path, "Dependency Plot Results.txt"))
var_importance <- read.csv(file.path(path, "Variable Importance.txt"))

plot_vars <- var_importance %>%
  filter(rel.inf > 4) %>%
  select(var)

plot_vars <- as.character(plot_vars$var)

n_vars <- length(plot_vars)

plot_data <- dependency %>%
  filter(Var %in% plot_vars) %>%
  left_join(var_importance, by = c("Var" = "var")) %>%
  arrange(desc(rel.inf)) %>%
  rowwise() %>%
  mutate(level = which(levels(Var) == Var)) %>%
  ungroup() %>%
  mutate(Var = factor(Var, levels(Var)[unique(level)])) %>%
  rowwise() %>%
  mutate(letter = letters[which(levels(Var) == Var) + 5],
         index = which(levels(Var) == Var))

png(file.path(path, "Green Lakes Partial Dependency.png"), type = "cairo", units = "in",
    height = 4.5, width = 5, res = 500)
colors <- plot_colors()
palette(adjustcolor(colors[c(1, 5)], alpha.f = 0.5))
xmin <- c(17, 0, 5, 5.8, 0, 7)
xmax <- c(30, 0.23, 1675, 14, 12, 12)
units <- c("Max SWE [in]",
           expression(NO[3] ~ "[mg"~L^-1~"N]"),
           "DIN:TDP [mol:mol]",
           expression("Water temp. ["*degree*C*"]"),
           expression("TDP ["* mu *"g"~L^-1~"P]"),
           expression("Monthly mean temp. ["*degree*C*"]"))
y_range <- range(log(data$chl_a))
par(mfrow = c(2, 3), mar = c(3.5, 2, 1, 0.5), 
    oma = c(2, 2, 3.5, 0.3), mgp = c(2, 0.3, 0), tcl = 0.5)
plyr::d_ply(plot_data, "Var", function(x, data, y_range, colors){
  # if (x$index[1] == 6){
  #   xaxp <- c(0, 12, 4)
  # }else{
  xaxp <- NULL
  #}
  subset <- data[,as.character(x$Var[1])]
  plot(subset, log(data$chl_a), ylim = y_range, las = 1,
       xlab = "", ylab = "", pch = c(16, 17)[as.numeric(data$name)], 
       col = data$name, cex = 0.9,
       main = paste0(round(x$rel.inf[1], 1), " %"),
       yaxt = "n", cex.main = 1.2,
       xlim = c(xmin[x$index[1]], xmax[x$index[1]]),
       xaxp = xaxp)
  lines(Chl_a ~ Value, data = x, lwd = 3)
  y_vals <- c(0.01, 0.1, 1, 10)
  y_vals2 <- as.numeric(sapply(y_vals, function(x){x * 2:9}))
  axis(side = 2, at = log(y_vals), labels = y_vals, las = 1)
  axis(side = 2, at = log(y_vals2), labels = FALSE, tcl = 0.2)
  fit <- loess(Chl_a ~ Value, data = x)
  xvals <- seq(min(x$Value), max(x$Value), length.out = 30)
  pred <- predict(fit, data.frame(Value = xvals))
  lines(xvals, pred, col = colors["vermillion"], lwd = 2, lty = 6)
  
  mtext(side = 1, units[x$index[1]], line = 2, cex = 0.7)
  legend("topleft", legend = bquote(bold("("*.(letters[x$index[1]])*")")), pch = NA,
         bty = "n", inset = c(-0.11, -0.05))
}, data, y_range, colors)

mtext(side = 2, expression("Chlorophyll" ~ italic(a) ~ "["*mu*g~L^-1*"]"), outer = TRUE, line = 0.3)
mtext(side = 3, bquote("Intra-Annual Model (C.V. " * R^2 ~ "=" ~ .(a) * ")", list(a = round(R2$CVR2, 2))), outer = TRUE,
      line = 1.5)
mtext(side = 3, "Green Lakes Valley Dataset", outer = TRUE, line = 0.5, cex = 0.8)
legend("bottomleft", legend = c("Green Lakes 1", "Green Lakes 4"), pch = c(16, 17), cex = 1.1, 
       col = rev(unique(data$name)), bty = "n", horiz = TRUE, xpd = NA, inset = c(-1.7, -0.5))
dev.off()