source("Plot Functions.R")
library(dplyr)

#ALL LAKES MODE1L

################################################
#All Lakes Best Climate only
#ALL LAKES MODEL
all_lakes <- read.csv("All_Lakes/Climate_no-ndep/Less4/Variable Importance.txt") %>%
  filter(rel.inf >= 5) %>%
  mutate(Type = c(rep("Climate", 3), "Index", rep("Climate", 4)),
         Type = factor(Type, levels = c("Climate", "Envt", "Index", "WS")))
all_lakes2 <- read.csv("All_Lakes/Climate+LU_no-ndep/Less3/Variable Importance.txt") %>%
  filter(rel.inf >= 5) %>%
  mutate(Type = c("Climate", "WS", "Climate", "Climate", "WS", "Climate", "Index", "Climate", "Climate", "Climate"),
         Type = factor(Type, levels = c("Climate", "Envt", "Index", "WS")))

GL <- read.csv("Green Lakes/No year/Less3/Variable Importance.txt") %>%
  filter(rel.inf > 5) %>%
  mutate(Type = c("Climate", "Envt", "Envt", "Envt", "Envt"),
         Type = as.factor(Type))
LS <- read.csv("Loch_Sky/No Ndep/Less2/Variable Importance.txt") %>%
  filter(rel.inf >= 5) %>%
  mutate(Type = c("Envt", "Envt", "Index", "Climate", "Climate", "Climate", "Envt", "Climate"),
         Type = as.factor(Type))

colors <- c(RColorBrewer::brewer.pal(3, "Dark2"), "gray40")
colors <- colors[c(2, 3, 4, 1)]
#colors <- adjustcolor(colors, alpha.f = 0.7)
palette(colors)

all_labs <- c("Weekly precip.",
              "Monthly mean temp.",
              "Daily mean temp.",
              "DOY",
              "Temp. % normal",
              "Precip. % normal",
              "Daily precip.",
              "Max SWE")
all_labs2 <- c("Weekly precip.",
               "Drainage ratio",
               "Daily mean temp.",
               "Monthly mean temp.",
               "Max lake depth",
               "Precip % normal",
               "DOY",
               expression(Delta*"Snow cover ('92-'11)"),
               "Temp. % normal",
               "Snow cover (2011)")
GL_labs <- c("Max SWE",
             expression(NO[3]),
             "DIN:TDP",
             "Water temp.",
             "TDP")
LS_labs <- c("DIN:TP",
             "Water temp.",
             "DOY",
             "Monthly precip.",
             "Weekly mean temp.",
             "Precip. % normal",
             "DIN:TDP",
             "Weekly precip.")

n_vars <- sapply(list(all_labs, all_labs2, GL_labs, LS_labs), length)


pdf("Figure2_Variable_Importance.pdf", width = 3, height = 6)
xmax<- max(c(all_lakes$rel.inf, all_lakes2$rel.inf, GL$rel.inf, LS$rel.inf))
layout(mat = matrix(1:4, ncol = 1), heights = n_vars + 6)
par(mar = c(2, 7, 2, 1), oma = c(1, 0, 0.5, 0), mgp = c(2, 0.5, 0))
barplot(rev(all_lakes$rel.inf), horiz = TRUE, names.arg = rev(all_labs), las = 1, col = rev(all_lakes$Type),
        xlim = c(0, xmax), cex.axis = 0.8, cex.names = 0.8)
mtext(side = 3, paste("Regional Climate Model\n(C.V.", expression(R^2), "= 0.38)"), line = 0.3, cex = 0.7)
add_label(-0.01, 0.05, label = "(a)", pos = 3)

barplot(rev(all_lakes2$rel.inf), horiz = TRUE, names.arg = rev(all_labs2), las = 1, col = rev(all_lakes2$Type),
        xlim = c(0, xmax), cex.axis = 0.8, cex.names = 0.8)
# mtext(side = 3, expression("Regional Climate + WS Model\n(C.V." ~ R^2 ~"= 0.37)"), line = 0.4, cex = 0.7)
mtext(side = 3, paste("Regional Climate + WS Model\n(C.V.", expression(R^2), "= 0.37)"), line = 0.3, cex = 0.7)
add_label(-0.01, 0.05, label = "(b)", pos = 3)

barplot(rev(GL$rel.inf), horiz = TRUE, names.arg = rev(GL_labs), las = 1, col = rev(GL$Type),
        xlim = c(0, xmax), cex.axis = 0.8, cex.names = 0.8)
mtext(side = 3, expression("Long-Term Model (C.V." ~ R^2 ~"= 0.72)"), line = 0.8, cex = 0.7)
mtext(side = 3, "Green Lakes Valley Dataset", line = 0, cex = 0.6)
add_label(-0.01, 0.05, label = "(c)", pos = 3)

barplot(rev(LS$rel.inf), horiz = TRUE, names.arg = rev(LS_labs), las = 1, col = rev(LS$Type),
        xlim = c(0, xmax), cex.axis = 0.8, cex.names = 0.8)
mtext(side = 3, expression("Intra-Seasonal Model (C.V." ~ R^2 ~"= 0.64)"), line = 0.8, cex = 0.7)
mtext(side = 3, "Loch Vale Watershed Dataset", line = 0, cex = 0.6)
add_label(-0.01, 0.05, label = "(d)", pos = 3)

mtext(side = 1, "Variance Explained [%]", outer = FALSE, line = 1.8, xpd = NA, cex = 0.7)
legend("bottomright", legend = c("Climate", "Environmental", "Watershed", "Index"), fill = colors[c(1, 2, 4, 3)], 
       title = "Variable Type", bty = "n", cex = 0.8)

dev.off()
