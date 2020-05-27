#Green Lakes Interaction Plots
library(ggplot2)
library(dplyr)
library(gridExtra)
interactions <- read.csv("Green Lakes/No year/Less3/Interaction results.txt") %>%
  filter(int.size > 1)
data <- read.csv("LACdataset180507_GL.csv")
plots <- list()

ylabs <- c("Max SWE [in]",
           "DIN:TDP [mol:mol]",
           expression("Lake temp. ["*degree*C*"]"))
xlabs <- c(expression(NO[3]~"[mg" ~ L^-1~"N]"),
           expression("Lake temp. ["*degree*C*"]"),
           "Diff. snow free [days]")

  for (i in 1:nrow(interactions)){
    par(mfrow = c(1, 2), mar = c(4, 4, 0.5, 0.5))
    results <- read.csv(paste0("Green Lakes/No year/Less3/Interaction plot_", 
                               interactions$var1.names[i], "_",
                               interactions$var2.names[i], ".csv"))
    x <- seq(min(results[,2]), max(results[,2]), length.out = 50)
    y <- seq(min(results[,3]), max(results[,3]), length.out = 50)
    combined <- expand.grid(x, y)
    combined$Predicted <- results[,4]
    colnames(combined)[c(1,2)] <- colnames(results)[c(2,3)]
    
    point_data <- data[,colnames(combined[c(1,2)])]
    point_data$y <- min(point_data[,2], na.rm = TRUE) - 
      0.05 * diff(range(point_data[,2], na.rm = TRUE))
    point_data$x <- min(point_data[,1], na.rm = TRUE) - 
      0.05 * diff(range(point_data[,1], na.rm = TRUE))
    
    lbl <- data.frame(x = min(combined[,1]),
                      y = max(combined[,2]),
                      lab = (paste0("(", letters[i], ")")),
                      stringsAsFactors = FALSE)
    
    plots[[i]] <-  ggplot() +
            geom_tile(data = combined, aes_string(x = colnames(combined)[1], 
                                           y = colnames(combined)[2],
                                           fill = "Predicted"),
                      color = "white", size = 0.1) +
            scale_fill_distiller(palette = "Greens", direction = 1, limits = c(-4, 2.4)) +
            theme(legend.position = "bottom") +
            geom_point(aes_string(x = colnames(point_data)[1], y = "y"),
                       point_data,
                       inherit.aes = FALSE, alpha = 0.7, pch = "|") +
            geom_point(aes_string(y = colnames(point_data)[2], x = "x"), 
                       point_data,
                       inherit.aes = FALSE, alpha = 0.7, pch = "_") +
            theme(aspect.ratio = 1) +
            ggtitle(bquote(.(b)~"Interaction = "*.(a), list(b = lbl$lab[1], a = round(interactions$int.size[i], 1)))) +
            labs(x = xlabs[i], y = ylabs[i])
            # annotate("text", x = min(combined[,1]), y = max(combined[,2]),
            #          label = bquote('bold("("*.(lbl)*")")'), parse = TRUE)
            #geom_text(aes(x, y, label = lab, group = NULL), data = lbl, parse = TRUE)
    
    colors <- cRamp(log(data$chl_a), "Greens")
    # plot(data[,colnames(combined)[1]], data[,colnames(combined)[2]], pch = 21,
    #      bg = colors, ylab = colnames(combined)[2],
    #      xlab = colnames(combined)[1], las = 1)
  }

png("Green Lakes/No year/Less3/Green Lakes Interactions.png", type = "cairo", units = "in",
    height = 6.5, width = 5.5, res = 500)
grid.arrange(grobs = plots, ncol = 2)
dev.off()
