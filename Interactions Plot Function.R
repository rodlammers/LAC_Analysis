
gbm.perspecIB<-function(gbm.object, #IB model fitted with gbm function from package gbm
                        n.trees,              #IB optimal number of trees as determined by gbm.perf function from package gbm
                        data,
                        x = 1,                # the first variable to be plotted
                        y = 2,                # the second variable to be plotted
                        pred.means = NULL,    # allows specification of values for other variables
                        x.label = NULL,       # allows manual specification of the x label
                        x.range = NULL,       # manual range specification for the x variable
                        y.label = NULL,       # and y label
                        y.range = NULL,       # and the y
                        z.range = NULL,       # allows control of the vertical axis
                        ticktype = "detailed",# specifiy detailed types - otherwise "simple"
                        theta = 55,           # rotation 
                        phi=40,               # and elevation
                        smooth = "none",      # controls smoothing of the predicted surface
                        #mask = FALSE,        # controls masking using a sample intensity model
                        perspective = FALSE)   # controls whether a contour or perspective plot is drawn
#...)                 # allows the passing of additional arguments to plotting routine
# useful options include shade, ltheta, lphi for controlling illumination
# and cex for controlling text size - cex.axis and cex.lab have no effect
{
  #
  # gbm.perspec version 2.9 April 2007
  # J Leathwick/J Elith
  #
  #(Code modified by IB, commented out code was from the original function created by Leathwick & Elith)
  #
  # takes a gbm boosted regression tree object produced by gbm.step and
  # plots a perspective plot showing predicted values for two predictors
  # as specified by number using x and y
  # values for all other variables are set at their mean by default
  # but values can be specified by giving a list consisting of the variable name
  # and its desired value, e.g., c(name1 = 12.2, name2 = 57.6)
  
  require(gbm)
  require(splines)
  library(ggplot2)
  
  #get the boosting model details
  
  #gbm.call <- gbm.object$gbm.call
  #gbm.x <- gbm.call$gbm.x
  #n.preds <- length(gbm.x)
  n.preds<-length(gbm.object$var.names) 
  #gbm.y <- gbm.call$gbm.y
  #pred.names <- gbm.call$predictor.names
  pred.names<-gbm.object$var.names
  #family = gbm.call$family
  family<-"gaussian"
  
  #x.name <- gbm.call$predictor.names[x] #IB gives name to first variable
  x.name <- gbm.object$var.names[x] #IB gives name to first variable
  
  if (is.null(x.label)) {
    #x.label <- gbm.call$predictor.names[x] #IB gives label to x
    x.label <- gbm.object$var.names[x]
    } #IB gives label to x   
  
  #y.name <- gbm.call$predictor.names[y] #IB gives name to second variable
  y.name <- gbm.object$var.names[y] #IB gives name to second variable
  
  if (is.null(y.label)) {
    #y.label <- gbm.call$predictor.names[y] #IB gives label to y
    y.label <- gbm.object$var.names[y]
    } #IB gives label to y
  
  #data <- eval(parse(text=gbm.call$dataframe))[,gbm.x]  #IB selects columns of predictors used in the model
  data<-data[,gbm.object$var.names] #IB selects columns of predictors used in the model (CHANGE ACCORDING TO DATASET USED!)
  #n.trees <- gbm.call$best.trees
  
  if (is.factor(data[,x])){
    x.var <- 1:length(unique(data[,x]))
  }else{
    if (is.null(x.range)) {
      x.var <- seq(min(data[,x],na.rm=T),max(data[,x],na.rm=T),length = 50)
    } else {x.var <- seq(x.range[1],x.range[2],length = 50)}
  }
  
  if (is.factor(data[,y])){
    y.var <- 1:length(unique(data[,y]))
  }else{
    if (is.null(y.range)) {
      y.var <- seq(min(data[,y],na.rm=T),max(data[,y],na.rm=T),length = 50)
    } else {y.var <- seq(y.range[1],y.range[2],length = 50)}
  }
  
  pred.frame <- expand.grid(list(x.var,y.var))
  names(pred.frame) <- c(x.name,y.name)
  
  j <- 3
  for (i in 1:n.preds) {
    if (i != x & i != y) {
      if (is.vector(data[,i])) {
        m <- match(pred.names[i],names(pred.means))
        if (is.na(m)) {
          pred.frame[,j] <- mean(data[,i],na.rm=T)
        }
        else pred.frame[,j] <- pred.means[m]
      }
      if (is.factor(data[,i])) {
        m <- match(pred.names[i],names(pred.means))
        temp.table <- table(data[,i])
        if (is.na(m)) {
          pred.frame[,j] <- rep(names(temp.table)[2],2500)
        }
        else pred.frame[,j] <- pred.means[m]
        pred.frame[,j] <- factor(pred.frame[,j],levels=names(temp.table))
      }
      names(pred.frame)[j] <- pred.names[i]
      j <- j + 1
    }
  }
  #
  # form the prediction
  #
  #prediction <- predict.gbm(gbm.object,pred.frame,n.trees = n.trees, type="response")  #IB Forms predictions
  prediction <- predict.gbm(gbm.object,pred.frame,n.trees = n.trees, type="response") #IB Forms predictions
  
  # model smooth if required
  
  #if (smooth == "model") {
  #pred.glm <- glm(prediction ~ ns(pred.frame[,1], df = 8) * ns(pred.frame[,2], df = 8), data=pred.frame,family=poisson)
  #prediction <- fitted(pred.glm)
  #}
  
  # report the maximum value and set up realistic ranges for z
  
  max.pred <- max(prediction)
  cat("maximum value = ",round(max.pred,2),"\n")
  
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0,1)
    }
    else if (family == "poisson") {
      z.range <- c(0,max.pred * 1.1)
    }
    else {
      z.min <- min(data[,y],na.rm=T)
      z.max <- max(data[,y],na.rm=T)
      z.delta <- z.max - z.min
      z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * z.delta))
    }
  }
  # form the matrix
  
  pred.matrix <- matrix(prediction,ncol=50,nrow=50)
  
  # kernel smooth if required
  
  if (smooth == "average") {  #apply a 3 x 3 smoothing average
    pred.matrix.smooth <- pred.matrix
    for (i in 2:49) {
      for (j in 2:49) {
        pred.matrix.smooth[i,j] <- mean(pred.matrix[c((i-1):(i+1)),c((j-1):(j+1))])
      }
    }
    pred.matrix <- pred.matrix.smooth
  }
  
  # mask out values inside hyper-rectangle but outside of sample space
  
  #if (mask) {
  #mask.trees <- mask.object$gbm.call$best.trees
  #point.prob <- predict.gbm(mask.object[[1]],pred.frame, n.trees = mask.trees, type="response")
  #point.prob <- matrix(point.prob,ncol=50,nrow=50)
  #pred.matrix[point.prob < 0.5] <- 0.0
  #}
  #
  # and finally plot the result
  #
  plot_data <- data.frame(pred.frame[,1], pred.frame[,2], prediction)
  colnames(plot_data) <- c(x.name, y.name, "Predicted")
  if (!perspective) {
    #image(x = x.var, y = y.var, z = pred.matrix, zlim = z.range)
    print(ggplot(plot_data, aes_string(x = x.name, y = y.name, fill = "Predicted")) +
      geom_tile(color = "white", size = 0.25) +
      scale_fill_distiller(palette = "Greens", direction = 1) +
      theme(legend.position = "bottom"))
  }else {
    persp(x=x.var, y=y.var, z=pred.matrix, zlim= z.range,      # input vars
          xlab = x.label, ylab = y.label, zlab = "Fitted Value",   # labels
          theta=theta, phi=phi, r = sqrt(10), d = 3,               # viewing pars
          ticktype = ticktype, mgp = c(4,1,0)) #
  }
  
  return(plot_data)
}
