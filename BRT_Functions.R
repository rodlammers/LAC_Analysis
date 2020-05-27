#FUNCTION TO AUTOMATICALLY GO THROUGH FITTING AND RUNNING MODEL, REMOVING LOW IMPORTANCE VARIABLES EACH TIME
iterative_BRT <- function(data, variables, out_path, start_cut = NA){
  #Create directory
  dir.create(out_path, showWarnings = FALSE)
  
  #Initial fit
  if (is.na(start_cut)){
    print("Working on initial fit\n")
    imp <- run_BRT(data, variables, out_path)
  }
  
  cuts <- c(1, 2, 3, 4, 5)
  
  if (!is.na(start_cut)){
    cuts <- seq(start_cut, 5, 1)
    folders <- list.files(out_path)
    folders <- folders[which(substr(folders, 1, 4) == "Less")]
    max_val <- max(as.numeric(substr(folders, 5, 5)))
    
    path <- ifelse(is.infinite(max_val), file.path(out_path, "Variable Importance.txt"),
                   file.path(out_path, paste0("Less", min(start_cut - 1, max_val)), "Variable Importance.txt"))
    imp <- read.csv(path, header = TRUE)
  }

  for (i in 1:length(cuts)){
    print(paste0("Working on new fit omitting variables <", cuts[i], "\n"))
    
    #Check that there are variables with less importance
    rm_vars <- as.character(imp$var[which(imp$rel.inf < cuts[i])])
    
    if (length(rm_vars) > 0){
      vars <- imp$var
      var_new <- as.character(vars[-which(vars %in% rm_vars)])
      new_path <- paste0(out_path, "/Less", cuts[i])
      dir.create(new_path, showWarnings = FALSE)
      
      data <- dplyr::select(data, dplyr::one_of(c(var_new, "chl_a")))
      
      imp <- run_BRT(data, var_new, new_path)
    }
  }
}



#FUNCTION TO CALIBRATE AND RUN BRT MODELS GIVEN A DATA SET AND MODEL VARIABLES
run_BRT <- function(data, variables, out_path){

  #Set up model
  
  model <- paste("log(chl_a) ~", paste(variables, collapse = " + "))
  ##Long Run 1
  
  ##First we determine which combinations of parameters yield the highest cross-validation Rsquared##
  
  ###Loop to choose values of bag.fraction and shrinkage that give min cv error (i.e. max cv.r.squared)###
  
  bag.frac <- c(0.5,0.6,0.7)                       #range of bag.fraction values to test
  shrin <- c(0.1,0.05,0.01,0.005,0.001,0.0005)     #range of shrinkage values to test
  comb <- expand.grid(bag.frac,shrin)              #all possible combinations
  
  n.rep <- 10                                      #repeat cross validation for each combination 10 times
  count <- seq(0,170,10)                           #Counting index for for loop
  cv.res <- matrix(NA, length(comb[,1])*n.rep,4)   #Matrix to store loop results
  colnames(cv.res) <- c("bf","lr","best.iter","cv.r.squared")
  
  pb <- txtProgressBar(min = 1, max = length(comb[,1]), style = 3)
  for(i in 1:length(comb[,1])){                  #Loop through each combination of parameter values
    
    bag.fraction<-comb[i,1]
    shrinkage<-comb[i,2]
    
    for(j in 1:n.rep){                           #Fit the model 10 times for each combination of parameter values
      fit_calib<- gbm(as.formula(model), data = data,
                          distribution = "gaussian", n.trees = 40000,
                          shrinkage = shrinkage, bag.fraction = bag.fraction, cv.folds=10, interaction.depth=2,
                          n.cores = 5)
      
      cv.res[count[i]+j,1]<-bag.fraction
      cv.res[count[i]+j,2]<-shrinkage
      cv.res[count[i]+j,3]<-gbm.perf(fit_calib,plot.it = F,oobag.curve = FALSE,overlay = TRUE,method="cv")
      cv.res[count[i]+j,4]<-((max(fit_calib$cv.error)- min(fit_calib$cv.error))/ max(fit_calib$cv.error))
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  Chla.cv<-aggregate(cv.res[,3], by=list(cv.res[,1],cv.res[,2]), FUN=mean) #mean of best number of trees across 10 repetitions 
  Chla.cv[,4]<-aggregate(cv.res[,3], by=list(cv.res[,1],cv.res[,2]), FUN=sd)[,3]   #standard deviation of best number of trees across 10 repetitions 
  
  Chla.cv[,5]<-aggregate(cv.res[,4], by=list(cv.res[,1],cv.res[,2]), FUN=mean)[,3] #mean of cv.r.squared across 10 repetitions 
  Chla.cv[,6]<-aggregate(cv.res[,4], by=list(cv.res[,1],cv.res[,2]), FUN=sd)[,3]   #standard deviation of cv.r.squared across 10 repetitions 
  colnames(Chla.cv)<-c("bf", "lr", "best.iter","best.iter.sd","cv.r.squared", "cv.r.squared.sd")
  
  Chla.cv.best.LAC<-Chla.cv[Chla.cv[,5]==max(Chla.cv[,5]),]   #Combination of parameter values giving highest cv.r.squared. Use these in the next section    
  
  ###########################################################################################################################################################
  
  #Run 2 (short)
  
  ##The next step is to run the BRT model once with the parameters we determined above
  ##Replace n.trees, shrinkage, bag.fraction
  
  fit_optim <- gbm(as.formula(model), data=data,
                      distribution = "gaussian", n.trees = Chla.cv.best.LAC[1,3],
                      shrinkage = Chla.cv.best.LAC[1,2], bag.fraction = Chla.cv.best.LAC[1,1], 
                   cv.folds=10, interaction.depth=2,
                      n.cores = 5)
  
  #summary(fit_optim)
  par(mfrow=c(1,1), mar=c(4,4,2,1))
  best.iter1_LAC<-gbm.perf(fit_optim,plot.it = TRUE,oobag.curve = FALSE,overlay = TRUE,method="cv")
  # print(fit_optim)
  # plot(fit_optim)
  # max(fit_optim$train.error)
  # min(fit_optim$train.error)
  # min(fit_optim$cv.error)
  # max(fit_optim$cv.error)
  
  cv.r.squared2<-((max(fit_optim$cv.error)- min(fit_optim$cv.error))/ max(fit_optim$cv.error))
  #cv.r.squared2
  tr.r.squared2<-((max(fit_optim$train.error) - min(fit_optim$train.error))/ max(fit_optim$train.error))
  #tr.r.squared2
  
  #######################################################################################################################
  
  #Long Run 3
  
  #Then we repeat the 10-fold cross validation 10 times (run model once first)
  #This cross validation is the real deal, we're fitting the model 10 times using 
  # 10 different sections or "folds" of the data, and predicting to the one part that is left out. It takes a super long time. 
  
  #First you store results of previous model run in first row of matrix, then you can just update it.
  
  cv.results10_LAC<-data.frame(n.trees=best.iter1_LAC,cv.r.squared=cv.r.squared2) 
  
  pb <- txtProgressBar(min = 2, max = 100, style = 3)
  for(i in 2:100){
    fit_optim2 <- update(fit_optim) #re-fit model using update function
    best.iter10b_LAC<-gbm.perf(fit_optim2, plot.it=F, method="cv") #Estimate optimal number of trees to use in plots
    cv.results10_LAC[i,1]<-best.iter10b_LAC
    cv.results10_LAC[i,2]<-((max(fit_optim2$cv.error)- min(fit_optim2$cv.error))/ max(fit_optim2$cv.error)) 
    
    setTxtProgressBar(pb, i)  
  }
  close(pb)
  
  mean(cv.results10_LAC$cv.r.squared)
  sd(cv.results10_LAC$cv.r.squared)
  
  best.tree <- round(mean(cv.results10_LAC$n.trees))
  sd(cv.results10_LAC$n.trees)
  
  
  #####################################################################################################################
  #ANALYSIS
  
  #Update path for each model - this is where all the results will be written
  #path <- "~/WORK/All_Lakes/Surface_Only_Less5" #CHANGE PATH
  path <- out_path
  var_importance <- summary(fit_optim2)
  write.table(var_importance, paste0(path, "/Variable Importance.txt"), col.names = TRUE, 
              row.names = FALSE, sep = ",")
  
  BR.MAX<-max(log(data$chl_a*10))
  chl_results <- list()
  n_var <- nrow(var_importance)
  
  for (i in 1:n_var){
    chl_results[[i]] <- plot.gbm(fit_optim2, i.var = i, n.trees = best.tree, return.grid = TRUE)
    chl_results[[i]]$Chl_a_modified <- chl_results[[i]]$y / BR.MAX
    chl_results[[i]]$var <- colnames(chl_results[[i]])[1]
    colnames(chl_results[[i]]) <- c("Value", "Chl_a", "Chl_a_modified", "Var")
  }
  
  chl_results <- do.call("rbind", chl_results)
  write.table(chl_results, paste0(path, "/Dependency Plot Results.txt"), col.names = TRUE,
              row.names = FALSE, sep = ",")
  
  
  #Write a bunch of other results
  write.table(cv.res, paste0(path, "/CV Res.txt"), col.names = TRUE, row.names = FALSE, sep = ",")
  write.table(Chla.cv.best.LAC, paste0(path, "/CV Best.txt"), col.names = TRUE, row.names = FALSE, sep = ",")
  write.table(Chla.cv, paste0(path, "/Chla CV.txt"), col.names = TRUE, row.names = FALSE, sep = ",")
  write.table(cv.results10_LAC, paste0(path, "/CV Results 10.txt"), col.names = TRUE, row.names = FALSE, sep = ",")
  
  #Write fitted values
  write.table(fit_optim2$fit, paste0(path, "/Fitted Chl_a.txt"),
              col.names = FALSE, row.names = FALSE, sep = ",")
  
  #R2 data
  write.table(data.frame(R2 = tr.r.squared2,
                         CVR2 = cv.r.squared2,
                         best_iter = best.iter1_LAC), paste0(path, "/R2 Values.txt"),
              col.names = TRUE, row.names = FALSE, sep = ",")
  write.table(data.frame(R2 = fit_optim$train.error,
                         CVR2 = fit_optim$cv.error),
              paste0(path, "/R2 by iterations.txt"),
              col.names = TRUE, row.names = FALSE, sep = ",")
  
  ##look for interactios between variables that may have multiplicative effects on the response
  
  interaction_results <- gbm.interactionIB(fit_optim, best.iter1_LAC, data) ## uses the results from Run 2 here due to code structure
  write.table(interaction_results[[1]], paste0(path, "/Interaction results.txt"), col.names = TRUE,
              row.names = FALSE, sep = ",")
  
  #Uses a modified version of Bella's interaction plots function - 
  #need to load it from wherever that file is saved on your computer
  source("~/Interactions Plot Function.R") #CHANGE PATH
  
  library(dplyr)
  interactions <- interaction_results[[1]] %>%
    filter(int.size > 0)
  for (i in 1:nrow(interactions)){
    plot_data <- gbm.perspecIB(fit_optim, best.tree, data,
                               interactions$var1.index[i], 
                               interactions$var2.index[i])
    write.csv(plot_data, paste0(path, "/Interaction plot_", 
                                interactions$var1.names[i], "_",
                                interactions$var2.names[i],
                                ".csv"))
  }
  
  return(var_importance)

}

########################################################################################################################
###This is the function to identify interactions between variables, easiest to just run it first and move on to line 162
########################################################################################################################

gbm.interactionIB<-function(gbm.object, #IB model fitted with gbm function from package gbm
                            n.trees,
                            data)                  #IB optimal number of trees as determined by gbm.perf function from package gbm
  #use.weights = FALSE,     #use weights for samples 
  #mask.object)         	   #a gbm object describing sample intensity 
{
  #
  # gbm.interactions version 2.9 
  #
  # j. leathwick, j. elith - May 2007 
  
  #(Code modified by IB, commented out code was from the original function created by Leathwick & Elith)
  #
  # Function assesses the magnitude of 2nd order interaction effects in gbm models fitted with interaction depths greater than 1.
  # This is achieved by:
  #   1. forming predictions on the linear scale for each predictor pair;
  #   2. fitting a linear model that relates these predictions to the predictor
  #        pair, with the predictors fitted as factors;
  #   3. calculating the mean value of the residuals, the magnitude of which
  #        increases with the strength of any interaction effect;
  #   4. results are stored in an array;
  #   5. finally, the n most important interactions are identified,
  #        where n is 25% of the number of interaction pairs;
  
  require(gbm)
  
  #gbm.call <- gbm.object$gbm.call
  #n.trees <- gbm.call$best.trees
  #n.trees<-best.iter
  #depth <- gbm.call$interaction.depth
  #gbm.x <- gbm.call$gbm.x
  #n.preds <- length(gbm.x)
  n.preds<-length(gbm.object$var.names) 
  #pred.names <- gbm.object$gbm.call$predictor.names
  pred.names<-gbm.object$var.names
  cross.tab <- matrix(0,ncol=n.preds,nrow=n.preds)
  dimnames(cross.tab) <- list(pred.names,pred.names)
  
  #if (use.weights) mask.trees <- mask.object$gbm.call$best.trees #IB dont need this
  
  cat("gbm.interactions - version 2.9 - modified by IB \n")
  cat("Cross tabulating interactions for gbm model with ",n.preds," predictors","\n",sep="")
  
  #data <- eval(parse(text=gbm.call$dataframe))[,gbm.x]  #IB selects columns of predictors used in the model
  data<-data[,gbm.object$var.names] #IB Selects columns of predictors used in the model                     (CHANGE ACCORDING TO DATASET USED!)
  
  for (i in 1:(n.preds - 1)) {  # step through the predictor set
    
    if (is.vector(data[,i])) {  # create a sequence through the range
      x.var <- seq(min(data[,i],na.rm=T),max(data[,i],na.rm=T),length = 20)
    }
    else {                      # otherwise set up simple factor variable
      x.var <- factor(names(table(data[,i])),levels = levels(data[,i]))
    }
    x.length <- length(x.var)
    
    cat(i,"\n")
    
    for (j in (i+1):n.preds) { #create vector or factor data for second variable
      
      if (is.vector(data[,j])) {
        y.var <- seq(min(data[,j],na.rm=T),max(data[,j],na.rm=T),length = 20)
      }
      else {
        y.var <- factor(names(table(data[,j])),levels = levels(data[,j]))
      }
      y.length <- length(y.var)
      
      # and now make a temporary data frame
      
      pred.frame <- expand.grid(list(x.var,y.var))
      names(pred.frame) <- c(pred.names[i],pred.names[j]) 
      
      n <- 3 # and add the balance of the variables to it
      
      for (k in 1:n.preds) {
        if (k != i & k != j) {
          if (is.vector(data[,k])) {  # either with the mean
            pred.frame[,n] <- mean(data[,k],na.rm=T)
          }
          else {   # or the most common factor level
            temp.table <- sort(table(data[,k]),decreasing = TRUE)
            pred.frame[,n] <- rep(names(temp.table)[1],x.length * y.length)
            pred.frame[,n] <- as.factor(pred.frame[,n])
          }
          names(pred.frame)[n] <- pred.names[k] 
          n <- n + 1
        }
      }
      #
      # form the prediction
      #
      #prediction <- predict.gbm(gbm.object,pred.frame,n.trees = n.trees, type="link") #IB Forms predictions
      prediction <- predict.gbm(gbm.object,pred.frame,n.trees = n.trees, type="response") #IB Forms predictions
      
      #if (use.weights) {
      #point.prob <- predict.gbm(mask.object[[1]],pred.frame, n.trees = mask.trees, type="response")
      #interaction.test.model <- lm(prediction ~ as.factor(pred.frame[,1]) + as.factor(pred.frame[,2]), 
      #weights = point.prob)
      #}
      
      #else {
      
      interaction.test.model <- lm(prediction ~ as.factor(pred.frame[,1]) + as.factor(pred.frame[,2]))
      #}
      
      interaction.flag <- round(mean(resid(interaction.test.model)^2) * 1000,2)
      
      cross.tab[i,j] <- interaction.flag
      
    }   # end of j loop
  }  # end of i loop
  
  # create an index of the values in descending order
  
  search.index <- ((n.preds^2) + 1) - rank(cross.tab, ties.method = "first")
  
  n.important <- max(2,round(0.1 * ((n.preds^2)/2),0))
  var1.names <- rep(" ",n.important)
  var1.index <- rep(0,n.important)
  var2.names <- rep(" ",n.important)
  var2.index <- rep(0,n.important)
  int.size <- rep(0,n.important)
  
  for (i in 1:n.important) {
    
    index.match <- match(i,search.index)
    
    j <- trunc(index.match/n.preds) + 1
    var1.index[i] <- j
    var1.names[i] <- pred.names[j]
    
    k <- index.match%%n.preds
    if (k > 0) {   #only do this if k > 0 - otherwise we have all zeros from here on 
      var2.index[i] <- k
      var2.names[i] <- pred.names[k] 
      
      int.size[i] <- cross.tab[k,j]
    }
    
  }
  
  rank.list <- data.frame(var1.index,var1.names,var2.index,var2.names,int.size)
  
  #return(list(rank.list = rank.list, interactions = cross.tab, gbm.call = gbm.object$gbm.call))
  list(rank.list = rank.list, interactions = cross.tab) #IB same as above line but without gbm.call
}