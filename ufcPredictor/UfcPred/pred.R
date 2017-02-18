#Code to setup the proper installation of packages needed for caret
if (!require(caret)) {
  install.packages("caret", dependencies = c("Depends", "Suggests"))
}
library(doMC)
library(parallel)
library(doParallel)
registerDoMC(cores = detectCores() - 1)

library(caret)
#library(rattle)
library(mlbench)
library(ggplot2)
library(ggthemes)
library(rbenchmark)

library(formatR)
library(plyr)
library(dplyr)
library(nnet)
library(rpart)
library(microbenchmark)
library(GGally)


mainDir <- "."
subDirs <- list("metrics","metrics/ufc")
lapply(subDirs, function(subDir) ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE))

# data process
data.ufc <- read.csv("./training.csv")
data.ufc <- na.omit(data.ufc)
data.ufc$EventDate <- as.Date(data.ufc$EventDate)


# bar chart to show how the sample distribution is to determine the type of metric to choose
data.graph.distrib <- ggplot(data.ufc, aes(x = Class, fill = Class)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = -1) + labs(title = "Cancer Class Distribution") + 
  theme_hc()


#pairs(Class ~ ., data=data.bc, col=data.bc$Class)

ucfForm <- "Result ~ Fighter1Height + Figther1Weight + Fighter1Country + Fighter2Height + Figther2Weight + Fighter2Country + Ref"

set.seed(998)


decisiontree.cart <- function(data, 
                              y = "Result", 
                              formula = as.formula(ucfForm), 
                              modelFileName = NULL,
                              trainSampleSize = 0.7, 
                              positive = "f1", 
                              method = "rpart", 
                              tuneLength = NULL, 
                              tuneGrid = NULL) {
  
  inTraining <- createDataPartition(data[, y], p = trainSampleSize, list = FALSE)
  training <- data[inTraining, ]
  testing <- data[-inTraining, ]
  
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  
  training.time <- microbenchmark(fit <- caret::train(form = formula, 
                                                      data = training,
                                                      method = method, 
                                                      trControl = fitControl,
                                                      tuneGrid = tuneGrid,
                                                      #trace = F,
                                                      tuneLength = tuneLength), 
                                  times = 1, 
                                  unit = "nanoseconds")$time * 1e-9
  
  
  testing.train.time <- microbenchmark(ISplsClasses <- predict(fit, newdata = training), times = 1, unit = "nanoseconds")$time * 1e-9
  cm.is <- caret::confusionMatrix(data = ISplsClasses, training[, y], positive = positive)
  
  
  testing.time <- microbenchmark(plsClasses <- predict(fit, newdata = testing), times = 1, unit = "nanoseconds")$time * 1e-9
  cm <- caret::confusionMatrix(data = plsClasses, testing[, y], positive = positive)
  
  
  #fancyGraph <- fancyRpartPlot(fit$finalModel)
  stats <- list(cm = cm,
                insamplecm = cm.is,
                fit = fit,
                trainingTime = training.time[[1]], 
                testingTime = testing.time[[1]],
                
                # in sample metrics
                isAccuracy = cm.is$overall["Accuracy"][[1]],
                isKappa = cm.is$overall["Kappa"][[1]],
                isAccuracyLower = cm.is$overall["AccuracyLower"][[1]],
                isAccuracyUpper = cm.is$overall["AccuracyUpper"][[1]],
                isAccuracyNull = cm.is$overall["AccuracyNull"][[1]],
                isAccuracyPValue = cm.is$overall["AccuracyPValue"][[1]],
                isMcnemarPValue = cm.is$overall["McnemarPValue"][[1]],
                isSensitivity = cm.is$byClass["Sensitivity"][[1]],
                isSpecificity = cm.is$byClass["Specificity"][[1]],
                isPosPredValue = cm.is$byClass["Pos Pred Value"][[1]],
                isNegPredValue = cm.is$byClass["Neg Pred Value"][[1]],
                isPrecision = cm.is$byClass["Precision"][[1]],
                isRecall = cm.is$byClass["Recall"][[1]],
                isF1 = cm.is$byClass["F1"][[1]],
                isPrevalence = cm.is$byClass["Prevalence"][[1]],
                isDetectionRate = cm.is$byClass["Detection Rate"][[1]],
                isDetectionPrevalence = cm.is$byClass["Detection Prevalence"][[1]],
                isBalancedAccuracy = cm.is$byClass["Balanced Accuracy"][[1]],
                
                # out sample metrics
                Accuracy = cm$overall["Accuracy"][[1]],
                Kappa = cm$overall["Kappa"][[1]],
                AccuracyLower = cm$overall["AccuracyLower"][[1]],
                AccuracyUpper = cm$overall["AccuracyUpper"][[1]],
                AccuracyNull = cm$overall["AccuracyNull"][[1]],
                AccuracyPValue = cm$overall["AccuracyPValue"][[1]],
                McnemarPValue = cm$overall["McnemarPValue"][[1]],
                Sensitivity = cm$byClass["Sensitivity"][[1]],
                Specificity = cm$byClass["Specificity"][[1]],
                PosPredValue = cm$byClass["Pos Pred Value"][[1]],
                NegPredValue = cm$byClass["Neg Pred Value"][[1]],
                Precision = cm$byClass["Precision"][[1]],
                Recall = cm$byClass["Recall"][[1]],
                F1 = cm$byClass["F1"][[1]],
                Prevalence = cm$byClass["Prevalence"][[1]],
                DetectionRate = cm$byClass["Detection Rate"][[1]],
                DetectionPrevalence = cm$byClass["Detection Prevalence"][[1]],
                BalancedAccuracy = cm$byClass["Balanced Accuracy"][[1]],
                samplePercent = trainSampleSize, 
                trainsampleSize = dim(training)[[1]], 
                testSampleSize = dim(testing)[[1]])
  
  populateMetric(modelFileName, stats)
  
  return(stats)
}

populateMetric <- function(filename, dataframe){
  if(!file.exists(filename)){
    saveRDS(dataframe, filename)
    return(dataframe)
  }else {
    
    tryCatch(return(readRDS(filename)),
             error=function(cond) {
               file.remove(filename)
             }
    )  
    
  }
}

santizeDf <- function(df){
  df$fit <- NULL
  df$cm <- NULL
  df$insamplecm <- NULL
  df <- as.data.frame(sapply( df, as.numeric ))
  return(df)
}

#diffSamples <- seq(10, 90, by = 10) / 100
diffSamples <- c(.7)

decisiontree.params <- list(
  
  # list(name = "rcart", 
  #      data = data.ufc, 
  #      metricFileName = "./metrics/rcart.Rds", 
  #      modelFileName = "./metrics/rcart-train-",
  #      method = "rpart",
  #      tuneLength = 5,
  #      tuneGrid = NULL
  # ),
  # 
  # list(name = "rcart-bag", 
  #      data = data.ufc, 
  #      metricFileName = "./metrics/rcart-bag.Rds", 
  #      modelFileName = "./metrics/rcart-bag-train-",
  #      method = "treebag",
  #      tuneLength = 5,
  #      tuneGrid = NULL
  # ),
  # 
  # 
  # list(name = "c5", 
  #      data = data.ufc, 
  #      metricFileName = "./metrics/c5.Rds", 
  #      modelFileName = "./metrics/c5-train-",
  #      method = "C5.0",
  #      tuneLength = NULL,
  #      tuneGrid =   expand.grid(trials = seq(from = 1, to = 9, by = 2), model = "tree", winnow = c(TRUE,FALSE))
  # ),
  # 
  # 
  # list(name = "nn-grid2", 
  #      data = data.ufc, 
  #      metricFileName = "./metrics/nn-grid2.Rds", 
  #      modelFileName = "./metrics/nn-grid2-train-",
  #      method = "nnet",
  #      tuneLength = NULL,
  #      tuneGrid =  expand.grid(decay = seq(from = 0, to = 1, by =0.2), size = c(1,3,5, 7))
  # ),
  # 
  # list(name = "nn-grid3",
  #      data = data.ufc,
  #      metricFileName = "./metrics/nn-grid3.Rds",
  #      modelFileName = "./metrics/nn-grid3-train-",
  #      method = "mlpWeightDecayML",
  #      tuneLength = NULL,
  #      tuneGrid =  expand.grid(decay = c(0, .01), layer1 = 1:3, layer2 = 1:3, layer3 = 1:3)
  # ),
  # 
  # 
  # list(name = "nn-act",
  #      data = data.ufc,
  #      metricFileName = "./metrics/nn-act.Rds",
  #      modelFileName = "./metrics/nn-act-train-",
  #      method = "elm",
  #      tuneLength = NULL,
  #      tuneGrid =  expand.grid(nhid = c(0, 1,3,5,10,50), actfun = c('sig','radbas', 'tansig', 'sin'))
  # ),
  # 
  # list(name = "adaboostm1", 
  #      data = data.ufc, 
  #      metricFileName = "./metrics/adaboostm1.Rds", 
  #      modelFileName = "./metrics/adaboostm1-train-",
  #      method = "AdaBoost.M1",
  #      tuneLength = NULL,
  #      tuneGrid =  expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3), coeflearn = c("Breiman", "Freund", "Zhu"))
  # ),
  # 
  # 
  # list(name = "svmlin",
  #      data = data.ufc,
  #      metricFileName = "./metrics/svmpoly.Rds",
  #      modelFileName = "./metrics/svmpoly-train-",
  #      method = "svmLinear",
  #      tuneLength = 4,
  #      tuneGrid =  NULL
  # ),
  # 
  # list(name = "svmrad",
  #      data = data.ufc,
  #      metricFileName = "./metrics/svmrad.Rds",
  #      modelFileName = "./metrics/svmrad-train-",
  #      method = "svmRadial",
  #      tuneLength = NULL,
  #      tuneGrid = data.frame(.C = c(.25, .5, 1,2), .sigma = c(.05, .5,1,2))
  # ),

  
  list(name = "knn", 
       data = data.ufc, 
       metricFileName = "./metrics/knn.Rds", 
       modelFileName = "./metrics/knn-train-",
       method = "knn",
       tuneLength = 4
       #tuneGrid =  expand.grid(k = c(1,2,3,5,7,9,11,15,20))
  )
)

useParallel <- FALSE

if(useParallel){
  tryCatch({
    cl <- makeCluster(detectCores() - 1, type = "PSOCK")
    
    clusterExport(cl, "decisiontree.cart")
    clusterExport(cl, "decisiontree.params")
    clusterExport(cl, "createDataPartition")
    clusterExport(cl, "data.bc")
    clusterExport(cl, "data.letter")
    
    clusterExport(cl, "expand.grid")
    clusterExport(cl, "populateMetric")
    clusterExport(cl, "santizeDf")
    clusterExport(cl, "diffSamples")
    clusterExport(cl, "trainControl")
    clusterExport(cl, "microbenchmark")
    clusterExport(cl, "confusionMatrix")
    clusterExport(cl, "predict")
    clusterExport(cl, "train")
    #clusterExport(cl, "parSapply")
    
    registerDoParallel(cl)
    
    
    for(i in 1:length(decisiontree.params)){
      l <- decisiontree.params[[i]]
      clusterExport(cl, "l")
      print(l$name)
      populateMetric(l$metricFileName,
                     santizeDf(as.data.frame(t(parSapply(cl,diffSamples,
                                                         function(s) return(decisiontree.cart( l$data,
                                                                                               # l$y,
                                                                                               # as.formula(l$formula),
                                                                                               modelFileName = paste(c(l$modelFileName,s*100,".Rds"), collapse = ''),
                                                                                               tuneLength = l$tuneLength,
                                                                                               tuneGrid = l$tuneGrid,
                                                                                               trainSampleSize=s,
                                                                                               method = l$method))))) ))
    }
  },
  error =function(cond) {
    print("ERROR ==============================")
    print(cond)
  },
  finally = stopCluster(cl)
  ) 
  
  
  
}else {
  
  for(i in 1:length(decisiontree.params)){
    l <- decisiontree.params[[i]]
    message(l$name)
    populateMetric(l$metricFileName,
                   santizeDf(as.data.frame(t(sapply(diffSamples,
                                                    function(s) return(decisiontree.cart( l$data,
                                                                                          # l$y,
                                                                                          # as.formula(l$formula),
                                                                                          modelFileName = paste(c(l$modelFileName,s*100,".Rds"), collapse = ''),
                                                                                          tuneLength = l$tuneLength,
                                                                                          tuneGrid = l$tuneGrid,
                                                                                          trainSampleSize=s,
                                                                                          method = l$method))))) ))
  }
}
