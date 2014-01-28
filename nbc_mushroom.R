args<-commandArgs(TRUE)
nbc_mushroom <- function(training.dataset, test.dataset, output.filename){  
  cat("Running...")
  ##read data from file to a data frame
  training.table <- read.table(training.dataset)
  test.table <- read.table(test.dataset)
  
  ##retrieve class and features from training data
  training.class <- training.table[, 1]
  training.features <- training.table[,-1]
  remove(training.table)
  
  ##funciton for calculating priors
  calculate.priors <- function(class.vector){
    priors <- c()
    for (class in unique(class.vector)){
      priors <- rbind(priors, c(class, length(class.vector[class.vector==class])/length(class.vector)))
      colnames(priors) <- c("classification", "probability")
    }
    return (priors)
  }
  priors <- calculate.priors(training.class)
  
  ##Learn the features by calculating likelihood
  likelihood.list <- list()
  #calculate CPD by feature
  for (i in 1:dim(training.features)[2]){
    feature.values <- training.features[, i]
    unique.feature.values <- unique(feature.values)
    likelihood.matrix <- matrix(rep(NA), nrow=dim(priors)[1], ncol=length(unique.feature.values))
    colnames(likelihood.matrix) <- unique.feature.values
    rownames(likelihood.matrix) <- priors[, "classification"]
    for (item in unique.feature.values){
      likelihood.item <- vector()
      for (class in priors[, "classification"]){
        feature.value.inclass <- feature.values[training.class==class]
        likelihood.value <- length(feature.value.inclass[feature.value.inclass==item])/length(feature.value.inclass)
        likelihood.item <- c(likelihood.item, likelihood.value)
      }
      likelihood.matrix[, item] <- likelihood.item
    }
    likelihood.list[[i]] <- likelihood.matrix
  }
    
  
  ##Predict class for the test dataset
  #retrieve the features and target class of the testing dataset
  test.features <- test.table[, -1]
  test.target.class <- test.table[, 1]
  test.predict.class <- rep(NA, length(test.target.class))
  remove(test.table)
  
  #calculate posteriors for each test data record
  for (i in 1:dim(test.features)[1]){
    record <- test.features[i, ]
    posterior <- vector()
    #calculate posteriors for each possible class of that record
    for (class in priors[, "classification"]){
      #initialize posterior as the prior value of that class
      posterior.value <- as.numeric(priors[priors[, "classification"]==class, 2])
      likelihood.v <- c()
      for (item in 1:length(record)){
        likelihood.value <- likelihood.list[[item]][class, as.character(record[1, item])]
        likelihood.v <- c(likelihood.v, likelihood.value)
        posterior.value <- as.numeric(posterior.value) * as.numeric(likelihood.value)
      }
      posterior <- rbind(posterior, c(class, posterior.value)) 
    }
    predict.class <- posterior[posterior[,2]==max(as.numeric(posterior[,2])),1]
    test.predict.class[i] <- predict.class
  }
  accuracy <- length(test.predict.class[test.predict.class==test.target.class])/length(test.target.class)
  test.output <- cbind(test.features, test.target.class, test.predict.class)
  
  #print result and export to file
  file.con <- file(output.filename)
  
  write("Naive Bayes Classification Results\n", file=output.filename)
  #write("\n", file = output.filename, append = TRUE)
  write(paste("The prior probability for P(y=p) is: ", priors[priors[, 1]=="p" ,2], sep=""), file=output.filename, append=TRUE)
  write(paste("The prior probability for P(y=e) is: ", priors[priors[, 1]=="e" ,2], "\n", sep=""), file=output.filename, append=TRUE)
  write("Next, print the likelihood value for all the 21 features\n", file=output.filename, append=TRUE)
  for (i in 1:length(likelihood.list)){
    write(paste("\nLikelihood for feature", i, "\n"), file=output.filename, append=TRUE)
    write.table(data.frame(likelihood.list[[i]]), file=output.filename, eol="\n", append=TRUE)
  }
  write("\n\nNext, print the test records with target class and predicted class\n", file=output.filename, append=TRUE)
  write.table(test.output, file=output.filename, eol="\n", append=TRUE)
  write(paste("\n\nFinally, with the above Naive Bayes classifier, the prediction accuracy is", accuracy,"\n"), file=output.filename, append=TRUE)
  cat("\nCompleted! Output in file:", output.filename, "\n")
}
nbc_mushroom(args[1], args[2], args[3])
