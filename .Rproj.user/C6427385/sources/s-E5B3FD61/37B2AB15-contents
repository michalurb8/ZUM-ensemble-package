#' Creates an ensemble model based on any classification algorithm
#' @export
#' @param algorithm a classification algorithm
#' @param formula formula defining what attribute to classify
#' @param dataset dataframe containing training data
#' @param mcount number of models in the ensemble model
#' @param pattr percent of all attributes to be sampled for one model
#' @param ... named arguments passed to a single model
ensemble <- function(algorithm, formula, dataset, mcount=10, pattr=0.8, ...) {
  results <- list()
  for (i in 1:mcount) {
    attr_count <- max(1, floor(pattr * (ncol(dataset)-1)))

    bootstrapped <- sample_n(dataset, nrow(dataset), replace=T)

    attr_randomized <- sample(bootstrapped[,-1], attr_count)

    attr_randomized$class <- bootstrapped$class

    new_model <- algorithm(formula, data=attr_randomized, ...)
    results[[i]] <- new_model
  }
  ensemble_model <- structure(results, class="EnsembleModel")
  return(ensemble_model)
}


#' Predict method for the created earlier ensemble model
#' @export
#' @param ensemble_model previously built ensemble model object
#' @param dataset dataframe containing test data
#' @param type if "class", returns a vector of classes, otherwise (default) a matrix of probabilities
#' @param single_predict a method for prediction, default is "predict"
predict.EnsembleModel <- function (ensemble_model, dataset, type="prob", single_predict=predict) {
  modelClasses <- levels(dataset$class)
  predictions <- matrix(0, nrow=nrow(dataset), ncol=length(modelClasses))
  colnames(predictions) <- modelClasses

  modelCount <- length(ensemble_model)

  for (i in 1:length(ensemble_model)) {
    newPrediction <- single_predict(ensemble_model[[i]], dataset, type="class")

    for (j in 1:length(newPrediction)) {
      predictions[j, newPrediction[j]] = predictions[j, newPrediction[j]] + 1/modelCount
    }
  }
  if (type == "class")
  {
    class_list <- c()
    for (i in 1:nrow(predictions)) {
      class_list[i] <- colnames(predictions)[which.max(predictions[i,])]
    }
    class_list <- as.factor(class_list)
    return(class_list)
  }
  return(predictions)
}
