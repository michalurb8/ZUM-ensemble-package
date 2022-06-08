#' @details
#' Build an ensemble classification model with ensemble().
#'
#' Predict the results with predict().
#' @keywords internal
"_PACKAGE"

#' Creates an ensemble model based on any classification algorithm
#' @export
#' @param algorithm a classification algorithm
#' @param formula formula defining what attribute to classify
#' @param dataset dataframe containing training data
#' @param mcount number of models in the ensemble model
#' @param pattr percent of all attributes to be sampled for one model
#' @param ... named arguments passed to a single model
ensemble <- function(algorithm, formula, dataset, mcount=100, pattr=0.6, ...) {
  predicted_class <- as.character(formula[[2]])
  results <- list()
  for (i in 1:mcount) {
    attr_count <- max(1, floor(pattr * (ncol(dataset)-1)))

    bootstrapped <- dataset[sample(nrow(dataset), nrow(dataset), replace=TRUE),]
    #without_class <- bootstrapped[,!(names(bootstrapped) == predicted_class)]
    #attr_randomized <- sample(without_class, attr_count)
    #attr_randomized[,predicted_class] <- bootstrapped[,predicted_class]
    attr_names <- names(bootstrapped)
    attr_names <- attr_names[!attr_names == predicted_class]
    attr_names <- sample(attr_names, attr_count)

    single_formula <- paste(predicted_class, " ~ ", paste(attr_names, collapse="+"), sep="")
    single_formula <- as.formula(single_formula)

    new_model <- algorithm(formula=single_formula, data=dataset, ...)
    results[[i]] <- new_model
  }
  results[[i+1]] <- predicted_class
  ensemble_model <- structure(results, class="EnsembleModel")
  return(ensemble_model)
}

#' Predict method for the previously created ensemble model
#' @export
#' @param ensemble_model previously built ensemble model object
#' @param dataset dataframe containing test data
#' @param type if "class", returns a vector of classes, otherwise (default) a matrix of probabilities
#' @param single_predict a method for prediction, default is "predict"
predict.EnsembleModel <- function (ensemble_model, dataset, type="prob", single_predict=predict) {
  predicted_class <- ensemble_model[[length(ensemble_model)]]
  modelClasses <- levels(as.factor(dataset[,predicted_class]))
  predictions <- matrix(0, nrow=nrow(dataset), ncol=length(modelClasses))
  colnames(predictions) <- modelClasse s

  modelCount <- length(ensemble_model)

  for (i in 1:(length(ensemble_model)-1)) {
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
    levels(class_list) <- modelClasses
    return(class_list)
  }
  return(predictions)
}

