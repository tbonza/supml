#' Implement Naive Bayes

# Generic methods

predict_proba <- function(obj, X) { UseMethod("predict_proba") }

fit <- function(obj, X, y) { UseMethod("fit") }

kernelDensity <- function(obj, x, mu=0) { UseMethod("kernelDensity") }

score <- function(obj, y, probs) { UseMethod("score") }

gridSearch <- function(obj, X, y, ...) { UseMethod("gridSearch") }

#' Bayesian object
#'
#' Used for Bayesian classification with kernel 
#' density estimation
#'
#' @param lambda kernel bin width
#' @param kernel, string name of kernel to use
#' @return s3 object, bayes
#'
#' @export
bayes <- function(lambda, kernel="gaussian"){
    value <- list(lambda = lambda, kernel = kernel,
                  classes = c(), models = list(),
                  logpriors = c())
    attr(value, "class") <- "bayes"
    return(value)
}

# Bayesian methods

#' Gaussian Kernel Density Estimate for Naive Bayes
#' 
#' Implements HT (6.23)
#'
#' @param obj requires instantiated Bayes object
#' @param x continuous feature vector
#' @return fhat, point estimate with gaussian kernel
#'
#' @export
kernelDensity.bayes <- function(obj, x, mu=0){
    lambda <- obj$lambda
    kernel <- obj$kernel # assume gaussian for now

    if (!is.null(nrow(x))) { N <- nrow(x) }
    else { N <- length(x) }
    
    estimates <- sapply(x, gauss_kern, mu, lambda)
    fhat <- sum(estimates) / N

    return(fhat)
}
 

#' Fit training data to Bayesian Classifier
#'
#' We want to assign models to each class,
#' accounting for continuous and categorical
#' variables. We also need the log priors
#' to avoid issues with floating point.
#'
#' @param obj bayes s3 object
#' @param X feature matrix
#' @param y target vector
#' @return obj updated bayes s3 object
#'
#' @export
fit.bayes <- function(obj, X, y){

    # Classes

    if (!is.factor(y)) { stop("y must be of type Factor") }
    obj$classes <- levels(y)

    # Define training sets

    training_sets <- list()
    for (yi in obj$classes){ training_sets[[yi]] <- X[y == yi,] }
    
    # Models

    N <- length(y)
    models <- list()
    logpriors <- list()
    
    for (name in names(training_sets)){

        Xc <- training_sets[[name]]

        kmodels <- list()
        klogpriors <- list()

        # p(c)

        p_c <- paste0("p(", name, ")")
        kmodels[[p_c]] <- sum(y == name) / length(y)

        for (j in 1:ncol(Xc)){
 
            Xtrain <- Xc[,j]

            # Categorical Variable

            if (is.factor(Xc[,j])){

                p_xc <- paste0("p(", j, "|c)")
                kmodels[[p_xc]] <- (sum(Xtrain == name))  / sum(y == name)

                # Log prior

                klogpriors[[j]] <- log(ifelse(kmodels[[p_xc]] == 1, 0.99, kmodels[[p_xc]]))
            }
        
            # Continuous 

            else if (is.numeric(Xc[,j])){

                # p(x|c)

                p_xc <- paste0("p(", j, "|c)")
                kmodels[[p_xc]] <- Xc[,j]

                # Log prior

                pr <- (length(Xtrain)) / N
                klogpriors[[j]] <- log(ifelse(pr == 1, 0.99, pr))
            }

            # Otherwise, error

            else {
             stop(sprintf("X vector %.0f is not a Factor or Numeric type", j))
            }            
        }

        models[[name]] <- kmodels
        logpriors[[name]] <- klogpriors
        
    }

    obj$models <- models
    obj$logpriors <- logpriors
    
    return(obj)
}

lookup <- function(name, cond=FALSE){
    if (cond == TRUE){
        return(paste0("p(", name, "|c)"))
    }
    else {
        return(paste0("p(", name, ")"))
    }
}

#' Predicts the log probabilities for each class and feature
#'
#' @param obj s3 object, bayes
#' @param X feature vector
#' @return bayes s3 object
#'
#' @export
predict_proba.bayes <- function(obj, X){

    proba <- list()

    for (i in 1:nrow(X)){

        proba[[i]] <- list()
        
        for (name in obj$classes){

            probs <- c()
            
            for (j in 1:ncol(X)){

                val <- X[i,j]
                prior <- obj$models[[name]][[lookup(name)]]

                probs <- c(probs, log(prior))

                # Categorical

                if (is.factor(val)){
                    
                    if (name == val){
                        cond <- obj$models[[name]][[lookup(j, cond=TRUE)]]
                        probs <- c(probs, log(cond))
                    }
                    else {
                        # Choose low value other than zero
                        probs <- c(probs, log(0.1 * 10^(-5)))
                    }
                }
                else if (is.numeric(val)){

                    cond <- obj$models[[name]][[lookup(j, cond=TRUE)]]
                    mu <- mean(cond)
                    probs <- c(probs, log(kernelDensity(obj, val, mu)))
                }
                else {
                    stop(sprintf("X vector %.0f is not a Factor or Numeric type", j))
                }
            }

            proba[[i]][[name]] <- sum(probs)
        }
    }
    
    obj$proba <- proba
    return(obj)
}

#' Predict log probabilities for each class K
#'
#' @param object s3 object bayes
#' @param X feature matrix
#' @return s3 object bayes with prediction attribute
#'
#' @export
predict.bayes <- function(object, ...){

    X <- list(...)$X
    obj <- object

    # Log probabilties for each feature by class

    obj <- predict_proba(obj, X)
    probs <- obj$proba

    # Find and report class likelihood

    df <- list()
    for (name in obj$classes){

        vec <- c()
        for (i in probs){

            vec <- c(vec, i[[name]])
        }

        df[[name]] <- vec
    }

    df <- as.data.frame(df)
    colnames(df) <- obj$classes

    obj$predictions <- df
    return(obj)
}

#' Accuracy scoring function for Binomial Naive Bayes
#'
#' Converts log probabilities to probabilities before
#' scoring.
#'
#' @param obj bayes s3 object
#' @param y target vector for training set
#' @return float, accuracy score
#' 
#' @export
score.bayes <- function(obj, y) {

    df <- obj$predictions
    yhat <- ifelse(exp(df$`0`) > exp(df$`1`), "0", "1")

    percent_correct <- sum(yhat == y) / length(y)
    return(percent_correct)
}

#' Implement grid search for the bayes object
#'
#' @param obj s3 object bayes
gridSearch.bayes <- function(obj, X, y, grid, score){

    max_score <- -Inf
    best_lambda <- -Inf

    vscores <- c()
    vlambda <- c()
    for (lambda in grid$lambda){

        b <- bayes(lambda=lambda)
        b <- fit(b, X, y)
        b <- predict(b, X)
        current_score <- score(b, y)

        if (current_score > max_score){
            max_score <- current_score
            best_lambda <- lambda
        }

        vscores <- c(vscores, current_score)
        vlambda <- c(vlambda, lambda)
    }

    scoredf <- cbind(vlambda, vscores)

    b$gridSearch <- list(best_lambda=best_lambda, max_score = max_score,
                         scoredf= scoredf)

    return(b)
}
