#' Implement Naive Bayes Classifier

# Constants used for Bayesian classifier

LOGMIN <- 0.0001
LOGMAX <- 0.9999

# Generic methods

predict_proba <- function(object, X) { UseMethod("predict_proba") }

fit <- function(object, X, y) { UseMethod("fit") }

spatialProbs <- function(object, X,y) { UseMethod("spatialProbs") }
categoricalProbs <- function(object, X, y) { UseMethod("categoricalProbs") }
priorProbs <- function(object, y) { UseMethod("priorProbs") }

kernelDensity <- function(object, x, kernel, ...) {
    UseMethod("kernelDensity") }

score <- function(object, y, probs) { UseMethod("score") }

gridSearch <- function(object, X, y, ...) { UseMethod("gridSearch") }

#' Bayesian object
#'
#' Used for Bayesian classification with kernel 
#' density estimation
#'
#' @param map list of variable types for X
#' @return s3 object, bayes
#'
#' @export
bayes <- function(map){
    value <- list(map = map, classes = c(), models = list(),
                  logpriors = list())
    attr(value, "class") <- "bayes"
    return(value)
}

# Bayesian methods

#' Kernel Density Estimate for Naive Bayes
#' 
#' Implements HT (6.23), implements K-Block kernel
#'
#' @param object requires instantiated Bayes object
#' @param x feature vector
#' @param kernel name of kernel ("gaussian", "kblock")
#' @param ... kernel specific arguments
#' @return kernel estimate
#'
#' @export
kernelDensity.bayes <- function(object, x, kernel, ...){

    arg = list(...)

    if (kernel == "gaussian"){

        if (!is.null(nrow(x))) { N <- nrow(x) }
        else { N <- length(x) }
    
        mu <- arg$mu
        lambda <- arg$lambda
    
        estimates <- sapply(x, gauss_kern, mu, lambda)
        fhat <- sum(estimates) / N
        
        return(fhat)
    }
    else if (kernel == "kblock"){
        # count of number of k blocks existing within x
        j <- arg$j
        k <- arg$k
        m <- arg$m

        estimates <- sapply(x, kblock_kern, j=j, m=m, k=k)
        
        return(estimates)
    }
    else { stop("Unknown kernel specified") }
}

#' Conditional probability fitting step for Spatial data
#'
#' This method should only be used for customizing your
#' own Bayesian classifier.
#' 
#' @param object bayesian s3 object with mappings set
#' @param X boolean spatial feature matrix
#' @param y target vector, factor
#' @return natural log probabilities for each class in y
#'
#' @export
spatialProbs.bayes <- function(object, X, y){

    # Retrieve hyperparameters

    kernel <- object$map$kernels['spatial']
    k <- object$map$hyperparameters['kblocks']

    # Compute kernel block count

    i <- 1:nrow(X)
    blocks <- matrix(nrow=nrow(X), ncol=ncol(X))
    for (j in 1:ncol(X)){

        estimates <- kernelDensity(object, x=i, kernel=kernel,
                                   m=X, j=j, k=k)

        blocks[,j] <- estimates
    }

    # Compute conditional probs for each class in y

    lnprobs <- list()
    for (label in object$classes){

        ksum <- sum(blocks[y==label,])
        tsum <- sum(X[y==label,])

        if (ksum == 0 | tsum == 0){
            lnprob <- log(LOGMIN)
        }
        else if (ksum == tsum){
            lnprob <- log(LOGMAX)
        }
        else {
            lnprob <- log(ksum) - log(tsum)
        }
        lnprobs[[label]] <- lnprob
    }
    
    return(lnprobs)
}

#' Conditional probability fitting step for Categorical data
#'
categoricalProbs.bayes <- function(object, X, y){
}

#' Prior probability fitting step for Continous/Categorical data
#' 
#' Compute prior probs for each class in y. This method
#' should only be used by customizing your own Bayesian
#' classifier.
#'
#' @param object bayesian s3 object
#' @param y target vector, vector
#' @param natural log probabilities for each class y given |y|
#'
#' @export
priorProbs.bayes <- function(object, y) {

    y_count <- as.data.frame(table(y))
    y_len <- length(y)

    y_count['logprobs'] <- log(y_count$Freq) - log(y_len)
    y_count['logprobs'][y_count$Freq == y_len,] <- log(LOGMAX)
    y_count['logprobs'][y_count$Freq == 1,] <- log(LOGMIN)

    priors <- as.data.frame(y_count$y)
    priors <- cbind(priors, y_count$logprobs)
    colnames(priors) <- c("y", "logprobs")
    
    return(priors)
}

#' Fit training data to Bayesian Classifier
#'
#' We want to assign models to each class,
#' accounting for continuous and categorical
#' variables. We also handle spatial data.
#' Log priors are used to avoide issues with
#' floating point.
#'
#' @param object bayes s3 object
#' @param X feature matrix
#' @param y target vector
#' @return obj updated bayes s3 object
#'
#' @export
fit.bayes <- function(object, X, y){

    # Classes

    if (!is.factor(y)) { stop("y must be of type Factor") }
    object$classes <- levels(y)

    spatial_cols <- object$map$spatial
    continuous_cols <- object$map$continuous
    categorical_cols <- object$map$categorical

    logprobs <- list()

    # Compute conditional probs for spatial data

    logprobs[['spatial']] <- spatialProbs(object, X[,spatial_cols], y)

    # Compute conditional probs for continuous data

    ypriors <- priorProbs(object, y)
    logprobs[['continuous']] <- ypriors
    
    # Compute conditional probs for categorical data

    logprobs[['categorical']] <- categoricalProbs(object,
                                                  X[,categorical_cols], y)

    # Compute priors for continuous & categorical data
    
    logprobs[['priors']] <- ypriors

    # Cache log prior and conditional probabilities

    object$logpriors <- logprobs

    return(object)
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
predict.bayes <- function(object, X){
    
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
