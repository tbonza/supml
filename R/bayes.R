#' Implement Naive Bayes

# Generic methods

predict_proba <- function(object, X) { UseMethod("predict_proba") }

fit <- function(object, X, y) { UseMethod("fit") }

spatialProbs <- function(object, X,y) { UseMethod("spatialProbs") }

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
                  logpriors = c())
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

#' Intermediate Fitting step for Spatial data
spatialProbs.bayes <- function(object, X, y){

    # Manually set priors due to imbalanced classes

    spatial_priors <- object$map$spatial_priors
    kernel <- object$map$kernels['spatial']
    k <- object$map$kblocks

    # Compute kernel block count

    i <- 1:nrow(X)
    blocks <- matrix(nrow=nrow(X), ncol=ncol(X))
    for (j in 1:ncol(X)){

        estimates <- kernelDensity(object, x=i, kernel=kernel,
                                   m=X, j=j, k=k)

        blocks[,j] <- estimates
    }

    # Compute prior times conditional probs for each class in y

    lnprobs <- list()
    for (label in object$classes){

        ksum <- sum(blocks[y==label,])
        tsum <- sum(X[y==label,])

        if (ksum == 0 | tsum == 0){
            lnprob <- log(0.0001)
        }
        else if (ksum == tsum){
            lnprob <- log(0.9999)
        }
        else {
            lnprob <- log(ksum) - log(tsum)
        }
        lnprobs[[label]] <- log(spatial_priors[[label]]) + lnprob
    }
    
    return(lnprobs)
}

#' Intermediate Fitting step for Continuous data
continuous_probs.bayes <- function(object, X, y){
}

#' Intermediate Fitting step for Categorical data
categorical_probs.bayes <- function(object, X,y){
}

#' Fit training data to Bayesian Classifier
#'
#' We want to assign models to each class,
#' accounting for continuous and categorical
#' variables. We also need the log priors
#' to avoid issues with floating point.
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
    categorical_cols <- object$map$categorical
    continuous_cols <- object$map$continuous

    # Process spatial data


    # Process continuous and categorical data

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
