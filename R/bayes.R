#' Implement Naive Bayes Classifier

# Constants used for Bayesian classifier

LOGMIN <- 0.0001
LOGMAX <- 0.9999

# Generic methods

#' @export
predict_proba <- function(object, X) { UseMethod("predict_proba") }

#' @export
fit <- function(object, X, y) { UseMethod("fit") }

#' @export
spatial_fit <- function(object, X, y) { UseMethod("spatial_fit") }

#' @export 
spatialProbs <- function(object, X,y) { UseMethod("spatialProbs") }

#' @export
continuousProbs <- function(object, X, y) { UseMethod("continuousProbs") }

#' @export
categoricalProbs <- function(object, X, y) { UseMethod("categoricalProbs") }

#' @export
priorProbs <- function(object, y) { UseMethod("priorProbs") }

#' @export
postSpatialProbs <- function(object, X) { UseMethod("postSpatialProbs") }

#' @export
kernelDensity <- function(object, x, kernel, ...) {
    UseMethod("kernelDensity") }

#' @export
score <- function(object, y, probs) { UseMethod("score") }

#' @export
confusionMatrix <- function(object, Y_true, Y_predict) {
    UseMethod("confusionMatrix")
}

#' @export
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
    value <- list(map = map, models = list(),
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
#' own Bayesian classifier. Computes p(x_ij|k), p(x_ij|w)
#' where k = TRUE in k-block.
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
    for (label in object$map$classes){

        # p(X in kblock)
        ksum <- sum(blocks[y==label,])
        tsum <- sum(X[y==label,])

        if (tsum == 0){
            logprobk <- log(LOGMIN)
            logprobw <- log(LOGMIN)
        }
        else if (ksum == 0){
            logprobk <- log(LOGMIN)
            logprobw <- log(LOGMAX)
        }
        else if (ksum == tsum){
            logprobk <- log(LOGMAX)
            logprobw <- log(LOGMIN)
        }
        else {
            logprobk <- log(ksum) - log(tsum)
            logprobw <- log(tsum - ksum) - log(tsum)
        }
        
        lnprobs[[paste0(label, "|k")]] <- logprobk
        lnprobs[[paste0(label, "|w")]] <- logprobw
    }
    
    return(lnprobs)
}

#' Conditional probability fitting step for Continuous data
#'
#' Only 'gaussian' kernel currently supported. Gaussian kernel
#' requies mean mu given y in addition to the priors for y. The
#' Gaussian kernel standard deviation is specified by hyperparameter
#' lambda and not required for conditional probability computation.
#' Priors for y are computed for the model rather than duplicated for
#' each continuous feature j. 
#'
#' This method should only be used for customizing your own
#' Bayesian classifier.
#'
#' @param object bayesian s3 object with mappings set
#' @param X entire feature matrix
#' @param y target vector, factor
#' @param continuous_cols int vector of column positions
#' @return mean for each column j given a y class label; 'mu|j|y'
#'
#' @export
continuousProbs.bayes <- function(object, X, y, continuous_cols){

    # Retrieve hyperparameters

    kernel <- object$map$kernels['continuous']

    if (kernel != "gaussian") { stop("only 'gaussian' kernel supported") }

    logprobs = list()
    for (label in object$map$classes){
        for (j in object$map$continuous){
            jmu <- mean(X[y==label,j], na.rm=TRUE)
            logprobs[[paste0("mu|",as.character(j),"|",label)]] <- jmu
        }
    }

    return(logprobs)
}

#' Conditional probability fitting step for Categorical data
#'
#' @return "n|j|label"
categoricalProbs.bayes <- function(object, X, y){

    logprobs = list()
    for (label in object$map$classes){
        for (j in object$map$categorical){

            Xtrain <- X[y==label,j] 
            p_jl <- length(Xtrain)

            for (n in levels(Xtrain)){

                p_njl <- sum(Xtrain == n)

                # handle log values correctly

                if (p_njl == p_jl){ # logmax
                    logprob <- log(LOGMAX)
                }
                else if (p_njl == 1){ # logmin
                    logprob <- log(LOGMIN)
                }
                else if (p_njl > 0){ # normal case
                    logprob <- log(p_njl) - log(p_jl)
                }
                else { # class not present, LOGMIN
                    logprob <- log(LOGMIN)
                }

                # update log probs

                name <- paste0(n,"|",j,"|",label)
                logprobs[[name]] <- logprob
            }
        }
    }
    return(logprobs)
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

    cls <- object$map['classes']
    for (label in setdiff(levels(cls), levels(y))){
        y_count <- rbind(c(label, 1))
        y_len <- y_len + 1
    }


    y_count['logprobs'] <- log(LOGMIN)
    for (i in 1:nrow(y_count)){
        if (y_count[i,]$Freq == y_len){
            y_count[i,]$logprobs <- log(LOGMAX)
        }
        else if (y_count[i,]$Freq == 1){
            y_count[i,]$logprobs <- log(LOGMIN)
        }
        else {
            y_count[i,]$logprobs <- log(y_count[i,]$Freq) - log(y_len)
        }
    }

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

    spatial_cols <- object$map$spatial

    logprobs <- list()

    # Compute conditional probs for spatial data

    logprobs[['spatial']] <- spatialProbs(object,
                                          as.matrix(X[,spatial_cols]), y)

    # Compute conditional probs for continuous data

    logprobs[['continuous']] <- continuousProbs(object, X, y)
    
    # Compute conditional probs for categorical data

    logprobs[['categorical']] <- categoricalProbs(object,X, y)

    # Compute priors for continuous & categorical data
    
    #logprobs[['priors']] <- priorProbs(object, y)
    logprobs[['priors']] <- object$map$spatial_priors

    # Cache log prior and conditional probabilities

    object$logpriors <- logprobs

    return(object)
}

#' Fit spatial data
#' 
#' The conditional priors with a maximum likelihood
#' are fit to the model using spatial data.
#'
#' @param object bayes s3 object
#' @param X boolean spatial feature matrix
#' @param Y boolean matrix of same dimensions as X
#'
#' @export
spatial_fit.bayes <- function(object, X, Y){


    # Find y with maximum likelihood for class k
    
    maxsum <- 0
    maxj <- 0
    
    for (j in 1:ncol(Y)){
        y <- as.factor(ifelse(Y[,j] == TRUE, "1", "0"))

        logprobs <- spatialProbs(object, X, y)

        candidates <- c()
        for (label in object$map$classes){
            name <- paste0(label, "|k")
            candidates <- c(candidates, logprobs[[name]])
        }
        csum <- sum(candidates)

        if (csum >= maxsum){
            maxsum <- csum
            maxj <- j
        }
    }

    # Fit model based on max y

    y <- as.factor(ifelse(Y[,maxj] == TRUE, "1", "0"))
    object <- fit(object, X, y)

    return(object)
}

#' Posterior Spatial probabilties
#'
#' Multinomial classification is not currently supported.
#' Assume m[i,j] is TRUE if FALSE, then
#' check to see if K-Block kernel evaluates to TRUE.
#' If K-Block is TRUE, then use conditional probabilities
#' to determine class membership. 
#'
#' @param object s3 bayes object
#' @param X spatial feature vector
#' @return matrix of spatial features
#'
#' @export
postSpatialProbs.bayes <- function(object, X){

    # Retrieve hyperparameters

    kernel <- object$map$kernels['spatial']
    k <- object$map$hyperparameters['kblocks']

    # Compute each x_ij in X

    kblocks <- matrix(NA, nrow=nrow(X), ncol=ncol(X))
    wblocks <- matrix(NA, nrow=nrow(X), ncol=ncol(X))
    resolve <- matrix(0, nrow=nrow(X), ncol=ncol(X))
    name_k0 <- "0|k"
    name_w0 <- "0|w"
    name_k1 <- "1|k"
    name_w1 <- "1|w"

    for (j in 1:ncol(X)){

        for (i in 1:nrow(X)){

            m <- X
            if (m[i,j] == FALSE){

                m[i,j] <- TRUE
                estimate <- kernelDensity(object, x=i, kernel=kernel,
                                          m=m, j=j, k=k)
                
                if (estimate == TRUE){
                    kblocks[i,j] <- object$logpriors$spatial[[name_k1]]
                    wblocks[i,j] <- object$logpriors$spatial[[name_w1]]
                }
                else if (estimate == FALSE){
                    kblocks[i,j] <- object$logpriors$spatial[[name_k0]]
                    wblocks[i,j] <- object$logpriors$spatial[[name_w0]]
                }
                else { stop("bad estimate received") }
            }                
        }
    }
    resolve[!is.na(kblocks)] <- 1
    
    object$models[["spatial"]] <- list(wblocks=wblocks,
                                       kblocks=kblocks,
                                       resolve=resolve)
    return(object)
}

#' Predicts the log probabilities for each class and feature
#'
#' @param object s3 object, bayes
#' @param X feature vector
#' @return bayes s3 object
#'
#' @export
predict_proba.bayes <- function(object, X){

    proba <- list()

    # Just handle spatial for now

    object <- postSpatialProbs(object, X)

    return(object)
}

#' Predict log probabilities for each class K
#'
#' @param object s3 object bayes
#' @param X feature matrix
#' @return s3 object bayes with prediction attribute
#'
#' @export
predict.bayes <- function(object, X){

    # Log probabilties for each feature by class

    object <- predict_proba(object, X) # only spatial data for now
    #probs <- obj$proba

    w <- object$models$spatial$wblocks
    k <- object$models$spatial$kblocks
    resolve <- object$models$spatial$resolve

    if (sum(is.na(w)) > 0) { w[is.na(w)] <- 0 }
    if (sum(is.na(k)) > 0) { k[is.na(k)] <- 0 }

    clf <- ifelse(k > w, TRUE, FALSE)
    clf <- ifelse(k == w, FALSE, TRUE)
    clf[resolve == 0] <- NA

    object$predictions <- clf
    return(object)
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

#' Confusion matrix for reporting accuracy
#'
#' Using an accuracy score when classes are imbalanced
#' can be really deceiving. A confusion matrix can be
#' used as an alternative measure of performance. This
#' confusion matrix expects boolean spatial matrices 
#' for Y. It uses a 'resolve' matrix to exclude ineligible
#' predictions. 
#' 
#' @param object bayes s3 object
#' @param Y_true boolean matrix of correct values
#' @param Y_predict boolean matrix of predicted values
#' @return 2 by 2 confusion matrix
#'
#' @export
confusionMatrix.bayes <- function(object, Y_true, Y_predict) {
    tmp <- Y_true[Y_true == Y_predict]
    resolve <- object$model$spatial$resolve == 0

    correct <- table(tmp[resolve])

    Y_predict[is.na(Y_predict)] <- FALSE
    incorrect <- table(Y_predict[resolve]) - correct

    confm <- matrix(0, nrow=2, ncol=2)
    for (name in names(correct)){
        if (name == TRUE){
            confm[2,2] <- correct[name]
        }
        else {
            confm[1,2] <- correct[name]
        }
    }

    for (name in names(incorrect)){
        if (name == TRUE){
            confm[2,1] <- incorrect[name]
        }
        else {
            confm[1,1] <- incorrect[name]
        }
    }

    return(confm)
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
