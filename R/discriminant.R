#' Implementations related to Discriminant analysis


#' Create indices for each class K
#' 
#' @param groups factor levels for all K classes
#' @param Y target vector
#' @return list of group indices within target vector
grp_indices <- function(groups, Y){
    partitions <- function(group, z) { which(z==group) }
    grpidx <- lapply(levels(Y), partitions, z=Y)
    return(grpidx)
}

#' Return a p*p covariance matrix for class K
#' 
#' Note that this matrix is not normalized
grp_covar <- function(indices, predictors){
    p <- ncol(predictors)
    m <- colMeans(predictors[indices,])
    covarm <- matrix(0,p,p)

    for(i in 1:p){
        for (j in 1:p){
            covarm[i,j] <- t(predictors[indices,i] - m[i]) %*%
                (predictors[indices,j] - m[j])
        }
    }
    return(covarm)
}

#' Compute Pooled Covariance Matrix
#' 
#' Equation comes from Pooled Variance
#' https://en.wikipedia.org/wiki/Pooled_variance
#'
#' @param X matrix of predictors
#' @param y target vector
#' @return pooled p by p covariance matrix
#'
#' @export
pooled_covar <- function(X, y){
    
    Y <- factor(y)
    grpidx <- grp_indices(levels(Y), Y)
    covar_mats <- lapply(grpidx,grp_covar,predictors=X)

    pooled_covar <- (1.0/ (length(Y) - nlevels(Y))) *
        Reduce('+', covar_mats)

    return(pooled_covar)
}

#' Compute prior probabilities for each class K
#'
#' See HT, Elements of Statistical Learning (4.10)
pi_k <- function(y){

    Y <- factor(y)

    priors <- function(group, N){ length(group) / N }
   
    grpidx <- grp_indices(levels(Y), Y)

    prior_probs <- lapply(grpidx, priors, N=length(Y))

    return(prior_probs)
}


#' Mean of each predictor attribute for each class K
#' 
#' See HT, Elements of Statistical Learning (4.10)
grp_mean <- function(indices, predictors){

    return(colMeans(predictors[indices,]))
}

# Mean values for Xi for each class K
mu_k <- function(X, y){
    
    Y <- factor(y)
    grpidx <- grp_indices(levels(Y), Y)
    muvals <- lapply(grpidx,grp_mean,predictors=X)

    return(muvals)
}

#' Linear discriminant function (LDF) implementation
#'
#' LDF is computed for k in K classes for LDA. See
#' HT, Elements of Statistical Learning equation (4.10)
#'
ldf <- function(X, y, kpi, kmu, sig){

    s <- (1/2) * t(kmu) %*% sig %*% kmu + log(kpi)
    
    ans <- X %*% sig %*% kmu - s[1,1]

    return(ans)
}

#' Given a matrix, find the max row value position
argmax <- function(rowidx, yhat){
    which(yhat[rowidx,] == max(yhat[rowidx,]))
}

factor_label <- function(yhat_raw, y_levels){
    return(y_levels[yhat_raw])
}

#' Linear Discriminant Analysis implementation
#' 
#' @param X matrix of predictors (no intercept)
#' @param y target vector
#' @return yhat vector
#'
#' @export
linda <- function(X, y){

    Y <- factor(y)
    
    all_mu <- mu_k(X, y)
    all_pi <- pi_k(y)
    sig <- solve(pooled_covar(X, y))

    kclasses <- matrix(0, ncol=nlevels(Y),
                       nrow=nrow(X))

     
    for (k in 1:nlevels(Y)){

        kmu <- all_mu[[k]]
        kpi <- all_pi[[k]]

        kclasses[,k] <- ldf(X,y,kpi, kmu, sig)
    }

    yhat_raw <- sapply(1:nrow(X), argmax, yhat=kclasses)

    yhat <- as.numeric(sapply(yhat_raw, factor_label,
                              y_levels=levels(Y)))
    
    return(list(yhat=yhat))
}
