#' Utility functions for working with Bayesian classifier


#' Create buckets given a vector x
#'
#' Bayesian classification for a spatial matrix
#' requires that we normalize coordinates.
#'
#' @param x numeric vector
#' @param nbreaks number of buckets for factor
#' @return factor vector 
#'
#' @export  
coord_cut <- function(x, nbreaks=100){
    x_step <- (max(x) - min(x))/ (nbreaks - 2)
    x_breaks <- seq(from=min(x) - x_step, to=max(x) + x_step, by= x_step)
    x_cut <- cut(x, breaks=x_breaks)
    return(x_cut)
}


#' Populate a Spatial Matrix with Aggregated Values
#'
#' Once a square spatial matrix has been created using 
#' normalized coordinates, we must populate it with 
#' values of interest.
#' 
#' @param xf factor vector of x coordinate
#' @param yf factor vector of y coordinate
#' @param z numeric vector of aggregated measure
#' @param m matrix with levels(xf) by levels(yf) dimensions
#' @return m spatial matrix
#'
#' @export
popmat <- function(xf,yf,z, m){
    x <- as.numeric(xf)
    y <- as.numeric(yf)

    for (i in 1:length(z)){
        m[x[i], y[i]] <- m[x[i], y[i]] + z[i]
    }
    return(m)
}
