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

#' Normalize a numeric object
#'
#' @param x numeric vector
#' @return normalized values (0,1)
#'
#' @export
normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

#' Coordinate grid for plotting
#'
#' Matrix must be square
#'
#' @param m number of rows
#' @param n number of columns
#' @param step step size, ex. sqrt(m) / (1.0 / n)
#'
#' @export
plotgrid <- function(m, n, step) {
    
    x <- 0.0
    y <- 0.0
    origin <- 0.0
    groupid <- 1
    
    xcol <- c()
    ycol <- c()
    grp <- c()
    
    for (i in 1:m){
        
        for (j in 1:n){
            
            # make block
            
            if (x <= n * (1.0/m)){
            xcol <- c(xcol, x, x, 
                x + step, x + step)
            ycol <- c(ycol, y,
                     y+ step, y+ step,
                     y)
            grp <- c(grp, groupid, groupid,
                    groupid, groupid)
            }
            # increment to next block          
            if (n * step > y){
                x <- x 
                y <- y + step
                
                if (y + step > n * step){
                    x <- x + step
                    y <- origin
                }
                
            } else {
                x <- x + step
                y <- origin
            }
            
            groupid <- groupid + 1
        }
    }
    df <- as.data.frame(grp)
    df <- cbind(df, xcol, ycol)
    return(df)
}

#' Convert matrix to visualization format
#'
#' This function is meant to be used when
#' creating an overlaying layer when 
#' generating maps in ggplot2. 
#'
#' @param X boolean square matrix
#' @return dataframe containing group
#'       information with factors
#'
#' @export
matrix_group <- function(X){
    grp <- 1
    groups <- c()
    values <- c()
    for (i in 1:nrow(X)){
        for (j in 1:ncol(X)){
        
            if (is.na(X[i,j]) == TRUE){
                values <- c(values, "None")
            }
            else if (X[i,j] == TRUE){
                values <- c(values, "True")
            }
            else {
                values <- c(values, "False")
            }
            
            groups <- c(groups, grp)
            grp <- grp + 1
        }
    }
    df <- as.data.frame(groups)
    df <- cbind(df, values)
    return(df)
}
