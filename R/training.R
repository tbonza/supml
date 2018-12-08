#' For use with in generating training and test sets


#' Create training, test, and hold out splits
#'
#' @param data data frame object
#' @param train integer, number of training samples
#' @param test integer, number of test samples
#' @return list of training, test and holdout samples
#'
#' @export
create_splits <- function(data, train, test){
    pos <- 1:nrow(data)

    rpos <- sample(pos)
    df <- data[rpos,]

    tr <- df[1:train,]
    te <- df[(train + 1): (test + train),]
    ho <- df[(test + 1):nrow(df),]

    return(list(train=tr, test=te, holdout=ho))
}


