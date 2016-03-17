#' Best matches between two sets of HP terms
#' 
#' This function returns the best matches from a semantic similarity matrix.
#' 
#' @param hpSetComp a matrix of semantic similarities between couples of
#' HP terms
#' @param direction taken into account. "r": best match per row.
#' "c": best match per column. "b" (symetric): best match for the whole
#' matrix
#' @author Patrice Godard
#' @return A data frame with the compared term, the best match and the
#' value of the match.
#' @example examples/compHpSet.R
#' @export
#' @seealso \code{\link{compareHPSets}} and \code{\link{hpSetCompSummary}}
#' 
hpSetCompBestMatch <- function(
    hpSetComp,
    direction=c("b", "r", "c")
){
    direction <- match.arg(direction)
    if(is.null(hpSetComp)){
        return(NULL)
    }
    if(nrow(hpSetComp)==0 | ncol(hpSetComp)==0){
        return(NULL)
    }
    ## For the whole matrix
    if(direction=="b"){
        ri <- which.max(hpSetComp)
        rowi <- ri %% nrow(hpSetComp)
        coli <- (ri %/% nrow(hpSetComp)) + 1
        if(rowi==0){
            rowi <- nrow(hpSetComp)
            coli <- coli - 1
        }
        compHp <- rownames(hpSetComp)[rowi]
        candHp <- colnames(hpSetComp)[coli]
        return(data.frame(
            compared=compHp,
            candidate=candHp,
            value=hpSetComp[ri],
            stringsAsFactors=FALSE
        ))
    }
    ## byRow
    if(direction=="r"){
        toRet <- data.frame(
            compared=rownames(hpSetComp),
            do.call(rbind, apply(
                hpSetComp,
                1,
                function(x){
                    return(data.frame(
                        candidate=colnames(hpSetComp)[which.max(x)],
                        value=max(x, na.rm=TRUE),
                        stringsAsFactors=FALSE
                    ))
                }
            )),
            stringsAsFactors=FALSE
        )
        rownames(toRet) <- c()
        return(toRet)
    }
    ## byColum
    if(direction=="c"){
        toRet <- data.frame(
            compared=colnames(hpSetComp),
            do.call(rbind, apply(
                hpSetComp,
                2,
                function(x){
                    return(data.frame(
                        candidate=rownames(hpSetComp)[which.max(x)],
                        value=max(x, na.rm=TRUE),
                        stringsAsFactors=FALSE
                    ))
                }
            )),
            stringsAsFactors=FALSE
        )
        rownames(toRet) <- c()
        return(toRet)
    }
}
