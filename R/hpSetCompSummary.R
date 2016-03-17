#' Global semantic similarity between 2 HP sets
#' 
#' This function summarize the comparison of 2 sets of HP terms
#' 
#' @param hpSetComp a matrix of semantic similarities between couples
#' of HP terms
#' @param method "bma" (Best Match Average): the average of the best
#' matches on rows or columns (see direction param).
#' "bm": the maximum value. "average": the average of the whole matrix.
#' @param direction taken into account only if method="bma".
#' "r": best match per row. "c": best match per column.
#' "symSim" (symmetric semantic similarity): average of calls with "r" and "c"
#' @author Patrice Godard
#' @return A numeric value corresponding to the semantic similarity of
#' the 2 HP sets
#' @example examples/compHpSet.R
#' @export
#' @seealso \code{\link{compareHPSets}}
#' 
hpSetCompSummary <- function(
    hpSetComp,
    method=c("bma", "bm", "average"),
    direction=c("symSim", "r", "c")
){
    method=match.arg(method)
    direction=match.arg(direction)
    if(is.null(hpSetComp)){
        return(NA)
    }
    if(nrow(hpSetComp)==0 | ncol(hpSetComp)==0){
        return(NA)
    }
    if(method=="average"){
        return(mean(hpSetComp, na.rm=TRUE))
    }
    if(method=="bm"){
        return(max(hpSetComp, na.rm=TRUE))
    }
    if(method=="bma"){
        if(direction=="r"){
            return(mean(apply(hpSetComp, 1, max, na.rm=TRUE)))
        }
        if(direction=="c"){
            return(mean(apply(hpSetComp, 2, max, na.rm=TRUE)))
        }
        if(direction=="symSim"){
            return(mean(c(
                hpSetCompSummary(hpSetComp, direction="r"),
                hpSetCompSummary(hpSetComp, direction="c")
            ), na.rm=TRUE))
        }
    }
}
