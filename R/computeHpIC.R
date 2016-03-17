#' Compute Information Content (IC) for each HP based on genes by HP 
#' 
#' @param content a list providing the content associated to each HP
#' @param hp.descendants a list providing for each HP all its descendant
#' HP terms
#' @return a vector of IC named with HP terms
#' 
#' @details This function assumes that all the HP terms taken into account
#' belong to the same family of terms(i.e they are all descendants of the
#' same term).
#' @example examples/ic.R
#' @export
#' 
computeHpIC <- function(content, hp.descendants){
    ##
    geneByHpDesc <- lapply(
        hp.descendants,
        function(hps){
            unique(unlist(content[hps]))
        }
    )
    gbhLength <- unlist(lapply(geneByHpDesc, length))
    genesNb <- length(unique(unlist(content)))
    prop <- gbhLength/genesNb
    prop[which(prop==0)] <- NA
    ic <- -log(prop)
    return(ic)
}
