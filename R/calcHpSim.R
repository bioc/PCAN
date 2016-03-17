#' Compare HP terms based on semantic similarity
#' 
#' This function compares 2 HP terms
#' based on provided Information Content and ancestors
#' 
#' @param term1 one of the HP term to compare
#' @param term2 the other HP term to compare
#' @param method the method for computing semantic simalirity
#' (default: "Resnik" returns the IC of the MICA: Most Informative
#' common ancestor)
#' @param IC a named vector of Information Content by HP term
#' @param ancestors a named list of ancestors by HP term
#' @author Patrice Godard
#' @return A numeric value
#' @example examples/hpsim.R
#' @export
#' @seealso \code{\link{compareHPSets}}
#' 
calcHpSim <-function(
    term1, term2,
    method=c("Resnik"),
    IC, ancestors
){
    method <- match.arg(method)
    if(method=="Resnik"){
        caIc <- IC[intersect(ancestors[[term1]], ancestors[[term2]])]
        toRet <- max(c(caIc, 0), na.rm=TRUE)
    }
    return(toRet)
}
