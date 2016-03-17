#' Compare 2 sets of HP terms based on semantic similarity
#' 
#' This function compares each couple of HP terms from each of the 2
#' provided sets
#' based on Information Content (IC)
#' 
#' @param hpSet1 a set of HP terms
#' @param hpSet2 another set of HP terms
#' @param IC a named vector of Information Content by HP term
#' @param ancestors a named list of ancestors by HP term
#' @param method the method for computing semantic simalirity among
#' those available in \code{\link{calcHpSim}} (default: "Resnik" returns
#' the IC of the MICA: Most Informative common ancestor)
#' @param BPPARAM An optional \code{\link{BiocParallelParam}} instance defining
#' the parallel back-end to be used during evaluation
#' (used internally by the \code{\link{bpmapply}} function).
#' @author Patrice Godard
#' @return A matrix of semantic similarity
#' @example examples/compHpSet.R
#' @example examples/compHpSet2.R
#' @export
#' @seealso \code{\link{calcHpSim}}
#' @importFrom BiocParallel bpmapply bpparam

compareHPSets <- function(
    hpSet1,
    hpSet2,
    IC,
    ancestors,
    method="Resnik",
    BPPARAM=bpparam()
){
    toRet <- bpmapply(
        FUN=function(a, b, IC, method, ancestors){
            return(PCAN::calcHpSim(
                term1=a, term2=b, IC=IC, method=method, ancestors=ancestors
            ))
        },
        rep(hpSet1, each=length(hpSet2)),
        hpSet2,
        BPPARAM=BPPARAM,
        MoreArgs = list(IC=IC, method=method, ancestors=ancestors)
    )
    toRet <- matrix(toRet, nrow=length(hpSet1), byrow=TRUE)
    rownames(toRet) <- hpSet1
    colnames(toRet) <- hpSet2
    return(toRet)
}
