#' HP semantic similarity for a whole gene list.
#' 
#' This function compare a whole gene list to a set of HP terms using
#' a matrix of semantic similarity.
#' 
#' @param geneList a vector providing the genes of interest.
#' @param ssMatByGene a list (one element per gene) of  matrix
#' of semantic similarity between HP terms as returned by
#' \code{\link{compareHPSets}}. This list has to be unbiased in
#' order to compute p-values.
#' @param geneSSScore a vector of semantic similarity scores for
#' all the genes in ssMatByGene list. If not provided these scores
#' are computed from ssMatByGene.
#' @param ... parameters for \code{\link{hpSetCompSummary}} if
#' geneSSScore is not provided.
#' @author Patrice Godard
#' @return A list with the following elements:\describe{
#' \item{hpoi}{The original HP of interest.}
#' \item{allScoreDist}{The distribution of scores for all genes
#' for the HP of interest.}
#' \item{scores}{The semantic similarity by gene.}
#' \item{best.matches}{For each gene which related HP terms best
#' fits with the HP of interest (colnames of the elements of ssMatByGene).}
#' \item{median}{The median of scores.}
#' \item{p.value}{According to a \code{\link{wilcox.test}} comparing genes
#' of interest to all the other genes.}
#' \item{best.gene}{Gene with the highest score among the genes of
#' interest.}
#' \item{max}{Maximum score.}
#' \item{score.quantiles}{Quantile of the scores compared to the
#' whole list of gene.}
#' \item{adj.quant}{Adjusted quantiles according Benjamini Hochberg
#' (\code{link{p.adjust}}).}
#' }
#' @import stats
#' @export
#' @example examples/pathwayConsensus.R
#' @seealso \code{\link{hpGeneHeatmap}}, \code{\link{compareHPSets}},
#' \code{\link{hpSetCompSummary}} and \code{\link{hpSetCompBestMatch}}
#' 

hpGeneListComp <- function(
    geneList,
    ssMatByGene,
    geneSSScore=NULL,
    ...
){
    if(is.null(geneSSScore)){
        geneSSScore <- unlist(lapply(ssMatByGene, hpSetCompSummary, ...))
    }
    
    scoresToRet <- geneSSScore[geneList]
    if(all(is.na(scoresToRet))){
        return(list(
            "hpoi"=colnames(ssMatByGene[[1]]),
            "allScoreDist"=quantile(geneSSScore, na.rm=TRUE),
            "scores"=NULL,
            "best.matches"=NULL,
            "median"=NA,
            "p.value"=NA,
            "best.gene"=NA,
            "max"=NA,
            "score.quantiles"=NA,
            "adj.quant"=NA
        ))
    }
    names(scoresToRet) <- geneList
    scoreMax <- max(scoresToRet, na.rm=TRUE)
    scoreQuant <- unlist(lapply(
        scoresToRet,
        function(x){
            sum(geneSSScore >= x)/length(geneSSScore)
        }
    ))
    names(scoreQuant) <- names(scoresToRet)
    adjQuant <- p.adjust(scoreQuant, method="BH")
    names(adjQuant) <- names(scoreQuant)
    bestMatches <- lapply(
        ssMatByGene[geneList],
        hpSetCompBestMatch,
        direction="c"
    )
    names(bestMatches) <- geneList
    geneOrder <- order(scoresToRet, decreasing=TRUE)
    return(list(
        "hpoi"=colnames(ssMatByGene[[1]]),
        "allScoreDist"=quantile(geneSSScore, na.rm=TRUE),
        "scores"=scoresToRet[geneOrder],
        "best.matches"=bestMatches[geneOrder],
        "median"=median(scoresToRet, na.rm=TRUE),
        "p.value"=wilcox.test(
            geneSSScore[setdiff(names(geneSSScore), geneList)],
            scoresToRet,
            alternative = "less"
        )$p.value,
        "best.gene"=names(scoresToRet)[which.max(scoresToRet)],
        "max"=scoreMax,
        "score.quantiles"=scoreQuant[geneOrder],
        "adj.quant"=adjQuant[geneOrder]
    ))
    
}
