#' HP to Gene heatmap
#' 
#' This function draw a heatmap corresponding to the result
#' of the pathway consensus method. For each gene of the pathway
#' under focus and each HP of interest it shows the best score.
#' 
#' @param hpGeneListRes the result of the \code{\link{hpGeneListComp}}
#' function.
#' @param genesOfInterest a list of gene to highlight.
#' @param geneLabels a named vector of gene labels (all the genes id found
#' in hpGeneListRes should be in names(geneLabels)).
#' @param hpLabels a named vector of HP labels (all the HP id found in
#' hpGeneListRes should be in names(hpLabels)).
#' @param clustByGene should the heatmap be clustered according to genes
#' (default: TRUE).
#' @param clustByHp should the heatmap be clustered according to HP
#' (default: TRUE).
#' @param palFun the palette function for the heatmap.
#' @param goiCol the color used to highlight genes of interest.
#' @param ... parameters for the code{\link{heatmap}} function
#' @return A list of 2 matrix (invisible return):\describe{
#' \item{bmValues}{For each gene and each HP of interest the best match
#' value.}
#' \item{bestMatches}{The gene associated HP best matching the HP of
#' interest.}
#' }
#' @example examples/pathwayConsensus.R
#' @seealso \code{\link{hpGeneListComp}}, \code{\link{hpSetCompBestMatch}}
#' @importFrom grDevices colorRampPalette
#' @import stats
#' @export
#' 
hpGeneHeatmap <- function(
    hpGeneListRes,
    genesOfInterest=NULL,
    geneLabels=NULL,
    hpLabels=NULL,
    clustByGene=TRUE,
    clustByHp=TRUE,
    palFun=colorRampPalette(c("white", "red")),
    goiCol="blue",
    ...
){
    ## Prepare the dataset
    matToPlot <- do.call(cbind, lapply(
        hpGeneListRes$best.matches,
        function(x){
            rownames(x) <- x$compared
            x[hpGeneListRes$hpoi,"value"]
        }
    ))
    matToPlot.anno <- do.call(cbind, lapply(
        hpGeneListRes$best.matches,
        function(x){
            rownames(x) <- x$compared
            x[hpGeneListRes$hpoi,"candidate"]
        }
    ))
    rownames(matToPlot) <- rownames(matToPlot.anno) <- hpGeneListRes$hpoi
    ## Matrix ordering
    rorder <- order(apply(matToPlot,1,mean, na.rm=TRUE))
    corder <- order(apply(matToPlot,2,mean, na.rm=TRUE))
    matToPlot <- matToPlot[rorder, corder, drop=FALSE]
    matToPlot.anno <- matToPlot.anno[rorder, corder, drop=FALSE]
    ## Clustering
    rClust <- cClust <- NA
    if(ncol(matToPlot) > 1 & nrow(matToPlot) > 1){
        if(clustByGene){
            cClust <- hclust(dist(t(matToPlot)))
            cClust <- as.dendrogram(cClust)
        }
        if(clustByHp){
            rClust <- hclust(dist(matToPlot))
            rClust <- as.dendrogram(rClust)
        }
        ## Labels
        colLabs <- colnames(matToPlot)
        rowLabs <- rownames(matToPlot)
        if(!is.null(geneLabels)){
            colLabs <- geneLabels[colLabs]
        }
        if(!is.null(hpLabels)){
            rowLabs <- hpLabels[rowLabs]
        }
    }else{
        if(ncol(matToPlot) <= 1){
            oricnames <- colnames(matToPlot)
            matToPlot <- cbind(
                rep(min(matToPlot[,1]), nrow(matToPlot)),
                matToPlot
            )
            colnames(matToPlot) <- c("", oricnames)
            if(!is.null(geneLabels)){
                colLabs <- c("", geneLabels[oricnames])
            }else{
                colLabs <- colnames(matToPlot)
            }
        }else{
            colLabs <- colnames(matToPlot)
            if(!is.null(geneLabels)){
                colLabs <- geneLabels[colLabs]
            }
        }
        if(nrow(matToPlot) == 1){
            orirnames <- rownames(matToPlot)
            matToPlot <- rbind(
                rep(min(matToPlot[1,]), ncol(matToPlot)),
                matToPlot
            )
            rownames(matToPlot) <- c("", orirnames)
            if(!is.null(hpLabels)){
                rowLabs <- c("", hpLabels[orirnames])
            }else{
                rowLabs <- rownames(matToPlot)
            }
        }else{
            rowLabs <- rownames(matToPlot)
            if(!is.null(hpLabels)){
                rowLabs <- hpLabels[rowLabs]
            }
        }
    }
    
    ## Plot
    heatmap(
        x=matToPlot,
        Rowv=rClust,
        Colv=cClust,
        scale="none",
        col=palFun(100),
        labRow=rowLabs,
        labCol=colLabs,
        ColSideColors=ifelse(
            colnames(matToPlot) %in% genesOfInterest,
            goiCol,
            "transparent"
        ),
        ...
    )
    
    ## Quiet return
    invisible(list(
        bmValues=matToPlot,
        bestMatches=matToPlot.anno
    ))
}
