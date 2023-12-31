% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareHPSets.R
\name{compareHPSets}
\alias{compareHPSets}
\title{Compare 2 sets of HP terms based on semantic similarity}
\usage{
compareHPSets(hpSet1, hpSet2, IC, ancestors, method = "Resnik",
  BPPARAM = bpparam())
}
\arguments{
\item{hpSet1}{a set of HP terms}

\item{hpSet2}{another set of HP terms}

\item{IC}{a named vector of Information Content by HP term}

\item{ancestors}{a named list of ancestors by HP term}

\item{method}{the method for computing semantic simalirity among
those available in \code{\link{calcHpSim}} (default: "Resnik" returns
the IC of the MICA: Most Informative common ancestor)}

\item{BPPARAM}{An optional \code{\link{BiocParallelParam}} instance defining
the parallel back-end to be used during evaluation
(used internally by the \code{\link{bpmapply}} function).}
}
\value{
A matrix of semantic similarity
}
\description{
This function compares each couple of HP terms from each of the 2
provided sets
based on Information Content (IC)
}
\examples{
## Prerequisite
data(geneByHp, hp_descendants, package="PCAN")
geneByHp <- unstack(geneByHp, entrez~hp)
ic <- computeHpIC(geneByHp, hp_descendants)

###########################################
## Use case: comparing a gene and a disease
data(traitDef, geneDef, hp_ancestors, hpDef, package="PCAN")
omim <- "612285"
traitDef[which(traitDef$id==omim),]
entrez <- "57545"
geneDef[which(geneDef$entrez==entrez),]
## Get HP terms associated to the disease
data(hpByTrait, package="PCAN")
hpOfInterest <- hpByTrait$hp[which(hpByTrait$id==omim)]

## Get HP terms associated to the gene
hpByGene <- unstack(stack(geneByHp), ind~values)
geneHps <- hpByGene[[entrez]]
## Comparison of the two sets of HP terms
compMat <- compareHPSets(
    hpSet1=geneHps, hpSet2=hpOfInterest,
    IC=ic,
    ancestors=hp_ancestors,
    method="Resnik",
    BPPARAM=SerialParam()
)
## Get the symmetric semantic similarity score
hpSetCompSummary(compMat, method="bma", direction="symSim")
bm <- hpSetCompBestMatch(compMat, "b")
hpDef[match(c(bm$compared, bm$candidate), hpDef$id),]
## Assessing the significance of this score by comparing to all other genes
hpGeneResnik <- compareHPSets(
    hpSet1=names(ic), hpSet2=hpOfInterest,
    IC=ic,
    ancestors=hp_ancestors,
    method="Resnik",
    BPPARAM=SerialParam()
)
hpMatByGene <- lapply(
    hpByGene,
    function(x){
        hpGeneResnik[x, , drop=FALSE]
    }
)
resnSss <- unlist(lapply(
    hpMatByGene,
    hpSetCompSummary,
    method="bma", direction="symSim"
))
candScore <- resnSss[entrez]
hist(
    resnSss,
    breaks=100, col="grey",
    ylim=c(0,300),
    xlab=expression(Sim[sym]),
    ylab="Number of genes",
    main=paste(
        "Distribution of symmetric semantic similarity scores\\nfor all the",
        length(resnSss), "genes"
    )
)
polygon(
    x=c(candScore, 10, 10, candScore),
    y=c(-10, -10, 1000, 1000),
    border="#BE0000",
    col="#BE000080",
    lwd=3
)
withHigher <- sum(resnSss >= candScore)
text(
    x=candScore, y=mean(par()$usr[3:4]),
    paste0(
        withHigher, " genes (",
        signif(withHigher*100/length(resnSss), 2), "\%)\\n",
        "show a symmetric semantic\\n",
        "similarity score greater than\\n",
        "the gene candidate for\\n",
        "for the HPs under focus."
    ),
    pos=4,
    cex=0.6
)
}
\author{
Patrice Godard
}
\seealso{
\code{\link{calcHpSim}}
}

