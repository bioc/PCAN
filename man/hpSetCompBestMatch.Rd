% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpSetCompBestMatch.R
\name{hpSetCompBestMatch}
\alias{hpSetCompBestMatch}
\title{Best matches between two sets of HP terms}
\usage{
hpSetCompBestMatch(hpSetComp, direction = c("b", "r", "c"))
}
\arguments{
\item{hpSetComp}{a matrix of semantic similarities between couples of
HP terms}

\item{direction}{taken into account. "r": best match per row.
"c": best match per column. "b" (symetric): best match for the whole
matrix}
}
\value{
A data frame with the compared term, the best match and the
value of the match.
}
\description{
This function returns the best matches from a semantic similarity matrix.
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
}
\author{
Patrice Godard
}
\seealso{
\code{\link{compareHPSets}} and \code{\link{hpSetCompSummary}}
}

