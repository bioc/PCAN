% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataDoc.R
\docType{data}
\name{geneByTrait}
\alias{geneByTrait}
\title{Gene associated to trait (Example data)}
\format{A data frame with 4569 rows and 3 columns:
\describe{
\item{entrez}{Entrez gene IDs.}
\item{db}{Trait database: always "OMIM" here.}
\item{id}{Trait ID: OMI IDs here}
}}
\source{
\url{ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/xml/ClinVarFullRelease_2015-05.xml.gz}
}
\description{
Each trait is associated to one or several genes.
Only genes associated to OMIM disease with a "Pathogenic"
clinical status and one of the follwing origins:
"germline", "de novo", "inherited", "maternal", "paternal",
"biparental", "uniparental".
}
\details{
These data are used to examplify the different functions of
the package.
More data are available in the MultiHumanPhenoDB package.
}
\examples{
data(geneByTrait, traitDef, geneDef, package="PCAN")
omim <- "612285"
traitDef[which(traitDef$id==omim),]
# Gene associated to this disease
entrez <- geneByTrait[which(geneByTrait$id==omim), "entrez"]
geneDef[which(geneDef$entrez \%in\% entrez),]
# All diseases associated to this gene
traitDef[
     which(
         traitDef$id \%in\%
         geneByTrait[which(geneByTrait$entrez==entrez), "id"]
     ),
]
}

