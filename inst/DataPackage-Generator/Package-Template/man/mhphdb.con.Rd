% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/zzz.R
\name{mhphdb.con}
\alias{mhphdb.con}
\title{Connection to the unerlying SQLite DB}
\usage{
mhphdb.con()
}
\value{
An SQLite connection
}
\description{
Open and/or return the connection to the SQLite DB
in which data tables are stored.
}
\examples{
##########################
## Explore the data model

## The shema of all the data in the package
getDataModel()
## Show all the data directly available from the package
data(package="MultiHumanPhenoDB")
cv_clinSigOrder
head(hp_class)
## Show all the tables in the SQLite database
dbListTables(mhphdb.con())
## Show all the field for the traitDef table
dbListFields(mhphdb.con(), "traitDef")
## Get the traitDef table
traitDef <- dbReadTable(mhphdb.con(), "traitDef")
head(traitDef)
## Get genes associated to a disease
disName <- "Abetalipoproteinaemia"
query <- paste(
    "select gbt.entrez, en.name, en.symbol, rcvaId , gbt.db, gbt.id, td.name",
    "from geneByTrait gbt, traitDef td, cv_entrezNames en",
    "where td.name=", paste0("'", disName, "'"),
    "and td.id=gbt.id",
    "and td.db=gbt.db",
    "and gbt.entrez=en.entrez"
)
disGenes <- dbGetQuery(mhphdb.con(), query)
disGenes
}
\seealso{
\code{\link{getDataModel}}
}

