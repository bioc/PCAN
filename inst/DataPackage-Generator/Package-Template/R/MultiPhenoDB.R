#' Filter the geneByTrait data.frame
#' 
#' Get gene/trait association according to the origin and
#' the clinical status of the associated variants.
#' 
#' @param origin the origin of the variants to be taken into account.
#' If NULL this filter is not applied.
#' @param clinicalSignificance the clinical significance of the
#' variants to be taken into account. If NULL this filter is not applied.
#' @param diseaseDb the DB on which to focuse
#' (e.g. "OMIM", "MedGen", "Orphanet", ...)
#' @return An assocation data.frame with the following fields: \describe{
#'  \item{entrez}{the gene Entrez ID.}
#'  \item{db}{the trait DB.}
#'  \item{id}{the trait ID.}
#' }
#' 
#' @example examples/geneAssociations.R
#' 
#' @seealso \code{\link{getHpByGene}}, \code{\link{getGeneByHp}}
#' 
#' @export
#'
filterGeneByTrait <- function(
    origin=c(
        "germline", "de novo", "inherited",
        "maternal", "paternal", 
        "biparental", "uniparental"
    ),
    clinicalSignificance=c("Pathogenic"),
    diseaseDb=c()
){
    
    ####################
    ## Clauses
    csCl <- paste0("('", paste(clinicalSignificance, collapse="', '"), "')")
    origCl <- paste0("('", paste(origin, collapse="', '"), "')")
    dbCl <- paste0("('", paste(diseaseDb, collapse="', '"), "')")
    ####################
    ## Subqueries
    csRcvaQuery <- paste0(
        "select distinct id from cv_ReferenceClinVarAssertion where clinicalSignificance in ",
        csCl
    )
    ##
    orRcva1Query <- paste(
        "select distinct rcva.id",
        "from cv_ClinVarAssertions cva, cv_cvaObservedIn coi,",
        "cv_ReferenceClinVarAssertion rcva",
        "where", 
        "cva.id=coi.cvaId",
        "and rcva.cvs=cva.cvs",
        "and coi.origin in",
        origCl
    )
    orRcva2Query <- paste(
        "select distinct rcva.id",
        "from cv_rcvaObservedIn roi,",
        "cv_ReferenceClinVarAssertion rcva",
        "where", 
        "rcva.id=roi.rcvaId",
        "and roi.origin in",
        origCl
    )
    ####################
    ## Filter query
    toGetCl <- "select distinct entrez, db, id from geneByTrait"
    if(length(origin) > 0){
        if(length(clinicalSignificance) > 0){
            filterQuery <- paste(
                toGetCl, "where",
                "(rcvaId in (", orRcva1Query, ")",
                "or rcvaId in (", orRcva2Query, "))",
                "and rcvaId in (", csRcvaQuery, ")"
            )
        }else{
            filterQuery <- paste(
                toGetCl, "where",
                "(rcvaId in (", orRcva1Query, ")",
                "or rcvaId in (", orRcva2Query, "))"
            )
        }
    }else{
        if(length(clinicalSignificance) > 0){
            filterQuery <- paste(
                toGetCl, "where",
                "rcvaId in (", csRcvaQuery, ")"
            )
        }else{
            filterQuery <- paste(
                toGetCl
            )
        }
    }
    
    ####################
    ## Call filter query
    toRet <- dbGetQuery(
        mhphdb.con(),
        filterQuery
    )
    
    ## Filter on disease DB of interest
    if(length(diseaseDb)>0){
        toRet <- toRet[which(toRet$db %in% diseaseDb),]
    }
    
    ##
    return(toRet)
    
}

#' Get HP by Gene
#' 
#' Get all entrez HP associated to each gene entrez ID.
#' These associations come from gene/trait associations
#' and can be filtered according to origin and clinical signficance.
#' 
#' @param ... parameters for the \code{\link{filterGeneByTrait}} function.
#' @return a list providing for each entrez gene ID a set of HP.
#' 
#' @example examples/geneAssociations.R
#' 
#' @seealso \code{\link{filterGeneByTrait}}, \code{\link{getGeneByHp}}
#' 
#' @export
#'
getHpByGene <- function(...){
    geneByTrait <- filterGeneByTrait(...)
    hpByTrait <- dbReadTable(mhphdb.con(), "hpByTrait")
    hpByGene <- unique(merge(
        geneByTrait,
        hpByTrait,
        by=c("id", "db"),
        all=F
    )[,c("entrez", "hp")])
    toRet <- lapply(
        unstack(hpByGene, hp~entrez),
        unique
    )
}

#' Get gene by HP
#' 
#' Get all entrez gene entrez ID associated to each HP.
#' These associations come from gene/trait associations
#' and can be filtered according to origin and clinical signficance.
#' 
#' @param ... parameters for the \code{\link{filterGeneByTrait}} function.
#' @return a list providing for each entrez gene ID a set of HP.
#' 
#' @example examples/geneAssociations.R
#' 
#' @seealso \code{\link{filterGeneByTrait}}, \code{\link{getHpByGene}}
#' 
#' @export
#'
getGeneByHp <- function(...){
    geneByTrait <- filterGeneByTrait(...)
    hpByTrait <- dbReadTable(mhphdb.con(), "hpByTrait")
    hpByGene <- unique(merge(
        geneByTrait,
        hpByTrait,
        by=c("id", "db"),
        all=F
    )[,c("entrez", "hp")])
    toRet <- lapply(
        unstack(hpByGene, entrez~hp),
        unique
    )
}


#' Get the data model of the package
#' 
#' Plot the shema of the package data model.
#' 
#' @example examples/dataModel.R
#' 
#' @seealso \code{\link{mhphdb.con}}
#' 
#' @export
#' @importFrom png readPNG
#'
getDataModel <- function(){
    pkgname <- packageName()
    pngFile <- system.file(
        "Documentation", "Data.png",
        package=pkgname
    )
    message("The data model is located here: ", pngFile)
    img <- readPNG(pngFile)
    rimg <- as.raster(img) # raster multilayer object
    r <- nrow(rimg) / ncol(rimg) # image ratio
    opar <- par(mar=rep(0,4))
    on.exit(par(opar))
    plot(
        c(0,1), c(0,r),
        type = "n", xaxt="n", yaxt="n",
        xlab = "", ylab = "",
        asp=1
    )
    rasterImage(rimg, 0, 0, 1, r) 
}
