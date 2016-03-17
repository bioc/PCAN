#' @name geneByHp
#' @title Entrez gene IDs associated to HP terms (Example data)
#' @description Each entrez gene IDs is associated to one or several
#' HP terms
#' @docType data
#' @format A data frame with 67989 rows and 2 columns:
#' \describe{
#' \item{entrez}{entrez gene IDs}
#' \item{hp}{HP terms}
#' }
#' @source Two ressources were used in May 27 2015: \itemize{
#' \item{
#' \url{ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/xml/ClinVarFullRelease_2015-05.xml.gz}
#' was used to find genes associated to each OMIM disease with a
#' "Pathogenic" clinical status and one of the follwing
#' origins: "germline", "de novo", "inherited", "maternal",
#' "paternal", "biparental", "uniparental".}
#' \item{
#' \url{http://compbio.charite.de/hudson/job/hpo.annotations/1039/artifact/misc/phenotype_annotation.tab}
#' } was used to find HP associated to each OMIM disease.
#' }
#' @details These data are used to examplify the different functions of
#' the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/ic.R
NULL

#' @name hp_descendants
#' @title HP descendants (Example data)
#' @description HP terms which are descendants of each HP term
#' (including itself) in the Human Phenotype Ontology
#' (\url{http://www.human-phenotype-ontology.org/}).
#' Only descendants of 'Phenotypic abnormality' were taken into account.
#' @docType data
#' @format A named list of 10962 character vectors.
#' @source
#' \url{http://compbio.charite.de/hudson/job/hpo/1529/artifact/hp/hp.obo}
#' @details These data are used to examplify the different functions of
#' the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/ic.R
NULL

#' @name hp_ancestors
#' @title HP ancestors (Example data)
#' @description HP terms which are ancestors of each HP term
#' (including itself) in the Human Phenotype Ontology
#' (\url{http://www.human-phenotype-ontology.org/}).
#' Only descendants of 'Phenotypic abnormality' were taken into account.
#' @docType data
#' @format A named list of 10962 character vectors.
#' @source
#' \url{http://compbio.charite.de/hudson/job/hpo/1529/artifact/hp/hp.obo}
#' @details These data are used to examplify the different functions of
#' the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/hpsim.R
NULL

#' @name hpDef
#' @title Description of HP terms (Example data)
#' @description HP terms basic information. Only descendants of
#' 'Phenotypic abnormality' were taken into account.
#' @docType data
#' @format A data frame with 10962 rows and 2 columns:
#' \describe{
#' \item{id}{HP term IDs}
#' \item{name}{HP term names}
#' }
#' @source
#' \url{http://compbio.charite.de/hudson/job/hpo/1529/artifact/hp/hp.obo}
#' @details These data are used to examplify the different functions of
#' the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/hpsim.R
NULL

#' @name traitDef
#' @title Description of Traits (Example data)
#' @description Basic information about traits. Only OMIM diseases
#' associated to at least one gene are taken into account.
#' @docType data
#' @format A data frame with 3675 rows and 3 columns:
#' \describe{
#' \item{db}{Always "OMIM" here.}
#' \item{id}{The trait ID (OMIM IDs here).}
#' \item{name}{The name of the trait.}
#' }
#' @source
#' \url{ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/xml/ClinVarFullRelease_2015-05.xml.gz}
#' @details These data are used to examplify the different functions
#' of the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/compHpSet.R
NULL

#' @name geneDef
#' @title Description of genes (Example data)
#' @description Basic information about genes Only genes associated
#' to at least one OMIM disease are taken into account.
#' @docType data
#' @format A data frame with 3265 rows and 3 columns:
#' \describe{
#' \item{entrez}{Entrez gene ID.}
#' \item{name}{Gene name.}
#' \item{symbol}{Gene symbol.}
#' }
#' @source
#' \url{ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/xml/ClinVarFullRelease_2015-05.xml.gz}
#' @details These data are used to examplify the different functions
#' of the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/compHpSet.R
NULL

#' @name hpByTrait
#' @title HP IDs associated to trait (Example data)
#' @description Each trait is associated to one or several HP terms.
#' @docType data
#' @format A data frame with 55311 rows and 3 columns:
#' \describe{
#' \item{hp}{HP terms.}
#' \item{db}{Trait database: always "OMIM" here.}
#' \item{id}{Trait ID: OMI IDs here}
#' }
#' @source
#' \url{http://compbio.charite.de/hudson/job/hpo.annotations/1039/artifact/misc/phenotype_annotation.tab}
#' @details These data are used to examplify the different functions of
#' the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @example examples/compHpSet.R
NULL

#' @name geneByTrait
#' @title Gene associated to trait (Example data)
#' @description Each trait is associated to one or several genes.
#' Only genes associated to OMIM disease with a "Pathogenic"
#' clinical status and one of the follwing origins:
#' "germline", "de novo", "inherited", "maternal", "paternal",
#' "biparental", "uniparental".
#' @docType data
#' @format A data frame with 4569 rows and 3 columns:
#' \describe{
#' \item{entrez}{Entrez gene IDs.}
#' \item{db}{Trait database: always "OMIM" here.}
#' \item{id}{Trait ID: OMI IDs here}
#' }
#' @source
#' \url{ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/xml/ClinVarFullRelease_2015-05.xml.gz}
#' @details These data are used to examplify the different functions of
#' the package.
#' More data are available in the MultiHumanPhenoDB package.
#' @examples
#' data(geneByTrait, traitDef, geneDef, package="PCAN")
#' omim <- "612285"
#' traitDef[which(traitDef$id==omim),]
#' # Gene associated to this disease
#' entrez <- geneByTrait[which(geneByTrait$id==omim), "entrez"]
#' geneDef[which(geneDef$entrez %in% entrez),]
#' # All diseases associated to this gene
#' traitDef[
#'      which(
#'          traitDef$id %in%
#'          geneByTrait[which(geneByTrait$entrez==entrez), "id"]
#'      ),
#' ]
NULL

#' @name hp_class
#' @title HP class (Example data)
#' @description Each HP term can be of one or several classes.
#' Classes are HP terms direct descendants of the 'Phenotypic abnormality'
#' term.
#' @docType data
#' @format A named list of 10962 character vectors.
#' @source
#' \url{http://compbio.charite.de/hudson/job/hpo/1529/artifact/hp/hp.obo}
#' @details These data are used to examplify the different functions of the
#' package.
#' More data are available in the MultiHumanPhenoDB package.
#' @examples
#' data(hpDef, hp_class, package="PCAN")
#' hp <- "HP:0100089"
#' hpDef[which(hpDef$id==hp),]
#' # This term has 2 classes:
#' hpDef[which(hpDef$id %in% hp_class[[hp]]),] 
NULL

#' @name rPath
#' @title Reactome pathways (Example data)
#' @description Pathways taken from the Reactome database.
#' @docType data
#' @format A data frame with 1345 rows and 2 columns:
#' \describe{
#' \item{Pathway}{Reactome ID.}
#' \item{Pathway_name}{The name of the pathway.}
#' }
#' @source \url{http://www.reactome.org/download/current/UniProt2Reactome.txt}
#' @examples
#' \dontrun{example(hpGeneListComp)}
NULL

#' @name hsEntrezByRPath
#' @title Homo sapiens entrez gene ID by Reactome pathway (Example data)
#' @description The human genes coding for proteins involved in
#' the different Reactome pathways.
#' @docType data
#' @format A named list of 1345 character vectors.
#' @source Two ressources were used in June 2 2015: \itemize{
#' \item{\url{http://www.reactome.org/download/current/UniProt2Reactome.txt}
#' was used to get list of Uniprot ID associated to each pathway.}
#' \item{
#' \url{ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz}
#' was used to map Uniprot ID to Entrez gene IDs}
#' }
#' @examples
#' \dontrun{example(hpGeneListComp)}
NULL

#' @name hqStrNw
#' @title STRIND database network of Homo sapiens genes (Example data)
#' @description A network of human entrez gene IDs taken from
#' the STRING database.
#' @docType data
#' @format A data frame of 643683 and 3 columns:
#' \describe{
#' \item{gene1}{Entrez gene IDs.}
#' \item{gene2}{Entrez gene IDs.}
#' \item{upstream}{TRUE if the directionality of the interaction
#' between the 2 genes is known. In this case gene1 is upstream gene 2.}
#' }
#' @source Different ressources were used in June 2 2015: \itemize{
#' \item{
#' \url{http://string-db.org/newstring_download/protein.actions.v10/9606.protein.actions.v10.txt.gz}
#' was used to get the network of Ensembl protein IDs.
#' Only interaction with a score greater or equal to 500 were kept.}
#' \item{BioMart from \url{http://jan2013.archive.ensembl.org/index.html}
#' was used to map Ensembl protein IDs to Ensembl gene IDs.
#' Ensembl gene IDs were mapped to Entrez gene IDs using this
#' ressource in addition to
#' \url{ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz}}
#' }
#' @examples
#' \dontrun{example(hpGeneListComp)}
NULL

