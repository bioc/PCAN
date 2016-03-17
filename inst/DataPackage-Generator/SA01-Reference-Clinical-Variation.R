rm(list=ls())
gc()

library(XML)
library(parallel)

source("clinVar-Functions.R")

#############################
load("curOptions.rda")

#############################
message("Loading XML...")
message(Sys.time())
##
xmlFile <- file.path(
  tmpDir,
  paste(
    "ClinVarFullRelease_00-latest-",
    Sys.Date(),
    ".xml.gz",
    sep=""
  )
)
if(!file.exists(xmlFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/xml/ClinVarFullRelease_00-latest.xml.gz",
    destfile=xmlFile
  )
}

cvList <- readClinVar(xmlFile) #, n=400000) # total: 37210660
encoding <- attr(cvList, "encoding")

message(Sys.time())
message("... Done\n")

#############################
# x <- cvList[[36816]] # 18, 3
# node <- xmlRoot(xmlParse(x, encoding=encoding))
# # node <- node[["ReferenceClinVarAssertion"]]

message("Parsing XML...")
message(Sys.time())
cvDbList <- mclapply(
  cvList,
  function(x){
    parseCvs(xmlRoot(xmlParse(x, encoding=encoding)))
  },
  mc.cores=mc.cores
)
message(Sys.time())
message("... Done\n")

#############################
message("Generating tables...")
message(Sys.time())
toMerge <- unique(unlist(lapply(cvDbList, names)))
for(tm in toMerge){
  message(tm)
  assign(
    x=paste("cv.", tm, sep=""),
    value=lrbind(cvDbList, tm)
  )
  message(Sys.time())
}
message(Sys.time())
message("... Done\n")

#############################
message("Organizing tables...")
message(Sys.time())
##
cv.ReferenceClinVarAssertion <- merge(
  cv.ReferenceClinVarAssertion,
  cv.ClinVarSet,
  by.x="cvs",
  by.y="id",
  all=T
)
rm(cv.ClinVarSet)
##
cv.rcvaInhMode <- cv.rcvaAttributes[
  which(cv.rcvaAttributes$Type=="ModeOfInheritance"),
  c("rcva.id", "value")
]
cv.rcvaInhMode$value <- toupper(cv.rcvaInhMode$value)
cv.rcvaInhMode <- unique(cv.rcvaInhMode)
colnames(cv.rcvaInhMode) <- c("rcvaId", "inhMode")
otherAttTypes <- unique(setdiff(cv.rcvaAttributes$Type, "ModeOfInheritance"))
if(length(otherAttTypes)>0){
  warning("Other RCVA attributes: ", paste(otherAttTypes, collapse=", "))
}
rm(cv.rcvaAttributes)
##
cv.rcvaObservedIn <- cv.observedIn
cv.rcvaObservedIn <- cv.rcvaObservedIn[,c(
  "rcvaId", setdiff(colnames(cv.rcvaObservedIn), "rcvaId")
)]
rm(cv.observedIn)
##
# cv.ClinVarAssertions unchanged
##
##
cv.cvaSubmitters <- cv.submitters[,c("cvaId", "submitter", "primary")]
rm(cv.submitters)
##
cv.cvaObservedIn <- cv.cvaObservedIn[,c(
  "cvaId", setdiff(colnames(cv.cvaObservedIn), "cvaId")
)]
##
cv.rcvaTraits <- cv.traits[,c("rcvaId", "id", "type")]
colnames(cv.rcvaTraits) <- c("rcvaId", "t.id", "traitType")
rm(cv.traits)
##
cv.traits <- cv.traitNames[
  which(cv.traitNames$trait.name.type=="Preferred"),
  c("trait.id", "trait.name")
]
toAdd <- unique(setdiff(cv.traitNames$trait.id, cv.traits$trait.id))
if(length(toAdd) > 0){
  toAdd <- cv.traitNames[
    which(
      cv.traitNames$trait.id %in% toAdd &
        !duplicated(cv.traitNames$trait.id)
    ),
    c("trait.id", "trait.name")
  ]
  cv.traits <- rbind(cv.traits, toAdd)
}
colnames(cv.traits) <- c("id", "name")
##
cv.traitNames <- cv.traitNames[, c(
  "trait.id", "trait.name", "trait.name.type"
)]
colnames(cv.traitNames) <- c("t.id", "name", "type")
##
cv.traitCref <- cv.traitXRef[,c("trait.id", "ID", "DB", "Type")]
colnames(cv.traitCref) <- c("t.id", "id", "db", "type")
rm(cv.traitXRef)
##
cv.variants <- unique(cv.measures[,c("id", "type")])
cv.rcvaVariant <- cv.measures[,c("id", "rcvaId")]
colnames(cv.rcvaVariant) <- c("varId", "rcvaId")
rm(cv.measures)
##
cv.varNames <- cv.measureNames[,c("measureId", "name", "type")]
colnames(cv.varNames) <- c("varId", "name", "type")
rm(cv.measureNames)
cv.variants <- merge(
  cv.variants,
  cv.varNames,
  by.x="id", by.y="varId",
  all=T
)
cv.variants <- cv.variants[order(cv.variants$type.y, decreasing=T),]
cv.variants <- cv.variants[
  which(!duplicated(cv.variants$id)),
  c("id", "type.x", "name")
]
colnames(cv.variants) <- c("id", "type", "name")
##
cv.varEntrez <- unique(cv.measureRelationships[
  which(
    cv.measureRelationships$type=="variant in gene" &
      cv.measureRelationships$xref.db=="Gene"
  ),
  c("measureId", "xref.id")
])
colnames(cv.varEntrez) <- c("varId", "entrez")
##
cv.entrezNames <- unique(cv.measureRelationships[
  which(cv.measureRelationships$xref.db=="Gene"),
  c("xref.id", "name", "symbol", "type")
])
colnames(cv.entrezNames) <- c("entrez", "name", "symbol", "type")
otherRSTypes <- unique(setdiff(cv.measureRelationships$type, "variant in gene"))
if(length(otherRSTypes)>0){
  warning("Other measureRelationShips types: ", paste(otherRSTypes, collapse=", "))
}
rm(cv.measureRelationships)
##
cv.varCytoLoc <- cv.measureCytogeneticLocations[,c("measureId", "cytogenicLocation")]
colnames(cv.varCytoLoc) <- c("varId", "location")
rm(cv.measureCytogeneticLocations)
##
cv.varSeqLoc <- cv.measureSequenceLocations[,c(
  "measureId", setdiff(colnames(cv.measureSequenceLocations), "measureId")
)]
colnames(cv.varSeqLoc) <- c(
  "varId", "Accession", "alternateAllele", "Assembly",
  "AssemblyAccessionVersion", "AssemblyStatus", "Chr",
  "display_start", "display_stop", "innerStart", "innerStop",
  "outerStart", "outerStop", "referenceAllele", "start", "stop",
  "Strand", "variantLength"
)
rm(cv.measureSequenceLocations)
##
cv.varXRef <- cv.measureXRef[,c("measureId", "ID", "DB", "Type")]
colnames(cv.varXRef) <- c("varId", "id", "db", "type")
rm(cv.measureXRef)
##
cv.varAttributes <- cv.measureAttributes[
  ,
  c("measureId", setdiff(colnames(cv.measureAttributes), "measureId"))
]
colnames(cv.varAttributes) <- c("varId", "Type", "integerValue", "Change", "value")
rm(cv.measureAttributes)
##
message(Sys.time())
message("... Done\n")

#############################
message("Saving tables...")
message(Sys.time())
toSave <- grep("^cv[.]", ls(), value=T)
for(f in toSave){
  message(paste("Saving", f))
  ## Ensure unicity
  assign(f, get(f))
  if(length(names(f))==0){
      f <- unique(f)
  }
  ##
  save(
    list=f,
    file=file.path(tmpDir, paste(f, ".rda", sep=""))
  )
}
message(Sys.time())
message("... Done\n")
