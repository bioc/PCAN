rm(list=ls())
gc()

library(XML)
library(parallel)

#############################
load("curOptions.rda")

#############################
rdFile <- file.path(
  tmpDir,
  paste(
    "en_product1-",
    Sys.Date(),
    ".xml",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="http://www.orphadata.org/data/xml/en_product1.xml",
    destfile=rdFile
  )
}

#############################
onRaw <- readLines(rdFile)
encoding <- sub(
  "\".*$",
  "",
  sub("^[<][?]xml .*encoding=\"", "", onRaw[1])
)
starts <- grep("<Disorder[ >]", onRaw)
ends <- grep("<[/]Disorder>", onRaw)
onList <- apply(cbind(starts, ends), 1, function(x) paste(onRaw[x[1]:x[2]], collapse="\n"))

#############################
getDisorderInfo <- function(node){
  ## Basic info
  id <- xmlValue(node[["OrphaNumber"]])
  name <- xmlValue(node[["Name"]])
  disType <- xmlValue(node[["DisorderType"]][["Name"]])
  disTypeId <- xmlValue(node[["DisorderType"]][["OrphaNumber"]])
  basInfo <- data.frame(
    id=id, name=name,
    type=disType,
    type.id=disTypeId,
    stringsAsFactors=F
  )
  ## Synonyms
  synList <- node[["SynonymList"]]
  if(xmlAttrs(synList)["count"]==0){
    synonyms <- c()
  }else{
    synonyms <- unlist(xmlApply(
      synList,
      xmlValue
    ))
  }
  names(synonyms) <- c()
  synonyms <- c(name, synonyms)
  synonyms <- data.frame(
    id=rep(id, length(synonyms)),
    synonym=synonyms,
    stringsAsFactors=F
  )
  ## External refernces
  extRefList <- node[["ExternalReferenceList"]]
  if(xmlAttrs(extRefList)["count"]==0){
    extRef <- c()
  }else{
    extRef <- do.call(rbind, xmlApply(
      extRefList,
      function(extRef){
        refSource <- xmlValue(extRef[["Source"]])
        refId <- xmlValue(extRef[["Reference"]])
        return(c(refSource, refId))
      }
    ))
    colnames(extRef) <- c("Source", "reference.id")
    rownames(extRef) <- c()
    extRef <- data.frame(
      id=rep(id, nrow(extRef)),
      extRef,
      stringsAsFactors=F
    )
  }
  ## Output
  toRet <- list(
    basicInfo=basInfo,
    synonyms=synonyms,
    extRef=extRef
  )
  return(toRet)
}

#############################
onParsed <- mclapply(
  onList,
  function(x) getDisorderInfo(xmlRoot(xmlParse(x, encoding=encoding))),
  mc.cores=mc.cores
)
on.disorders <- do.call(rbind, lapply(
  onParsed,
  function(x) x$basicInfo
))
on.synonyms <- do.call(rbind, lapply(
  onParsed,
  function(x) x$synonyms
))
on.crossReferences <- do.call(rbind, lapply(
  onParsed,
  function(x) x$extRef
))

############################
toSave <- grep("^on[.]", ls(), value=T)
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

