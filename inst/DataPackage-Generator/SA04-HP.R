rm(list=ls())
gc()

#############################
load("curOptions.rda")

oboFn <- file.path(tmpDir, paste("hp-", Sys.Date(), ".obo", sep=""))
if(!file.exists(oboFn)){
  download.file(
    # url="http://purl.obolibrary.org/obo/hp.obo",
    url="http://www.berkeleybop.org/ontologies/hp.obo",
    destfile=oboFn
  )
}
obo <- readLines(oboFn)

############################
## Basic information
starts <- which(obo=="[Term]")
ends <- c(starts[-1]-1, length(obo))
hpDef <- do.call(rbind, apply(
   data.frame(starts, ends),
   1,
   function(x){
      termDesc <- obo[(x[1]+1):(x[2]-1)]
      ##
      fn <- "^id: "
      id <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(id)==0) id <- NA
      ##
      fn <- "^name: "
      name <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(name)==0) name <- NA
      ##
      fn <- "^def: "
      def <- sub(fn, "", grep(fn, termDesc, value=T))
      if(length(def)==0) def <- NA
      ##
      fn <- "^is_a: "
      parent <- sub(fn, "", grep(fn, termDesc, value=T))
      fn <- " [!].*$"
      parent <- sub(fn, "", parent)
      if(length(parent)==0) parent <- NA
      ##
      fn <- "^alt_id: "
      altId <- sub(fn, "", grep(fn, termDesc, value=T))
      altId <- paste(unique(c(id, altId)), collapse=", ")
      ##
      return(data.frame(
        id=id, name=name, def=def,
        parent=parent,
        altId=altId,
        stringsAsFactors=F)
      )
   }
))
altId <- unique(hpDef[, c("id", "altId")])
hpDef <- hpDef[, setdiff(colnames(hpDef), "altId")]
hpParents <- unstack(hpDef, parent~id)
hpParents <- lapply(
  hpParents,
  function(x){
    x <- setdiff(unique(x), NA)
    return(x)
  }
)
hpDef <- unique(hpDef[,setdiff(colnames(hpDef), "parent")])
rownames(hpDef) <- hpDef$id

############################
## Alternative ID
altIdList <- strsplit(altId$altId, ", ")
names(altIdList) <- altId$id
altId <- stack(altIdList)
colnames(altId) <- c("alt", "id")
altId$id <- as.character(altId$id)
altId$alt <- as.character(altId$alt)


############################
## Getting all parents
print(Sys.time())
getAncestors <- function(hpId){
   direct <- hpParents[[hpId]]
   parents <- direct
   level <- 0
   dLev <- c()
   for(d in direct){
      dPar <- getAncestors(d)
      dLev <- c(dLev, dPar$level)
      parents <- c(parents, dPar$parents)
   }
   if(length(dLev)>0){
      level <- max(dLev)+1
   }
   return(list(parents=unique(parents), level=level))
}
hpAncestors <- lapply(
   unique(hpDef$id),
   getAncestors
)
names(hpAncestors) <- unique(hpDef$id)
hpDef$level <- unlist(lapply(hpAncestors, function(x) x$level))[hpDef$id]
hpAncestors <- lapply(hpAncestors, function(x) x$parents)
hpAncestors <- hpAncestors[rownames(hpDef)]
##
hpRel <- rbind(
  stack(hpAncestors),
  data.frame(values=names(hpAncestors), ind=names(hpAncestors))
)
hpAncestors <- unstack(hpRel, values~ind)
hpDescendants <- unstack(hpRel, ind~values)
##
print(Sys.time())

############################
## Classes of HP terms
hpClass <- lapply(
   hpDef$id,
   function(id){
      lev1 <- hpDef[which(hpDef$level==1), "id"]
      phClass <- c("HP:0000118")#, "HP:0040006")
      notPhClass <- setdiff(lev1, phClass)
      if(hpDef[id, "level"] <= 1){
         return(c())
      }
      if(!any(hpAncestors[[id]] %in% phClass)){
         return(intersect(hpAncestors[[id]], notPhClass))
      }
      if(hpDef[id, "level"] == 2){
         return(id)
      }
      toRet <- unique(intersect(hpAncestors[[id]], hpDef[which(hpDef$level==2), "id"]))
      toRet <- toRet[which(unlist(lapply(
         hpAncestors[toRet],
         function(x){
            any(x %in% phClass)
         }
      )))]
      # return(data.frame(id=id, class=toRet, stringsAsFactors=F))
      return(setdiff(unique(toRet), NA))
   }
)
names(hpClass) <- hpDef$id

############################
hp.definitions <- hpDef
hp.parents <- hpParents
hp.ancestors <- hpAncestors
hp.descendants <- hpDescendants 
hp.class <- hpClass
hp.altid <- altId

############################
rdFile <- file.path(
  tmpDir,
  paste(
    "phenotype_annotation-",
    Sys.Date(),
    ".tab",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="http://compbio.charite.de/hudson/job/hpo.annotations/lastStableBuild/artifact/misc/phenotype_annotation.tab",
    destfile=rdFile
  )
}
hpo <- read.table(
  file=rdFile,
  header=FALSE,
  quote="", comment="",
  sep="\t",
  stringsAsFactors=F
)
hp.diseases <- unique(hpo[,c(1:3, 5)])
names(hp.diseases) <- c("Source", "Phenotype ID", "Phenotype", "HP")

############################
toSave <- grep("^hp[.]", ls(), value=T)
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

