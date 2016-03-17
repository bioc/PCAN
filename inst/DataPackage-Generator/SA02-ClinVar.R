rm(list=ls())
gc()

#############################
load("curOptions.rda")

####################
## Documentation
rdFile <- file.path(
  tmpDir,
  paste(
    "ClinVar-README-",
    Sys.Date(),
    ".txt",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/README.txt",
    destfile=rdFile
  )
}

####################
rdFile <- file.path(
  tmpDir,
  paste(
    "disease_names-",
    Sys.Date(),
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/disease_names",
    destfile=rdFile
  )
}
rd <- readLines(rdFile)
header <- unlist(strsplit(rd[1], split="\t"))
rdlist <- strsplit(rd[-1], "\t")
rdlist <- lapply(
  rdlist, function(x){
    x <- sub("^ ", "", sub(" $", "", x))
    toRet <- x
    if(length(x)>7){
      toRet <- c(
        paste(x[1:(length(x)-6)], collapse=" // "),
        x[(length(x)-5):length(x)]
      )
    }
    return(toRet)
  }
)
cv.diseaseNames <- as.data.frame(do.call(
  rbind,
  rdlist
), stringsAsFactors=F)
colnames(cv.diseaseNames) <- c("name", "source", "concept", "sourceID", "MIM", "LastModif", "Category")
rm(rd, rdlist, header)

####################
cv.clinSigOrder <- 1:13
names(cv.clinSigOrder) <- c(
  "protective",
  "Benign",
  "Likely benign",
  "drug response",
  "confers sensitivity",
  "conflicting data from submitters", 
  "not provided",
  "Uncertain significance",
  "other",
  "association",
  "risk factor",
  "Likely pathogenic",
  "Pathogenic"
)
cv.revStatOrder <- 1:5
names(cv.revStatOrder) <- c(
  "not classified by submitter",
  "classified by single submitter", 
  "classified by multiple submitters",
  "reviewed by professional society",
  "reviewed by expert panel"
)


####################
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
