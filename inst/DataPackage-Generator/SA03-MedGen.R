rm(list=ls())
gc()

#############################
load("curOptions.rda")

#################################
## Documentation
rdFile <- file.path(
  tmpDir,
  paste(
    "MedGen-README-",
    Sys.Date(),
    ".txt",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/medgen/README.txt",
    destfile=rdFile
  )
}
##
rdFile <- file.path(
  tmpDir,
  paste(
    "NAMES-",
    Sys.Date(),
    ".RRF.gz",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/medgen/NAMES.RRF.gz",
    destfile=rdFile
  )
}
rd <- readLines(rdFile)
header <- unlist(strsplit(rd[1], split="[|]"))
rdlist <- strsplit(rd[-1], "[|]")
rdlist <- lapply(
  rdlist, function(x){
    x <- sub("^ ", "", sub(" $", "", x))
    toRet <- x
    if(length(x)>4){
      toRet <- c(
        x[1],
        paste(x[2:(length(x)-2)], collapse=" // "),
        x[length(x)-1],
        x[length(x)]
      )
    }
    return(toRet)
  }
)
mg.concepts <- as.data.frame(do.call(
  rbind,
  rdlist
), stringsAsFactors=F)
colnames(mg.concepts) <- header
colnames(mg.concepts) <- c("CUI", "name", "source", "suppress")
rownames(mg.concepts) <- mg.concepts$CUI
mg.concepts <- mg.concepts[,c("CUI", "name", "source")]
rm(rd, rdlist, header)
##
rdFile <- file.path(
  tmpDir,
  paste(
    "MGDEF-",
    Sys.Date(),
    ".csv.gz",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/medgen/csv/MGDEF.csv.gz",
    destfile=rdFile
  )
}
mg.definitions <- read.table(
  rdFile,
  sep=",", header=T, quote="\"", comment.char="",
  stringsAsFactors=F, check.names=F
)
colnames(mg.definitions) <- c("CUI", "definition", "source", "suppress")
rownames(mg.definitions) <- mg.definitions$CUI
mg.definitions <- mg.definitions[, c("CUI", "definition", "source")]
##
rdFile <- file.path(
  tmpDir,
  paste(
    "MedGen_HPO_Mapping-",
    Sys.Date(),
    ".txt.gz",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/medgen/MedGen_HPO_Mapping.txt.gz",
    destfile=rdFile
  )
}
mg.hpo <- read.table(
  rdFile,
  sep="|", header=T, quote="", comment.char="",
  stringsAsFactors=F, check.names=F
)
mg.hpo <- mg.hpo[,-7]
colnames(mg.hpo) <- c("CUI", "SDUI", "HpoStr", "MedGenStr", "MedGenStr_SAB", "STY")
mg.hpoSty <- unique(mg.hpo[,c("CUI", "SDUI", "STY")])
mg.hpo <- unique(mg.hpo[,setdiff(colnames(mg.hpo), "STY")])
conToAdd <- unique(mg.hpo[
  which(!mg.hpo$CUI %in% mg.concepts$CUI),
  c("CUI", "MedGenStr", "MedGenStr_SAB")
])
colnames(conToAdd) <- c("CUI", "name", "source")
rownames(conToAdd) <- conToAdd$CUI
mg.concepts <- rbind(mg.concepts, conToAdd)
##
rdFile <- file.path(
  tmpDir,
  paste(
    "MedGen_HPO_OMIM_Mapping-",
    Sys.Date(),
    ".txt.gz",
    sep=""
  )
)
if(!file.exists(rdFile)){
  download.file(
    url="ftp://ftp.ncbi.nlm.nih.gov/pub/medgen/MedGen_HPO_OMIM_Mapping.txt.gz",
    destfile=rdFile
  )
}
mg.omim.hpo <- read.table(
  rdFile,
  sep="|", header=T, quote="", comment.char="",
  stringsAsFactors=F, check.names=F
)
mg.omim.hpo <- mg.omim.hpo[,-11]
colnames(mg.omim.hpo) <- c(
  "OMIM_CUI", "MIM_number", "OMIM_name",
  "relationship", "HPO_CUI", "HPO_ID", "HPO_name",
  "MedGen_name", "MedGen_source", "STY"
)
mg.omim <- unique(mg.omim.hpo[,c(
  "OMIM_CUI", "MIM_number",
  "relationship", "HPO_CUI"
)])
colnames(mg.omim) <- c("CUI", "MIM", "relationship", "HPO.CUI")
mg.omimNames <- unique(mg.omim.hpo[,c("MIM_number", "OMIM_name")])
colnames(mg.omimNames) <- c("MIM", "name")
rm(mg.omim.hpo)

#################################
## Saving the data
toSave <- grep("^mg[.]", ls(), value=T)
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

