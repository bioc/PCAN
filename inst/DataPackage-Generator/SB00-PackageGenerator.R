rm(list=ls())
gc()

library(RSQLite)

## Package update information
pckName <- "MultiHumanPhenoDB"
load("curOptions.rda")
curDate <- sub("TmpData-", "", tmpDir)
pckDir <- file.path(".", paste(pckName, curDate, sep="-"))

## Package documentation
library(roxygen2)
roxygenize(package.dir="Package-Template", clean=T)

## Package creation
dir.create(pckDir)
file.copy("Package-Template/R", pckDir, recursive=T)
file.copy("Package-Template/man", pckDir, recursive=T)
file.copy("Package-Template/NAMESPACE", pckDir, overwrite=T)

## DESCRIPTION file
descFile <- readLines("Package-Template/DESCRIPTION")
descFile <- c(descFile, paste("Date:", curDate))
write(descFile, file.path(pckDir, "DESCRIPTION"),ncolumns=1)

## Data files
dir.create(file.path(pckDir, "data"))
dir.create(file.path(pckDir, "inst", "extdata"), recursive=T)

## Copy documentation
file.copy(from="Documentation", to=file.path(pckDir,"inst"), recursive = T)

## Feed an SQLite DB and save objects which are note data.frame in .rda data files
db <- dbConnect(
    SQLite(),
    dbname=file.path(pckDir, "inst", "extdata", "mhphdb.sqlite")
)
fToLoad <- list.files(
    path=tmpDir,
    pattern="[.]rda$",
    full.names=T
)
for(f in fToLoad){
    message(f)
    load(f)
    on <- sub("[.]rda", "", basename(f))
    ## Make object name compatible with SQLite db
    newOn <- gsub("[.]", "_", on)
    assign(newOn, get(on))
    on <- newOn
    rm(newOn)
    if(!inherits(get(on), "data.frame")){
        message("   --> To file")
        save(
            list=on,
            file=file.path(pckDir, "data", paste(on, ".rda", sep=""))
        )
    }else{
        message("   --> To DB")
        dbWriteTable(
            conn=db,
            name=on,
            value=get(on),
            overwrite=T,
            row.names=F
        )
    }
}
dbDisconnect(db)
