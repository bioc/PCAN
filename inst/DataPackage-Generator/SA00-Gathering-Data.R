rm(list=ls())
gc()

## Global param for building
mc.cores = 3 #detectCores() %/% 2

## Directory to store the temporary data
tmpDir <- paste("TmpData", Sys.Date(), sep="-")
dir.create(tmpDir)

## Save current options for the scripts
save(mc.cores, tmpDir, file="curOptions.rda")

## Get the data
# toRun <- list.files(pattern="^S[[:digit:]]{2}-.*[.]R$")
toRun <- setdiff(
  grep("^SA[[:digit:]]{2}-.*[.]R$", list.files(), value=T),
  "SA00-Gathering-Data.R"
)
for(sf in toRun) {
  message(sf)
  source(sf)
}
