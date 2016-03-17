#' Connection to the unerlying SQLite DB
#' 
#' Open and/or return the connection to the SQLite DB
#' in which data tables are stored.
#' 
#' @return An SQLite connection
#' 
#' @example examples/dataModel.R
#' 
#' @seealso \code{\link{getDataModel}}
#' 
#' @export
#'
mhphdb.con <- function(){
    if(!exists("con", envir=datacache)){
        assign("con", dbConnect(SQLite(), datacache$dbfile), datacache)
    }
    return(datacache$con)
}

##########################
datacache <- new.env(hash=TRUE, parent=emptyenv())

##########################
.onLoad <- function(libname, pkgname)
{
    ## Connect to the SQLite DB
    dbfile <- system.file(
        "extdata", "mhphdb.sqlite",
        package=pkgname, lib.loc=libname
    )
    assign("dbfile", dbfile, envir=datacache)
}

##########################
.onUnload <- function(libpath)
{
    dbDisconnect(mhphdb.con())
}
