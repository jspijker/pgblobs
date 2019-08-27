#' Import blobs 
#'
#' This function import blobs (file and meta-data file) from elsewhere
#' into the current pbobjects database
#'
#' For each blobfile a meta-data file must exists in the same
#' directory (with extention .ini)
#'
#' @return the name of the created blob object
#'
#' @param importfile full path of the file to import
#' @param overwrite Should existing blobs be overwritten
#'
#' Please note that this function overwrites existing files in the
#' data directory
#'
#' @export


importBlob <- function(importfile,overwrite=FALSE) {

    inifile <- paste(importfile,"ini",sep=".")

    if(!file.exists(importfile)) {
        stop(paste("importfile",importfile,"does not exists"))
    }

    # read ini file
    if(!file.exists(inifile)) {
        stop(paste("inifile",inifile,"does not exists"))
    }

    ini <- read.ini(inifile)

    #copy file
    destfile <- datafile(basename(importfile))
    file.copy(importfile,destfile,overwrite=TRUE)

    # extract meta data from ini file
    fname <- subset(ini$kv,section=="meta"&key=="src")$value
    name <- subset(ini$kv,section=="meta"&key=="name")$value
    desc <- subset(ini$kv,section=="meta"&key=="description")$value
    md5 <- subset(ini$kv,section=="object"&key=="md5")$value

    # create txt file with long description of blob
    txtfile <- datafile("importblob_description.txt")
    f <- file(txtfile)
    cat(ini$text,file=f,sep="\n")
    close(f)

    # create kvlist
    kvpairs <- subset(ini$kv,select=c("key","value"),section=="object")
    kvlist <- kvpairs$value
    names(kvlist) <- kvpairs$key
    kvlist <- as.list(kvlist)


    if(overwrite) {
        if(objectExists(name)) {
            deleteBlob(name)
        }
    }

   if(!is.null(kvlist$description)) {
      desc=""
   } 

    x<-createBlob(fname=destfile,name=name,kv=kvlist,
                  description=desc,md5=md5,
                  textfile=txtfile)


    # # remove tmp file
    file.remove(txtfile)
    # # return name of object
    invisible(name)
    # 

}


