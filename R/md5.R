######################################################################
# md5 functions
######################################################################

getmd5 <- function(filename) {
	# calculates md5 hash of filename

	if(!is.character(filename)) {
		stop("filename is not character")
	}
	if(!file.exists(filename)) {
		stop("fname does not exists")
	}

	md5sum <- system2("md5sum",args=c(filename),stdout=TRUE)
	md5 <- strsplit(md5sum,split=" ")[[1]][1]
	return(md5)

}


checkmd5 <- function(path,blobobj) {
	# checks md5 of file (path) and compares with storedmd5 in blob
	# object. Immediately throws an error when they do not match

	if(!is.character(path)) {
		stop("path is not character")
	}
	if(!file.exists(path)) {
		stop("path does not exist")
	}

	if(!is.list(blobobj)) {
		stop("blob is not list ")
	}

	md5 <- getmd5(path)
	if(md5!=blobobj$md5) {
		cat("ERROR in md5 checksum\n")
		cat("file on disk:",path,"\n")
		cat("md5 of file:",md5,"\n")
		cat("expected md5 ",blobobj$md5,"\n")
		stop("md5 error")
	} else {
		return(TRUE)
	}

}



