######################################################################
# deleteblob

deleteBlob <- function(blob) {
	# delete blob. Removes blob object from database and deletes files

	if(!is.character(blob)) {
		stop("blob is not character")
	}

	blob.obj <- getObj(blob)
	blob.fname <- paste(blob.obj$path,blob.obj$fname,sep="/")
	blob.ini <- paste(blob.fname,"ini",sep=".")

	if(!file.exists(blob.fname)) {
		stop(paste("blob",blob.fname,"not found"))
	}

	cat("removing",blob.fname,"\n")
	file.remove(blob.fname)
	cat("removing",blob.ini,"\n")
	file.remove(blob.ini)
	deleteObj(blob)
	deleteObj(paste(blob,"md5",sep="."))
}


