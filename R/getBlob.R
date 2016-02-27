######################################################################
# getblob

getBlob <- function(blob, path="./",
					blobpath=getOption("pgobj.blobs")) {
	# options:
	# blob: name of blob object
	# path: if blob object is file, copy to path
	# blobpath: directory where blobs are stored


	##################################################################
	# check options

	if(!is.character(blob)) {
		stop("blob is not character")
	}

	if(!is.character(path)) {
		stop("path is not character")
	}
	if(!file.exists(path)) {
		stop("path does not exist")
	}

	if(!is.character(blobpath)) {
		stop("blobpath is not character")
	}
	if(!file.exists(blobpath)) {
		stop("blobpath does not exist")
	}

	##################################################################
	# get blob

	# get blob object, getObj returns error if blob does not exist
	blob.obj <- getObj(blob);

	# get blobfile
	blobfile <- paste(blobpath,blob.obj$fname,sep='/')
	if(!file.exists(blobfile)) {
		stop(paste("blob",blobfile,"not found"))
	}
	file.copy(blobfile,path,overwrite=TRUE)

	# check md5
	fpath <- paste(path,blob.obj$fname,sep='/')
	checkmd5(fpath,blob.obj)

	# return blob.obj
	return(blob.obj)

}


