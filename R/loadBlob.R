######################################################################
# loadBlob

loadBlob <- function(blob,
					blobpath=getOption("pgobj.blobs")) {
	# this function loads blob directory into R object (i.e. memory)
	# options:
	# blob: name of blob object
	# blobpath: directory where blobs are stored


	# check arguments
	if(!is.character(blob)) {
		stop("blob is not character")
	}

	if(!is.character(blobpath)) {
		stop("blobpath is not character")
	}
	if(!file.exists(blobpath)) {
		stop("blobpath does not exist")
	}

	# get blob objec, getObj returns error if object does not exist

	blob.obj <- getObj(blob)
	if(!blob.obj$isObject) {
		# blob is a file, so do not load it into memory
		stop("blob is not an object")
	}

	# check md5
	fpath <- paste(blobpath,blob.obj$fname,sep='/')
	checkmd5(fpath,blob.obj)

	# load data
	blob.data <- readRDS(fpath)

	# return blob
	return(blob.data)

}


