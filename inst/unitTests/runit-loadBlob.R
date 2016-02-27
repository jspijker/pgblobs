library(RPostgreSQL)
library(pgobjects)
library(localoptions)
readOptions("~/.R.options")

test.loadBlobExceptions <- function() {

	source("../inst/unitTests/sysSetup.R")

	checkException(loadBlob(blob=1))
	checkException(loadBlob(blob=blobname,blobpath=1))
	checkException(loadBlob(blob=blobname,blobpath="nonexistingpath"))
	checkException(loadBlob(blob="nonexistingblobname"))

	# if blob is not object, return error
	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

	if(!tableExists("robjects")){
		createPgobjTables()
	}

	blob.obj <- createBlob(fname=testdat,name=blobname,
						  kv=kvlist,
						  description="a test blobl")
	checkTrue(objectExists(blobname))
	checkException(loadBlob(blobname))

	deleteBlob(blobname)

	PgObjectsClose()
}


test.loadBlob <- function() {
	
	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

	if(!tableExists("robjects")){
		createPgobjTables()
	}

	# create blob, add some complexity
	x1 <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
	x2 <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
	blob.in <- list(x1=x1,x2=x2,x3=c(rnorm(100)))

	blob.obj <- createBlob(obj=blob.in,name=blobname,
						   kv=kvlist,
						   description="a test blob")
	checkTrue(objectExists(blobname))

	blob.data <- loadBlob(blobname)
	checkIdentical(blob.in,blob.data)
	deleteBlob(blobname)
	PgObjectsClose()

}

test.loadBlobMd5 <- function() {
	# test blob md5 functionality


	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

	if(!tableExists("robjects")){
		createPgobjTables()
	}

	# create blob, add some complexity
	x1 <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
	x2 <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
	blob.in <- list(x1=x1,x2=x2,x3=c(rnorm(100)))

	blob.obj <- createBlob(obj=blob.in,name=blobname,
						   kv=kvlist,
						   description="a test blob")
	checkTrue(objectExists(blobname))

	# now alter stored blobfile
	filepath <- paste(blobpath,"/",blobname,".rds",sep="")
	md5orig <- getmd5(filepath)
	# check if we got right file
	checkIdentical(blob.obj$md5,md5orig)
	# alter file
	blob.in <- list(x1=x1,x2=x2,x3=c(rnorm(101)))
	saveRDS(blob.in,filepath)
	checkException(loadBlob(blobname))
	deleteBlob(blobname)
	PgObjectsClose()
}
