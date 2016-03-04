library(RPostgreSQL)
library(pgobjects)
library(localoptions)
readOptions("~/.R.options")

test.getBlobExceptions <- function() {

	source("../inst/unitTests/sysSetup.R")

	checkException(getBlob(blob=1))
	checkException(getBlob(blob=blobname,path=1))
	checkException(getBlob(blob=blobname,path="nonexistingpath"))
	checkException(getBlob(blob=blobname,path=testpath,blobpath=1))
	checkException(getBlob(blob=blobname,
						   path=testpath,blobpath="nonexistingpath"))
	checkException(getBlob(blob="nonexistingblobname"))

}


test.getBlob <- function() {

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

	blob.out <- getBlob(blob=blobname,path=testtmp)
	checkIdentical(blob.out,blob.obj)


	blobfile=paste(testtmp,"/",blobname,".rds",sep="")
	checkTrue(file.exists(blobfile)) 
	checkIdentical(blob.out$fname,basename(blobfile))
	file.remove(blobfile)

	deleteBlob(blobname)

	PgObjectsClose()

}

test.getBlobMd5 <- function() {
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
	checkException(getBlob(blobname,path=testtmp))
	file.remove(paste(testtmp,"/",blobname,".rds",sep=""))
	deleteBlob(blobname)
	PgObjectsClose()

}

