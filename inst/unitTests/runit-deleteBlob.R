library(RPostgreSQL)
library(pgobjects)
library(localoptions)
readOptions("~/.R.options")

test.deleteBlobExceptions <- function() {
	checkException(deleteBlob(blob=1))

}


test.deleteBlobObject <- function() {

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
						  description="a test blobl")
	checkTrue(objectExists(blobname))

	deleteBlob(blobname)
	checkTrue(!objectExists(blobname))

	PgObjectsClose()
}

test.deleteBlobFile <- function() {

	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

	if(!tableExists("robjects")){
		createPgobjTables()
	}

	ini.obj <- paste(blobpath,"/",testfname,".ini",sep="")

	file.remove(paste(blobpath,testfname,sep="/"))
	file.remove(ini.obj)

	blob.obj <- createBlob(fname=testdat,name=blobname,
						  textfile=testtxt,
						  kv=kvlist,
						  description="a test blobl")


	checkTrue(objectExists(blobname))
	checkTrue(file.exists(ini.obj))

	deleteBlob(blobname)

	checkTrue(!objectExists(blobname))
	checkTrue(!file.exists(ini.obj))

	PgObjectsClose()

}
