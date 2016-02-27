library(RPostgreSQL)
library(pgobjects)
library(localoptions)
readOptions("~/.R.options")



test.getmd5 <- function() {

	source("../inst/unitTests/sysSetup.R")
	checkException(getmd5(filename=1))
	checkException(getmd5(filename="nonexistentfilename"))

	md5.test<-getmd5(testdat)
	checkIdentical(md5.test,testdat.md5)
}

test.checkmd5 <- function() {


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


	fpath <- paste(blobpath,blob.obj$fname,sep='/')
	# checkExceptions
	checkException(checkmd5(1,blob.obj))
	checkException(checkmd5(fpath,1))

	checkTrue(checkmd5(fpath,blob.obj))

	x<-rnorm(100)
	saveRDS(x,fpath)

	checkException(checkmd5(fpath,blob.obj))
	deleteBlob(blobname)
	PgObjectsClose()

}




