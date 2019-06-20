library(RPostgreSQL)
library(pgobjects)
library(localoptions)
readOptions("~/.R.options")

test.createBlobExceptions <- function() {

	# test input
	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))
	options("pgobj.blobs"=NULL)
	checkException(createBlob(kv=list(key1="val1"),file=testdat,
							  name=blobname, textfile=testtxt))

	checkException(createBlob(kv=list(key1="val1"),file=testdat,
							  name=blobname, textfile=testtxt,blobpath=1))

	options("pgobj.blobs"=blobpath)

	
	# options obj and file mutually exclusive
	checkException(createBlob(obj=1,file=testdat,name=blobname,blobpath=blobpath))
	checkException(createBlob(fname=1,name=blobname,blobpath=blobpath))
	checkException(createBlob(fname=testdat)) # forget name
	checkException(createBlob(fname=testdat,name=blobname,
							  kv=1))
	checkException(createBlob(fname=testdat,name=blobname,
							  description=1))
	checkException(createBlob(fname=testdat,name=blobname,
							  textfile=1))

	checkException(createBlob(fname=testdat,name=blobname,
							  textfile="nonExistetFileName.txt"))
	checkException(createBlob(fname="nonExistentFileName.dat",name=blobname,
							  textfile=testtxt))

	checkException(createBlob(kv=list("nonsens"),fname=testdat,
							  name=blobname,
							  textfile=testtxt))

	checkException(createBlob(kv=list(key1="val1",emptykey=""),fname=testdat,
							  name=blobname,
							  textfile=testtxt))

	checkException(createBlob(kv=list(key1="val1",emptykey=""),fname=testdat,
							  name=blobname,
							  textfile=testtxt))

	checkException(createBlob(fname=testdat,name=blobname,
                              kv=kvlist,description="test blob",
                              md5=1))

	checkException(createBlob(fname=testdat,name=blobname,
                              kv=kvlist,description="test blob",
                              overwrite=1))

	PgObjectsClose()
}


test.createBlob <- function() {

	
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


	
	# check blob from object, no textfile
	blob.obj <- createBlob(obj=blob.in,name=blobname,
						  kv=kvlist,
						  description="a test blobl")
	checkTrue(objectExists(blobname))
	checkTrue(isBlob(blobname))


	deleteBlob(blobname)

	# check blob from object
	blob.obj <- createBlob(obj=blob.in,name=blobname,
						  textfile=testtxt,
						  kv=kvlist,
						  description="a test blobl")

	checkTrue(objectExists(blobname))
	checkTrue(isBlob(blobname))
	v1<-getKeyval(blobname,"key1")
	checkIdentical(v1,"value1")

	connection <- file(testtxt)
	txtcontent<- readLines(connection)
	close(connection)
	checkIdentical(blob.obj$text,txtcontent)
	deleteBlob(blobname)

	# check blob from file
	blob.obj <- createBlob(fname=testdat,name=blobname,
						  textfile=testtxt,
						  kv=kvlist,
						  description="a test blobl")


	checkTrue(objectExists(blobname))
	checkTrue(isBlob(blobname))
	v1<-getKeyval(blobname,"key1")
	checkIdentical(v1,"value1")
	checkTrue(file.exists(paste(blobpath,testfname,sep="/")))

	# check md5sums
	obj.stored <- getBlob(blob=blobname,path=testtmp)
	md5 <- obj.stored$md5
	print(md5)
	checkIdentical(as.character(md5),testdat.md5)
	# remove file from testtmp
	file.remove(paste(testtmp,testfname,sep="/"))
	
	deleteBlob(blobname)
	PgObjectsClose()
}

test.createBlob.file <- function(){
	# test situation where files cannot or should not be written to
	# disk

	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

	# create blob, add some complexity
	x1 <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
	x2 <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
	blob.in <- list(x1=x1,x2=x2,x3=c(rnorm(100)))

	# check blob from object
	blob.obj <- createBlob(obj=blob.in,name=blobname,
						   textfile=testtxt,
						   kv=kvlist,
						   description="a test blobl")

	checkTrue(objectExists(blobname))
	checkTrue(isBlob(blobname))

	# try to overwrite blob

	checkException(createBlob(obj=blob.in,name=blobname,
							  textfile=testtxt,
							  kv=kvlist,
							  description="a test blobl"))

	deleteBlob(blobname)

# test errors in case file cannot be written due to permissions
	checkException(createBlob(obj=blob.in,name=blobname,
							  textfile=testtxt,
							  kv=kvlist,
							  description="a test blobl",
							  blobpath="/"))

	deleteBlob(blobname)

	# try to overwrite file 
	file.create(paste(blobpath,"/",blobname,".rds",sep=""))

	checkException(createBlob(obj=blob.in,name=blobname,
							 textfile=testtxt,
							 kv=kvlist,
							 description="a test blobl"))

	file.remove(paste(blobpath,"/",blobname,".rds",sep=""))

	deleteBlob(blobname)
	PgObjectsClose()

}

test.createblob.md5 <- function() {
    # this function tests md5 check of creatblob. If md5 argument of
    # createblob does not match md5 of file, createblob should give
    # error.


	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

    # alter md5, creat bogus md5
    md5.new <- testdat.md5
    md5.new <- gsub("e","0",md5.new)
    if(md5.new == testdat.md5) {
        # our bogus md5 is the same as the original md5, shit should
        # not happen
        stop("me5.new == testdat.md5")
    }

    # make sure no stale blob exists, if blob exists this will result
    # in the wrong exception
	try(deleteBlob(blobname),silent=TRUE)
    # test exemptions

	checkException(createBlob(fname=testdat,name=blobname,
						  textfile=testtxt,
						  kv=kvlist,md5=md5.new,
						  description="a test blob"))

	checkTrue(!file.exists(paste(blobpath,"/",testfname,sep="")))
	checkTrue(!objectExists(blobname))
	deleteBlob(blobname)
	PgObjectsClose()
}

test.createblob.overwrite <- function() {
    # test exeception for overwriting objects


	source("../inst/unitTests/sysSetup.R")

	PgObjectsInit(dbname=getOption("pgobj.dbname"),
				  passwd=getOption("pgobj.password"))

	try(deleteBlob(blobname),silent=TRUE)
    
    x <- 1
    storeObj(blobname,x)
    checkException(createBlob(fname=testdat,name=blobname,
                              textfile=testtxt,overwrite=FALSE,
                              kv=kvlist, description="a test blob"))

	checkTrue(!file.exists(paste(blobpath,"/",testfname,sep="")))
	checkTrue(!isBlob(blobname))
	PgObjectsClose()
}
