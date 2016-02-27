
storeIniObj <- function(obj.data,obj.meta) {

	kv <- obj.meta$kv
	obj.name=subset(kv,key=="name")$value
	if(is.null(obj.name)){
		stop("no object name found")
	}
	print(obj.name)
	storeObj(obj.name,obj.data)

	kv.obj <- subset(kv,section=="object")
	for (i in 1:nrow(kv.obj)) {
		k <- kv.obj$key[i]
		v <- kv.obj$value[i]
		if(k=="name") {
			next
		}
		storeKeyval(obj=obj.name,key=k,val=v,overwrite=TRUE)
	}

	meta.name <- paste(obj.name,"meta",sep='-')
	storeObj(name=meta.name,obj.meta)


	kv.meta <- subset(kv,section=="meta")
	for (i in 1:nrow(kv.meta)) {
		k <- kv.meta$key[i]
		v <- kv.meta$value[i]
		storeKeyval(obj=meta.name,key=k,val=v,overwrite=TRUE)
	}
	storeKeyval(obj=meta.name,key="object",val=obj.name,overwrite=TRUE)
	storeKeyval(obj=obj.name,key="meta",val=meta.name,overwrite=TRUE)
}

getMetaObjName <- function(obj.name) {
	meta.obj <- NA
	try(meta.obj <- getKeyval(obj=obj.name,key="meta"))
	if(is.na(meta.obj)) {
		warning("meta object not found")
	}
	return(meta.obj)
}

getMetaObj <- function(obj.name) {
	meta.obj <- getMetaObjName(obj.name)
	if(is.na(meta.obj)) {
		stop("object not found")
	}
	obj <- getObj(meta.obj)

	#recreate kv values
	kv.obj <- cbind(section="object",getKeyvalObj(obj.name))
	kv.meta <- cbind(section="meta",getKeyvalObj(meta.obj))
	kv <- rbind(kv.obj,kv.meta)

	metaobj <- list(kv=kv,text=obj$text)
	return(metaobj)
}


