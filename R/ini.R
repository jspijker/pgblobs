
read.ini <- function(inifile) {
	# read ini file and returns data.frame with section, key and value
	# pairs
	# arguments:
	# inifile: name of inifile

	# base on code taken from R-help mailinglist
	# (https://stat.ethz.ch/pipermail/r-help/2007-June/134115.html), by 
	# Earl F. Glynnm and Gabor Grothendieck.

	connection <- file(inifile)
	ini<- readLines(connection)
	close(connection)

	# remove '#' comments
	ini <- ini[grep("^#",ini,perl=TRUE,invert=TRUE)]

	# change section headers
	ini <- chartr("[]", "==", ini)  # change section headers

	connection <- textConnection(ini)
	d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
	close(connection)

	# location of section breaks
	L <- d$V1 == ""
	d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],V1 != "")

	txt <- subset(d,V3=="text")
	txt <- paste(txt$V1,collapse="\n\n")
	kv <- subset(d,select=c("V3","V1","V2"),V3!="text")
	names(kv) <- c("section","key","value")

	meta <- list(kv=kv,text=txt)

	return(meta)
}


write.ini <- function(ini) {

	s <- c()
	for (i in unique(ini$kv$section)) {
		s <- append(s,paste("[",i,"]",sep=''))
		ini.s <- subset(ini$kv,section==i)
		for (j in 1:nrow(ini.s)) {
			s <- append(s,paste(ini.s$key[j],"=",ini.s$value[j]))
		}
		s <- append(s,"")
	}

	s <- append(s,"[text]")
	s <- append(s,ini$text)
	s <- append(s,"")
	s <- paste(s,collapse="\n")

	return(s)
}


