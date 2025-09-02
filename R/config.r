#> ---------------------------------------------------------------------------
#> Basic configuration. These variables and settings are supposed to be
#> available throughout the system.
#> ---------------------------------------------------------------------------
options(stringsAsFactors = FALSE)
#options(scipen=999)

#> ---------------------------------------------------------------------------
#> Miscelaneous functions
#> ---------------------------------------------------------------------------
expandfile = function(file='', path='data') {
	if (path=='data')
		return(paste(clio.root, 'data/', file, sep=''))
	if (path=='dataproc')
		return(paste(clio.root, 'data/processed/', file, sep=''))
	if (path=='paper')
		return(paste(clio.root, 'paper/', file, sep=''))
	if (path=='output')
		return(paste(clio.root, 'output/', file, sep=''))
	if (path=='r' | path=='R')
		return(paste(clio.root, 'R/', file, sep=''))
	stop("Wrong place argument to expandfile(). It should be either 'data', 'dataproc', 'paper', 'output' or 'r'")
}

#> ---------------------------------------------------------------------------
setwd(expandfile(path='r'))
log.file = expandfile(file='clio.log', path='r')
#> ---------------------------------------------------------------------------

runandlog = function(com) {
	tmp = system(com, intern=TRUE)
	if (length(tmp)>0)
		for (i in tmp)
			ms.addLog('[', com, '] ', i)
}

ms.writeFileHeader = function(filename, format='tex') {
	if (format=='tex') {
		cat(paste("%> File", filename, "created automatically on", date(), "\n\n"), file = filename)
	} else if (format=='html') {
		cat(paste("<!-- File", filename, "created automatically on", date(), "-->\n"), file=filename)
	} else if (format=='raw') {
		cat(paste("#> File", filename, "created automatically on", date(), "\n"), file=filename)
	} else {
		warning("ms.writeFileHeader: format not supported, no output.")
	}
}

ms.read.csv = function(filename, path = expandfile(path='data')) {
	return(read.csv(paste(path, filename, sep=''), header=TRUE, sep=",", row.names=NULL))
}

ms.write.csv = function(thisdata, filename=NULL) {
	if (is.null(filename))
		stop('ms.write.csv(): You must specify a filename')
	if (substr(filename,nchar(filename)-3,nchar(filename))!='.csv')
		filename = paste(filename, '.csv', sep='')
	write.csv(thisdata, filename, row.names=FALSE)
}

ms.spit = function(..., file = NULL, head = "") {
	if (is.null(file)) stop('File cannot be null.')
	cat(head, ..., "\n", file = file, sep = "", append = TRUE)
}

ms.startLogFile = function() {
	ms.writeFileHeader(log.file, format='raw')
}

ms.addLog = function(...) {
	ms.spit(as.character(date()), ': ', ..., file = log.file, head='#> ')
}

ms.addVarVal = function(var, val, comment = NULL) {
	if (is.null(comment)) {
		ms.spit(as.character(var), ': ', as.character(val), file = log.file, head = '')
	} else {
		ms.spit(as.character(var), ': ', as.character(val), ' #> ', comment, file = log.file, head = '')
	}
}

ms.getVarVal = function(var) {
	vals = ms.getVarVals()
	return(vals[vals[,1]==var, 2])
}

ms.getVarVals = function() {
	wen = file(log.file, open='rt')
	logs = readLines(wen)
	close(wen)
	wen = strsplit(logs[grep('^#', logs, perl=TRUE, invert=TRUE)], ': ', fixed=TRUE)
	textlogs = logs[grep('^#', logs, perl=TRUE)]
	logs = NULL
	if (length(wen)>0)
	{
		for (i in 1:length(wen))
			logs = rbind(logs, wen[i][[1]])
		rm(i)
	}
	return(logs)
}


#> ---------------------------------------------------------------------------
#> Formatting functions
#> ---------------------------------------------------------------------------
cf.replace = function(vec, from, to) {
  replace(vec, which(vec==from), to)
}

cf.asPercentage = function(val, digits=1) {
	if (class(val)=='numeric' | class(val)=='character') {
		tmp = paste(round(100*as.numeric(val),digits), '%', sep='')
		return(replace(tmp, which(tmp=='NA%' | tmp=='Inf%'), ''))
	} else if (class(val)=='matrix' | class(val)=='table') {
		return(apply(val, c(1,2), function(x) { paste(round(100*as.numeric(x),digits), '%', sep='') }))
	} else {
		stop(paste('cf.asPercentage(val): I don\'t know how to convert val, because class(val)==', class(val), sep=''))
	}
}

cf.asSinglePValue = function(val, digits=4, latex=TRUE) {
	if (round(val, digits=digits)==0)
		return(paste(ifelse(latex,'$',''), "<0.", paste(rep.int('0',digits-1), sep='', collapse=''), '1', ifelse(latex,'$',''), sep=''))
	else
		return(sprintf(paste('%.', digits, 'f', sep=''), round(val, digits=digits)))
}

cf.asPValue = function(val, digits=4) {
	return(apply(as.matrix(val), c(1,2), cf.asSinglePValue, digits=digits))
}

cf.asHTMLTable = function(mat, usenames = TRUE, usefirstcol = FALSE, usefirstrow = FALSE, usetopleft = FALSE) {
	mat = apply(mat, c(1,2), function(x) { if (is.na(x)) {''} else {x}})
	mat2 = mat3 = matrix(ncol=ncol(mat), nrow=nrow(mat), data = 'td')

	if (usefirstrow)
		mat2[1,] = "th class='t'"
	if (usefirstcol)
		mat2[,1] = "th class='l'"
	if (usefirstrow & usefirstcol) {
		mat2[1,1] = if (usetopleft) "th class='t l'" else 'td'
		if (!usetopleft) mat3[1,1] = 'td'
	}

	if (usenames) {
		if (!is.null(colnames(mat))) {
			mat = rbind(colnames(mat), mat)
			mat2 = rbind(rep.int("th class='t'", ncol(mat2)), mat2)
		}
		if (!is.null(rownames(mat))) {
			mat = cbind(rownames(mat), mat)
			mat2 = cbind(rep.int("th class='l'", nrow(mat2)), mat2)
		}
		if (!is.null(colnames(mat)) & !is.null(rownames(mat))) {
			mat2[1,1] = if (usetopleft) "th class='t l'" else 'td'
			if (!usetopleft) mat3[1,1] = 'td'
		}
	}

	tmp = matrix(ncol=ncol(mat), data = paste('<', mat2, '>', mat, '</', mat3, '>', sep=''))
	cal = NULL
	for (i in 1:nrow(tmp))
		cal = c(cal, '<tr>', paste(tmp[i,], sep='', collapse=''), '</tr>\n')
	return(paste(cal, collapse=''))
}

cf.asLatexTabular = function(mat, digits=5, usenames = TRUE) {
	mat = apply(mat, c(1,2), function(x) { if (is.na(x)) {''} else {x}})
	mat = apply(mat, c(1,2), round, digits=digits)

	if (usenames) {
		if (!is.null(colnames(mat)))
			mat = rbind(paste("\\textit{", colnames(mat), '}', sep=''), mat)
		if (!is.null(rownames(mat)))
			mat = cbind(paste("\\textit{", rownames(mat), '}', sep=''), mat)
		if (!is.null(colnames(mat)) & !is.null(rownames(mat)))
			mat[1,1] = ''
	}

	tmp = NULL
	for (i in 1:nrow(mat))
		tmp = c(tmp, paste(mat[i,], sep='', collapse=' & '))
	for (i in 1:length(tmp))
		tmp[i] = paste(tmp[i], "\\\\")
	return(tmp)
}

#> Use this function when you want to display the different levels of a categorical variable.
cf.asValueTable = function(col) {
	return(cf.asHTMLTable(t(as.matrix(table(col)))))
}

#> Use this function when you want to display a list of items as a bulleted HTML list.
cf.asHTMLBulletList = function(lst, item = 'li', type = 'ol') {
	return(paste('<', type, '>', paste('<',item,'>',lst,'</',item,'>', sep='', collapse='\n'), '</', type, '>', sep=''))
}

# cf.secAsMinSec = function(seconds) {
#   min = trunc(seconds/60)
#   sec = trunc(seconds - min*60)
#   return(paste(min,'min', sec, 'sec'))
# }
# 
# cf.msAsSec = function(milliseconds, label = TRUE) {
#   if (label)
#     return(paste(round(milliseconds/1000,1),'sec'))
#   else
#     return(round(milliseconds/1000,1))
# }


#> ---------------------------------------------------------------------------
#> Numerical functions
#> ---------------------------------------------------------------------------
num.coerceToLikert = function(lst, points = 5) {
	likert <- rep.int(0, points)
	for (i in 1:length(lst)) {
		likert[round(lst[i])] <- likert[round(lst[i])] + 1
	}
	return(likert)
}


#> ---------------------------------------------------------------------------
#> Power functions
#> ---------------------------------------------------------------------------
pow.check = function(mat, format) {
	if (class(mat)!='matrix' | nrow(mat)!=2 | ncol(mat)!=2)
		stop('stt.power(mat, format): mat has to be a 2x2 matrix.')
	if (format!='prop' & format!='yesno')
		stop('stt.power(mat, format): format has to be either \'prop\' or \'yesno\'')
	if (format=='yesno')
		mat[,2] = mat[,2] + mat[,1]
	return(mat)
}

pow.vals = function(mat) {
	p1 = mat[1,1]/mat[1,2]
	p2 = mat[2,1]/mat[2,2]
	h = 2*(asin(sqrt(p1))-asin(sqrt(p2)))
	return(c(p1, p2, h))
}

#> It receives a 2x2 matrix, and calculates current power based on two proportions, contained in rows of the matrix.
#> If format=='prop', then [,1] is the number of 'yes'es, and [,2] is the total.
#> If format=='yesno', then [,1] is the number of 'yes'es, and [,2] is the number of 'no's.
pow.single = function(mat, format, alpha=0.05) {
	mat = pow.check(mat, format)
	vals = pow.vals(mat)
	library(pwr)
	return(pwr.2p2n.test(h=vals[3],
				   n1=mat[1,2],
				   n2=mat[2,2],
				   sig.level=alpha)$power)
}

pow.samplesize.matrix = function(mat, format, alpha=0.05, scale=seq(0.5, 5, 0.5)) {
	mat = pow.check(mat, format)
	vals = pow.vals(mat)
	library(pwr)
	narf = matrix(ncol=length(scale), nrow=length(scale))
	for (i in 1:length(scale))
		for (j in 1:length(scale))
			narf[i,j] = pwr.2p2n.test(h=vals[3], n1=floor(mat[1,2]*scale[i]), n2=floor(mat[2,2]*scale[j]), sig.level=alpha)$power
	rownames(narf) = colnames(narf) = paste(scale,'X', sep='')
	return(narf)
}
