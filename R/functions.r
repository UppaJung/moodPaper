#> ---------------------------------------------------------------------------
#> Functions that belong only to this project.
#> ---------------------------------------------------------------------------

shortTreatmentName = function(code) {
	return(treatments[treatments[,1]==code, 2])
}

longTreatmentName = function(code) {
	return(treatments[treatments[,2]==code, 3])
}

obtainLastMinuteValues = function(mat) {
	mat[2,] <- mat[2,]-mat[1,]
	chi <- chisq.test(mat)
	return(c(chi$statistic, chi$p.value))
}

underlineThesePValues = function(mat, alpha=.05) {
	return(apply(as.matrix(mat), c(1,2), function(x){ if (x<alpha) paste('\\underline{', x, '}', sep='') else x }))
}

#> This function assumes a two-column list. The first column is a participant ID, and will be displayed verbatim.
#> The second column contains text.
displayFreeResponsesAsHTMLList = function(lst, type='ol') {
	if (nrow(lst)>0) {
		cat(cf.asHTMLBulletList(paste(lst[,1],': ', lst[,2], sep=''), type=type))
	} else {
		cat('<p>No responses to this question.</p>')
	}
}

getFreeResponses = function(dataset, col) {
	return(dataset[dataset[,c(col)]!='', c('pid',col)])
}

displayVariableAsProportionTable = function(var) {
	tmp = as.matrix(table(allfolks[,c(var)]))/nrow(allfolks)
	cat(cf.asHTMLTable(cf.asPercentage(tmp)))
}
