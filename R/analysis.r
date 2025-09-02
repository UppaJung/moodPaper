#> ------------------------------------------------------------------------------------------
#> Project: Mood paper
#> Analysis.
#> ------------------------------------------------------------------------------------------

#> Next, we create a graph showing the proportion of answers for 'proceed' (yes, caution, unsure, no), split according to levels of 'Knew about study'.
proceed = NULL
for (i in 1:nrow(treatments)) {
	proceed = rbind(proceed, table(folksWhoDidntKnow$Proceed_Facebook[ folksWhoDidntKnow$Treatment==treatments[i] ]))
}
colnames(proceed) = c('No','Unsure','Yes, with\ncaution', 'Yes')
rownames(proceed) = treatments[,2]
grph.stackedFullProportionBarplot(proceed, width=8, height=4, outmargins=c(1.5, 15, 1.2, 4.5), inmargins=c(0.1, 0), legendoffset=0.2,
					    thiscolor=c('firebrick1','goldenrod1','darkolivegreen2','forestgreen'), useratiowithin=TRUE, orderfirst=TRUE)
rm(i,proceed)

#> Finally, we create a similar graph for the surrogancy
surrogate = NULL
for (i in 1:nrow(treatments)) {
	surrogate = rbind(surrogate, table(folksWhoDidntKnow$Surrogate_Facebook[ folksWhoDidntKnow$Treatment==treatments[i] ]))
}
colnames(surrogate) = c('No','Indifferent','Yes')
rownames(surrogate) = treatments[,2]
grph.stackedFullProportionBarplot(surrogate, file=expandfile(file=paste('grph-surrogate-alltreatments.pdf', sep=''), path='output'),
					    width=8, height=4, outmargins=c(1.5, 15, 1.2, 5), inmargins=c(0.1, 0), legendoffset=0.25,
					    thiscolor=c('firebrick1','goldenrod1','forestgreen'),  useratiowithin=TRUE, orderfirst=TRUE)
rm(i,surrogate)


#> --------------------------------------------------------------------------------------------------------------------------------
#> --------------------------------------------------------------------------------------------------------------------------------
#> --------------------------------------------------------------------------------------------------------------------------------
#> Here's what we do: we do chi2 comparisons between relevant pairs of treatments, and we also do a Mann-Whitney (DV is ordinal) to
#> check for "honestly significant" differences (Holm-Bonferroni for p-values extracted from the Mann-Whitney) between Control and
#> the rest of the treatments.

#> -------------------------------------------------------------------------------------------------------------------
#> 1. Mann-Whitney calculations.
#> Compute uncorrected p-values for all comparisons
pairwise.uncorrected.for.proceed_facebook = with(folksWhoDidntKnow,
								 pairwise.wilcox.test(as.numeric(Proceed_Facebook), Treatment, p.adjust.method = "none"))
pairwise.uncorrected.for.surrogate_facebook = with(folksWhoDidntKnow,
								   pairwise.wilcox.test(as.numeric(Surrogate_Facebook), Treatment, p.adjust.method = "none"))

#> Proceed calculations
proceed.mw.pvalues = matrix(ncol=2, nrow=0)
for (i in 1:nrow(treatment.pairs)) {
	proceed.mw.pvalues = rbind(proceed.mw.pvalues, c(
		paste('MWProceed', treatment.pairs[i,1], 'x', treatment.pairs[i,2], sep=''),
		pairwise.uncorrected.for.proceed_facebook$p.value[ c(shortTreatmentName(treatment.pairs[i,2])), c(shortTreatmentName(treatment.pairs[i,1])) ]
	))
}
proceed.mw.pvalues[,2] = sprintf("%.5f", as.numeric(proceed.mw.pvalues[,2]))
proceed.mw.pvalues.corrected = proceed.mw.pvalues
proceed.mw.pvalues.corrected[,1] = paste(proceed.mw.pvalues.corrected[,1], 'corrected', sep='')
proceed.mw.pvalues.corrected[,2] = sprintf("%.5f", p.adjust(proceed.mw.pvalues.corrected[,2], method='holm'))

#> Surrogate calculations
surrogate.mw.pvalues = matrix(ncol=2, nrow=0)
for (i in 1:nrow(treatment.pairs)) {
	surrogate.mw.pvalues = rbind(surrogate.mw.pvalues, c(
		paste('MWSurrogate', treatment.pairs[i,1], 'x', treatment.pairs[i,2], sep=''),
		pairwise.uncorrected.for.surrogate_facebook$p.value[ c(shortTreatmentName(treatment.pairs[i,2])), c(shortTreatmentName(treatment.pairs[i,1])) ]
	))
}
surrogate.mw.pvalues[,2] = sprintf("%.5f", as.numeric(surrogate.mw.pvalues[,2]))
surrogate.mw.pvalues.corrected = surrogate.mw.pvalues
surrogate.mw.pvalues.corrected[,1] = paste(surrogate.mw.pvalues.corrected[,1], 'corrected', sep='')
surrogate.mw.pvalues.corrected[,2] = sprintf("%.5f", p.adjust(surrogate.mw.pvalues.corrected[,2], method='holm'))
rm(i)
rm(pairwise.uncorrected.for.proceed_facebook, pairwise.uncorrected.for.surrogate_facebook)

#> -------------------------------------------------------------------------------------------------------------------
#> 2. Chi2 calculations

#> Proceed calculations
proceed.chi2.stats   = matrix(ncol=2, nrow=0)
proceed.chi2.pvalues = matrix(ncol=2, nrow=0)
nicework = table(folksWhoDidntKnow$Treatment, folksWhoDidntKnow$Proceed_Facebook)
nicework = cbind(nicework[,1], rowSums(nicework[,2:4]))
colnames(nicework) = c('no','rest')

for (i in 1:nrow(treatment.pairs)) {
	mat = nicework[c(shortTreatmentName(treatment.pairs[i,1]), shortTreatmentName(treatment.pairs[i,2])), ]
	thisname = paste(treatment.pairs[i,1], 'x', treatment.pairs[i,2], sep='')
	tst = chisq.test(mat)

	proceed.chi2.stats = rbind(proceed.chi2.stats, c(
		paste('ChiSqProceedStat', thisname, sep=''),
		sprintf("%.3f", tst$statistic)
	))

	proceed.chi2.pvalues = rbind(proceed.chi2.pvalues, c(
		paste('ChiSqProceed', thisname, sep=''),
		sprintf("%.5f", tst$p.value)
	))
}
proceed.chi2.pvalues.corrected = proceed.chi2.pvalues
proceed.chi2.pvalues.corrected[,1] = paste(proceed.chi2.pvalues.corrected[,1], 'corrected', sep='')
proceed.chi2.pvalues.corrected[,2] = sprintf("%.5f", p.adjust(proceed.chi2.pvalues.corrected[,2], method='holm'))

#> Surrogate calculations
surrogate.chi2.stats   = matrix(ncol=2, nrow=0)
surrogate.chi2.pvalues = matrix(ncol=2, nrow=0)
nicework = table(folksWhoDidntKnow$Treatment, folksWhoDidntKnow$Surrogate_Facebook)
nicework = cbind(nicework[,1], rowSums(nicework[,2:3]))
colnames(nicework) = c('no','rest')

for (i in 1:nrow(treatment.pairs)) {
	mat = nicework[c(shortTreatmentName(treatment.pairs[i,1]), shortTreatmentName(treatment.pairs[i,2])), ]
	thisname = paste(treatment.pairs[i,1], 'x', treatment.pairs[i,2], sep='')
	tst = chisq.test(mat)

	surrogate.chi2.stats = rbind(surrogate.chi2.stats, c(
		paste('ChiSqSurrogateStat', thisname, sep=''),
		sprintf("%.3f", tst$statistic)
	))

	surrogate.chi2.pvalues = rbind(surrogate.chi2.pvalues, c(
		paste('ChiSqSurrogate', thisname, sep=''),
		sprintf("%.5f", tst$p.value)
	))
}
surrogate.chi2.pvalues.corrected = surrogate.chi2.pvalues
surrogate.chi2.pvalues.corrected[,1] = paste(surrogate.chi2.pvalues.corrected[,1], 'corrected', sep='')
surrogate.chi2.pvalues.corrected[,2] = sprintf("%.5f", p.adjust(surrogate.chi2.pvalues.corrected[,2], method='holm'))


#> Finally, we write everything to a macros file
thisfile = '../results_macros.tex'
ms.writeFileHeader(thisfile)

cat('%> Chi2 stats\n', file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\', proceed.chi2.stats[,1], '}{', proceed.chi2.stats[,2],'}\n', sep=''), file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\', surrogate.chi2.stats[,1], '}{', surrogate.chi2.stats[,2],'}\n', sep=''), file=thisfile, sep='', append=TRUE)

cat('\n%> Chi2 uncorrected p-values\n', file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\', proceed.chi2.pvalues[,1], '}{', underlineThesePValues(proceed.chi2.pvalues[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)
cat(paste('\\newcommand{\\', surrogate.chi2.pvalues[,1], '}{', underlineThesePValues(surrogate.chi2.pvalues[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)

cat('\n%> Chi2 corrected p-values\n', file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\', proceed.chi2.pvalues.corrected[,1], '}{', underlineThesePValues(proceed.chi2.pvalues.corrected[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)
cat(paste('\\newcommand{\\', surrogate.chi2.pvalues.corrected[,1], '}{', underlineThesePValues(surrogate.chi2.pvalues.corrected[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)

cat('\n%> Mann-Whitney uncorrected p-values\n', file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\', proceed.mw.pvalues[,1], '}{', underlineThesePValues(proceed.mw.pvalues[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)
cat(paste('\\newcommand{\\', surrogate.mw.pvalues[,1], '}{', underlineThesePValues(surrogate.mw.pvalues[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)

cat('\n%> Mann-Whitney corrected p-values\n', file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\', proceed.mw.pvalues.corrected[,1], '}{', underlineThesePValues(proceed.mw.pvalues.corrected[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)
cat(paste('\\newcommand{\\', surrogate.mw.pvalues.corrected[,1], '}{', underlineThesePValues(surrogate.mw.pvalues.corrected[,2]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)

#> Now we build an ordered logistic model, as described in http://www.ats.ucla.edu/stat/r/dae/ologit.htm
# require(MASS)
# olm.proceed = polr(formula = Proceed_Facebook ~ Treatment, data = folksWhoDidntKnow, Hess = TRUE)
# olm.surrogate = polr(formula = Surrogate_Facebook ~ Treatment, data = folksWhoDidntKnow, Hess = TRUE)
# summary(olm.proceed)
# summary(olm.surrogate)

#> A polr model doest not give p values by default, so we have to calculate them.
# coefs.proceed = coef(summary(olm.proceed))
# p = round(pnorm(abs(coefs.proceed[, "t value"]), lower.tail = FALSE) * 2, 6)
# coefs.proceed = cbind(coefs.proceed, `p value` = p)

# coefs.surrogate = coef(summary(olm.surrogate))
# p = round(pnorm(abs(coefs.surrogate[, "t value"]), lower.tail = FALSE) * 2, 6)
# coefs.surrogate = cbind(coefs.surrogate, `p value` = p)
# rm(p)

#> Run this manually to generate the calculations.log
# sink(file='calculations.log')
# kruskal.test(formula = Proceed_Facebook ~ Treatment, data = folksWhoDidntKnow)
# summary(olm.proceed)
# coefs.proceed
# print('--------------------------------')
# kruskal.test(formula = Surrogate_Facebook ~ Treatment, data = folksWhoDidntKnow)
# summary(olm.surrogate)
# coefs.surrogate
# sink()

#> Create latex tables with data from regressions.
# thistable = paste(
# 	paste('\\Describe{', tex.treatments, '} & ', sep='')[2:10],
# 	cf.asLatexTabular(coefs.proceed[1:9,], usenames=FALSE), sep='',
# 	c('', '\\hline', '', '\\hline', '', '\\hline', '\\hline', '', '')
# 	)
# thisfile = paste(clio.root, 'tables/tab_proceed_olr_data.tex', sep='')
# ms.writeFileHeader(thisfile)
# ms.spit(paste(thistable, sep='', collapse="\n"), file=thisfile)
# 
# thistable = paste(
# 	paste('\\Describe{', tex.treatments, '} & ', sep='')[2:10],
# 	cf.asLatexTabular(coefs.surrogate[1:9,], usenames=FALSE), sep='',
# 	c('', '\\hline', '', '\\hline', '', '\\hline', '\\hline', '', '')
# )
# thisfile = paste(clio.root, 'tables/tab_surrogate_olr_data.tex', sep='')
# ms.writeFileHeader(thisfile)
# ms.spit(paste(thistable, sep='', collapse="\n"), file=thisfile)
# rm(thistable,thisfile)

#> -----------------------------------------------------------------------------------------------
#> Last-minute addition!
thesevalues = NULL
thesevalues = obtainLastMinuteValues(matrix(ncol=2, data=c(682, 1437, 481, 2102)))
thesevalues = rbind(thesevalues, obtainLastMinuteValues(matrix(ncol=2, data=c(67, 145, 50, 207))))
thesevalues = rbind(thesevalues, obtainLastMinuteValues(matrix(ncol=2, data=c(812, 1437, 667, 2102))))
thesevalues = rbind(thesevalues, obtainLastMinuteValues(matrix(ncol=2, data=c(78, 145, 72, 207))))
thesevalues[,2] = p.adjust(thesevalues[,2])

cat('\n%> Final ChiSq values\n', file=thisfile, sep='', append=TRUE)
cat(paste('\\newcommand{\\ChiSqStat', c('AX','AY','BX','BY'), '}{', sprintf("%.2f", thesevalues[,1]), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)
cat(paste('\\newcommand{\\ChiSqPVal', c('AX','AY','BX','BY'), '}{', cf.asPValue(thesevalues[,2], digits=5), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)

#> Time analysis
timean = kruskal.test(formula = Time ~ Treatment, data = folksWhoDidntKnow)
cat(paste('\\newcommand{\\TimeKruskalStat}{', round(timean$statistic, digits=2), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)
cat(paste('\\newcommand{\\TimeKruskalPValue}{', round(timean$p.value, digits=4), "}\n", sep=''), file='../results_macros.tex', sep='', append=TRUE)

#> Cleaning
rm(i,mat,nicework,treatment.pairs,tst,thisname,thisfile,thesevalues,timean)
rm(proceed.chi2.pvalues,surrogate.chi2.pvalues,proceed.chi2.pvalues.corrected,surrogate.chi2.pvalues.corrected)
rm(proceed.mw.pvalues,proceed.mw.pvalues.corrected,surrogate.mw.pvalues,surrogate.mw.pvalues.corrected)
rm(proceed.chi2.stats, surrogate.chi2.stats)
