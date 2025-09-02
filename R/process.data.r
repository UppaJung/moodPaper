#> ------------------------------------------------------------------------------------------
#> Project: Mood paper
#> Dataset file: cbravo@caprica:/mnt/files/PhD/Ethics/data/
#> ------------------------------------------------------------------------------------------

suffixes = c('BotnetSpam', 'Facebook', 'OSCredentialSpoofing', 'SocialPhishing', 'WarningsDeception')
treatments = matrix(ncol=2, byrow=TRUE, data=c(
	'Control',					'Control', #> F0
	'WithoutRemovingNegativePosts',	'Only remove\npositive posts', #> F1
	'WithoutRemovingPositivePosts',	'Only remove\nnegative posts', #> F2
	'WithoutPublication',			'Remove mention\nof publication', #> F3
	'WithoutImprovingProduct',		'Remove mention of\nproduct improvement', #> F4
	'NoAdvertising',				'Promise not to use\nfor advertising', #> F5
	'InsertPosts',				'Insert posts\ninstead of hiding', #> F6
	'InsertOnlyPostiveposts',		'Insert posts, and\nonly positive ones', #> F7
	'UnnamedCompany',				'Replace \'Facebook\'\nwith \'a social network\'', #> F8
	'Twitter',					'Replace \'Facebook\'\nwith \'Twitter\''  #> F9
))

#> -------------------------------
#> 1: Reading
#> -------------------------------

#> We read the (only) data file
allfolks = read.csv('../data/RawResults201407TDF.csv')

#> -------------------------------
#> 2: Checks and cleaning
#> -------------------------------
if (anyDuplicated(allfolks$workerId)) {
	stop(paste('The following Worker IDs are duplicated', allfolks$WorkerId[duplicated(allfolks$WorkerId)]))
}

#> We drop some columns that we don't need anymore
for (i in c('assignmentId'))
	allfolks[,c(i)] = NULL
rm(i)

#> -------------------------------
#> 3: Transformations & coding
#> -------------------------------
#> Transform into boolean
for (i in c('KnewAboutMoodStudy', paste('AwareThisStudy_', suffixes, sep=''), paste('PrevParticipated_', suffixes, sep='')))
	allfolks[,c(i)] = allfolks[,c(i)]=='yes'
rm(i)

#> Transform into factors
for (i in paste('Proceed_', suffixes, sep=''))
	allfolks[,c(i)] = factor(allfolks[,c(i)], levels=c('no', 'unsure', 'caution', 'yes'))
for (i in paste('Surrogate_', suffixes, sep=''))
	allfolks[,c(i)] = factor(allfolks[,c(i)], levels=c('no', 'indifferent', 'yes'))
rm(i)

#> Treatments!
allfolks$Treatment = factor(allfolks$Treatment, levels=treatments[,1])

#> We create a column with a non-identifiable id
allfolks$pid = paste('P', row.names(allfolks), sep='')
#folksWhoDidntKnow = allfolks[ !allfolks$KnewAboutMoodStudy, ]
#folksWhoKnew = allfolks[ allfolks$KnewAboutMoodStudy, ]

for (i in suffixes)
	for (j in c('Proceed', 'Surrogate')) {
		ms.write.csv(
			getFreeResponses(allfolks, paste('Xplain', j, '_', i, sep='')),
			filename=expandfile(file=paste('Xplain', j,'_', i, '.csv', sep=''))
		)
	}
rm(i,j)

for (j in c('Proceed', 'Surrogate')) {
	ms.write.csv(
		getFreeResponses(allfolks, paste('Xplain', j, '_', i, sep='')),
		filename=expandfile(file=paste('Xplain', j,'_', i, '.csv', sep=''))
	)
}
rm(j)



# #> ------------------------------------------------------------------------------------------
# #> Project: Mood paper
# #> Dataset file: cbravo@caprica:/mnt/files/PhD/Ethics/data/RawResults201408v2TDF.csv
# #> Codebook: cbravo@caprica:/mnt/files/PhD/Ethics/moodPaper/data/CodeBook-DataSet-201408-v2.txt
# #> ------------------------------------------------------------------------------------------
# 
# suffixes = c('_BotnetSpam', '_OSCredentialSpoofing', '_SecurityAdoption', '_SocialPhishing', '_WarningsDeception',
# 		 '_Censorship', '_Censorship_OptIn', '_Censorship_OptOutDelay', '_Censorship_OriginalDesign',
# 		 '_Facebook', '_FacebookMood_Control', '_FacebookMood_InsertOnlyPostiveposts', '_FacebookMood_InsertPosts',
# 		 '_OKCupid', '_OKCupid_NoAlternative', '_OKCupid_WithAlternative',
# 		 '_SearchDelay', '_SearchDelay_Bing', '_SearchDelay_UnspecifiedCompany')
# 
# #> -------------------------------
# #> 1: Reading
# #> -------------------------------
# 
# #> We read the (only) data file
# allfolks = read.csv('../../data/RawResults201408v2TDF.csv')
# 
# #> -------------------------------
# #> 2: Checks and cleaning
# #> -------------------------------
# if (anyDuplicated(allfolks$workerId)) {
# 	stop(paste('The following Worker IDs are duplicated', allfolks$WorkerId[duplicated(allfolks$WorkerId)]))
# }
# 
# #> We drop some columns that we don't need anymore
# for (i in c('RandomizeMainSections','assignmentId','RandomizeScenarios'))
# 	allfolks[,c(i)] = NULL
# rm(i)
# 
# #> -------------------------------
# #> 3: Transformations & coding
# #> -------------------------------
# #> Transform into boolean
# for (i in c('KnewAboutMoodStudy', 'KnewAboutOKCupid')) {
# 	allfolks[,c(i)] = allfolks[,c(i)]=='yes'
# }
# 
# #> Puts scales in order and transform them into factors
# for (i in paste('Proceed', suffixes, sep='')) {
# 	allfolks[,c(i)] = factor(allfolks[,c(i)], levels=c('no','caution','yes'))
# }
# 
# for (i in paste('Participate', suffixes, sep='')) {
# 	allfolks[,c(i)] = factor(allfolks[,c(i)], levels=c('no','indifferent','yes'))
# }
# 
# for (i in paste('AwareSimilarStudy', suffixes, sep='')) {
# 	allfolks[,c(i)] = allfolks[,c(i)]=='yes'
# }
# 
# for (i in paste('ParticipatedInPast', suffixes, sep='')) {
# 	allfolks[,c(i)] = allfolks[,c(i)]=='yes'
# }
# rm(i)
# 
# #> Treatments!
# allfolks$VariantCensorship = factor(allfolks$VariantCensorship, levels=c('OriginalDesign', 'OptIn', 'OptOutDelay'))
# allfolks$VariantMood = factor(allfolks$VariantMood, levels=c('Control', 'InsertOnlyPostiveposts', 'InsertPosts'))
# allfolks$VariantOKCupid = factor(allfolks$VariantOKCupid, levels=c('WithAlternative','NoAlternative'))
# allfolks$VariantSearchDelay = factor(allfolks$VariantSearchDelay, levels=c('Bing','UnspecifiedCompany'))
# 
# #> User agent string
# library(utils)
# for (i in 1:nrow(allfolks))
# 	allfolks$UserAgent[i] = URLdecode(allfolks$UserAgent[i])
# rm(i)
# 
# #> We write the user agent to be able to process it externally
# #> This is automatic. No need to code anything.
# write.csv(allfolks[,c('workerId','UserAgent')], file='DamnFile.csv', row.names=FALSE)
# system('./detect_browser DamnFile.csv workerId UserAgent > DamnFile2.csv')
# tmp = read.csv('DamnFile2.csv')
# allfolks = merge(allfolks, tmp)
# rm(tmp)
# file.remove('DamnFile.csv', 'DamnFile2.csv')
# 
# #> We create a column with a non-identifiable id
# allfolks$pid = paste('P', row.names(allfolks), sep='')
# 
# #> We create some convenient columns
# allfolks$UserAgent.browser = allfolks$Browser
# allfolks$Browser = NULL
# allfolks$UserAgent.OS = allfolks$OS
# allfolks$OS = NULL
# allfolks$UserAgent.engine = allfolks$Engine
# allfolks$Engine = NULL
# allfolks$UserAgent.browser.brand = 'Unknown'
# allfolks$UserAgent.browser.brand[grep("Chrome", allfolks$UserAgent.browser)] = "Chrome"
# allfolks$UserAgent.browser.brand[grep("Firefox", allfolks$UserAgent.browser)] = "Firefox"
# allfolks$UserAgent.browser.brand[grep("MSIE", allfolks$UserAgent.browser)] = "MSIE"
# allfolks$UserAgent.browser.brand[grep("Opera", allfolks$UserAgent.browser)] = "Opera"
# allfolks$UserAgent.browser.brand[grep("Safari", allfolks$UserAgent.browser)] = "Safari"
