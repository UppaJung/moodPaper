#> ------------------------------------------------------------------------------
#> Project: Mood paper
#> ------------------------------------------------------------------------------

#> We process the logs so we display some useful data out
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
rm(wen)


#> ------------------------------------------------------------------------------------------
#> Finally, we create the output and display it
#> ------------------------------------------------------------------------------------------
library(knitr)
setwd(expandfile(path='output'))
knit(expandfile(path='output', file='index.Rhtml'), output=expandfile(path='output', file='index.html'))
setwd(expandfile(path='r'))
