#> ------------------------------------------------------------------------------
#> Project: Mood paper
#> ------------------------------------------------------------------------------

#> Change this to whatever is the root of the instance. WITH trailing slash.
clio.root = "../"

source('config.r')
source('graphs.r')
source('functions.r')
ms.startLogFile()

source('process.data.r')
#source('analysis.r')
source('generateOutput.r')




for (i in surrogate$pid) {
	allfolks$concerned[ allfolks$pid==i ] = surrogate$concern[ surrogate$pid==i ]
}