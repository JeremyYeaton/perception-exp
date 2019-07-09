library(dplyr)

# where your experiment is hosted
# removing initial "http://" and tildes
# and substituting slashes, etc. with periods
# no hyphens/dashes, alas (known bug)
# http://eden.rutgers.edu/~mil26/neg_perception/web/

#experigen.sourceURL = "www.awesomeuniversity.edu.iamawesome.questionnaire"
experigen.sourceURL1 = "eden.rutgers.edu.mil26.negperception1.web"
experigen.sourceURL2 = "eden.rutgers.edu.mil26.negperception2.web"
# the following information comes from your settings.js file:
experigen.experimentName = "pilot"
#experigen.experimentName = "French Negative Intonation Project"
experigen.database = "http://db.phonologist.org/"

# first, send some info to the server with the current 
# sourceURL and experimentName by submitting at least one screen
# to the server.
# otherwise, the server will return an error message

# check for usage of the experiment (number of page views per participant)
experigen.users1  =  paste(experigen.database, "users.cgi?experimentName=", experigen.experimentName, "&sourceurl=", experigen.sourceURL1, sep="")
experigen.users2  =  paste(experigen.database, "users.cgi?experimentName=", experigen.experimentName, "&sourceurl=", experigen.sourceURL2, sep="")
ex1 <- read.csv(experigen.users1, sep="\t") %>%
  filter(records>30 & userCode != 'EQH1')
ex2 <- read.csv(experigen.users2, sep="\t") %>%
  filter(records>30 & userCode != 'EQH1')

ex <- rbind(ex1,ex2) 
write.csv(ex,'users.csv')

# read the experimental results from the server
experigen.url1  =  paste(experigen.database, "makecsv.cgi?experimentName=", experigen.experimentName, "&sourceurl=", experigen.sourceURL1, sep="")
xp1  = read.csv(experigen.url1, sep="\t")
xp1$time = as.POSIXct(strptime(as.character(xp1$time), "%a %b %d %H:%M:%S %Y"))
meta1 = read.csv(paste(experigen.url1, "&file=demographics.csv", sep=""), sep="\t")
meta1$time = as.POSIXct(strptime(as.character(meta1$time), "%a %b %d %H:%M:%S %Y"))

experigen.url2  =  paste(experigen.database, "makecsv.cgi?experimentName=", experigen.experimentName, "&sourceurl=", experigen.sourceURL2, sep="")
xp2  = read.csv(experigen.url2, sep="\t")
xp2$time = as.POSIXct(strptime(as.character(xp2$time), "%a %b %d %H:%M:%S %Y"))
meta2 = read.csv(paste(experigen.url2, "&file=demographics.csv", sep=""), sep="\t")
meta2$time = as.POSIXct(strptime(as.character(meta2$time), "%a %b %d %H:%M:%S %Y"))

xp <- rbind(xp1,xp2) %>%
  filter(userCode != 'EQH1')

meta <- rbind(meta1,meta2) %>%
  filter(userCode != 'EQH1')

# assuming all went well, write to disk
# so that the results are saved even after the database server is gone
# it would be unwise not to keep a local copy of your results
write.csv(xp, "xp.csv")
write.csv(meta, "meta.csv")

# optional cleanup: remove all variables that begin with "experigen."
rm(list=ls(pattern="^experigen."))
rm(meta1,meta2,xp1,xp2,ex1,ex2)
