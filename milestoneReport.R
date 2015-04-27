# parallel processing
doMC::registerDoMC(4)
# load stringi package
library(stringi)
# read data
twitter <- readLines("final/en_US//en_US.twitter.txt", encoding = "UTF-8", skipNul = T)

# expand appreviations
## gsub("'t", " not", twitter[1:100])
## aren't
## isn't
## I'm
## They're
## he's
## she's
## don't
## doesn't

# remove all extraneous characters
#cleaned <- gsub("[^ a-zA-Z0-9!\"#$%&'()*+,-./:;<=>?@^_`{|}~\\]|\"|\\[|\\]", "", twitter)
cleaned3 <- stri_replace_all(regex = "[^ a-zA-Z0-9!\"#$%&'\\()*+,-./:;<=>?@^_`{}|~]|\"|\\[|\\]", replacement = "", news)

# examine number data
#table(stri_extract_all(cleaned[1:50000], regex = " [0-9]+(.*?) ", simplify = T))

# date
#table(stri_extract_all(cleaned[1:500000], regex = " ([0-1][1-2]|[1-9])[-/]([0-3][0-9]|[1-9])([-/]([0-9]{4}|[0-9]{2}))? ", omit_no_match = T, simplify = T))
#cleaned <- gsub(" ([0-1][1-2]|[1-9])[-/]([0-3][0-9]|[1-9])([-/]([0-9]{4}|[0-9]{2}))? ", " <DATE> ", cleaned)
cleaned3 <- stri_replace_all(regex = " ([0-1][1-2]|[1-9])[-/]([0-3][0-9]|[1-9])([-/]([0-9]{4}|[0-9]{2}))? ", replacement = " <DATE> ", cleaned3)

# time
#table(stri_extract_all(cleaned[1:50000], regex = " [0-2]?[0-9][:-][0-6]?[0-9]([AaPpMm.]*)? ", omit_no_match = T, simplify = T))
#cleaned <- gsub(" [0-2]?[0-9][:-][0-6]?[0-9]([AaPpMm.]*)? ", " <TIME> ", cleaned3)
cleaned3 <- stri_replace_all(regex =" [0-2]?[0-9][:-][0-6]?[0-9]([AaPpMm.]*)? ", replacement =" <TIME> ", cleaned3)

# replace numbers
#table(stri_extract_all(cleaned[1:50000], regex = " [$]?[0-9,]+[.]?[0-9]+(%|th|st|nd)? ", omit_no_match = T, simplify = T))
#cleaned <- gsub(" [$]?[0-9,]+[.]?([0-9]+)?(%|th|st|nd)? ", " <NUM> ", cleaned3)
cleaned3 <- stri_replace_all(regex =" [$]?[0-9,]+[.]?([0-9]+)?(%|th|st|nd)? ",replacement= " <NUM> ", cleaned3)

# replace phone number
#table(stri_extract_all(cleaned[1:200000], regex="1?[\\-.(]?[0123456789]{3}[\\-./)]?[0-9]{3}[/\\-.]?[0-9]{4}", omit_no_match=T, simplify=T))
#cleaned <- gsub("1?[-.(]?[0123456789]{3}[-.)]?[0-9]{3}[-.]?[0-9]{4}", " <PHONE> ", cleaned3)
cleaned3 <- stri_replace_all(regex =" 1?[-.(]?[0123456789]{3}[-.)]?[0-9]{3}[-.]?[0-9]{4} ", replacement= " <PHONE> ", cleaned3)


# emoji
#table(stri_extract_all(cleaned[1:100000], regex=" [<>0O%]?[xX:;=8]([-o*']+)?([()dDpP/:}{@|cC]|\\[|\\])+|([()dDpP/:}{@|cC]|\\[|\\])([-o*']+)?[xX:;=8][<>0O%]?", omit_no_match=T, simplify=T))
#cleaned <- gsub(" [<>0O%]?[xX:;=8]([-o*']+)?([()dDpP/:}{@|cC]|\\[|\\])+|([()dDpP/:}{@|cC]|\\[|\\])([-o*']+)?[xX:;=8][<>0O%]? ", " <EMOJI> ", cleaned3)
cleaned3 <- stri_replace_all(regex =" [<>0O%]?[xX:;=8-]([-o*'_]+)?([-()dDpP/:}{@|cC]|\\[|\\])+|([()dDpP/:}{@|cC]|\\[|\\])([-o*']+)?[xX:;=8][<>0O%]?|<3|</3", replacement = " <EMOJI> ", cleaned3)

# replace ... !? or : with new lines
#cleaned <- gsub("[.]{2,50}|[!?,]+ |[ ]?: |([ a-zA-Z0-9]{3})[.] |([^a-zA-Z0-9>]*)$", "\\1<BREAK>", cleaned3)
cleaned3 <- stri_replace_all(regex ="[.]{2,50}|[!?,]+ |[ ]?: |([ a-zA-Z0-9]{3})[.] |([^a-zA-Z0-9>]*)$", replacement = "$1<BREAK>", cleaned3)

# split up into phrases
cleaned3 <- unlist(stri_split(cleaned3, fixed = "<BREAK>", omit_empty = T))

# tabulate word frequencies 
table3 <- as.data.frame(table(tolower(unlist(stri_split(cleaned3, fixed=" ", omit_empty = T)))), stringsAsFactors=F)

# sort by frequency
table3 <- table3[order(-table3$Freq),]


plot(factor(table$Var1[1:10], levels=table$Var1[1:10]), table$Freq[1:10])



gsub("[^a-zA-Z]+$", "<STOP>", twitter[1:100])
strsplit(twitter[1:100], "[!?]+ ")
strsplit(paste("<START>", twitter[1:100], "<STOP>"), " ")

