library(stringi); library(data.table); library(tau)
options("scipen"=100, "digits"=4)
# setwd("~/Documents/Capstone")

# # list stop words
# stopwords <- readLines(system.file("stopwords", "english.dat",package = "tm"))

clean <- function(corpus){
    # replace any character that is not alphanumeric or symbols
    ##cleaned <- stri_replace_all(tolower(corpus), regex = "[^ a-zA-Z0-9!#$%'\\(),-./:;=?&@_`{}^|~\\[\\]]|\"", replacement = "")
    cleaned <- stri_replace_all(tolower(corpus), regex = "[^ a-zA-Z0-9!',\\-./:;?@()]+|\"", replacement = "")
    # cleaned <- remove_stopwords(cleaned, stopwords, lines = T)
    cleaned <- stri_replace_all(cleaned, regex = "'ve\\b", replacement = " have")
    cleaned <- stri_replace_all(cleaned, regex = "'re\\b", replacement = " are")
    cleaned <- stri_replace_all(cleaned, regex = "n't\\b", replacement = " not")
    cleaned <- stri_replace_all(cleaned, regex = "'ll\\b", replacement = " will")
    cleaned <- stri_replace_all(cleaned, regex = "'d\\b", replacement = " would")
    cleaned <- stri_replace_all(cleaned, regex = "\\b(i'm|im)\\b", replacement = "i am")
    # cleaned <- stri_replace_all(cleaned, regex = "\\b(isnt)\\b", replacement = "is not")
    cleaned <- stri_replace_all(cleaned, regex = "\\b(he|she|it|how|that|there|what|when|who|why|where)'?s\\b", replacement = "$1 is")
    # cleaned <- stri_replace_all(cleaned, regex = "\\b(u|ur|i|me|my|myself|we|our|ours|ourselves|you|your|yours|yourself|yourselves|he|him|his|himself|she|her|hers|herself|it|its|itself|they|them|their|theirs|themselves|us)\\b|^(u|ur|i|me|my|myself|we|our|ours|ourselves|you|your|yours|yourself|yourselves|he|him|his|himself|she|her|hers|herself|it|its|itself|they|them|their|theirs|themselves|us)\\b|\\b(u|ur|i|me|my|myself|we|our|ours|ourselves|you|your|yours|yourself|yourselves|he|him|his|himself|she|her|hers|herself|it|its|itself|they|them|their|theirs|themselves|us)$", replacement = "<pronoun>")
    # cleaned <- stri_replace_all(cleaned, regex = "\\b(am|is|are|was|were|be|been|being|will be)\\b", replacement = "<be>")
    # cleaned <- stri_replace_all(cleaned, regex = "\\b(have|has|had|having)\\b", replacement = "<have>")
    # cleaned <- stri_replace_all(cleaned, regex = "\\b(do|does|did|doing)\\b", replacement = "<do>")
    cleaned <- stri_replace_all(cleaned, regex = "\\b(\\w*f+u+c+k+\\w*|\\w*b+i+t+c+h+\\w*|\\w*s+h+i+t+\\w*|\\w*c+u+n+t+\\w*|\\w*f+a+g+g+o+t\\w*|t+w+a+t+|\\w*a+s+s+h+o+l+e+\\w*|\\w*n+i+g+g+e+r+\\w*)\\b", replacement = " <profanity> ")
    #cleaned <- stri_replace_all(cleaned, regex = " a|an|the|and|of|this|that|these|those ", replacement = "")
    # find and replace email data
    cleaned <- stri_replace_all(cleaned, regex = " ?([\\w\\d_.-]+)@([\\w\\d.-]+)\\.([\\w.]{2,6}) ?", replacement = " <email> ")
    # find and replace url
    cleaned <- stri_replace_all(cleaned, regex =" ?(https?:/+)?(([\\w\\d]+)\\.){1,4}\\w{2,4}[/\\w-!=?@_\\d]* ?", replacement = " <url> ", simplify = T, omit_no_match = T)
    # find and replace emoji data
    cleaned <- stri_replace_all(cleaned, regex = "\\b[<>0O%]?[:;=8]([-o*']+)?[()dp/ocs]+\\b|\\b[()dp/}{#|c]+([-o*']+)?[:;=8][<>]?\\b|<+3+|<+/+3+|[-o0><^][_.]+[-o0><^]\\b|\\b[<>0O%]?[:;=8]([-o*']+)?[()dp/ocs]+\\b|\\b[()dp/}{#|c]+([-o*']+)?[:;=8][<>]?\\b|<+3+|<+/+3+|[-o0><^][_.]+[-o0><^]$", replacement = " <emoticon> ")
    # find and replace date data
    cleaned <- stri_replace_all(cleaned, regex = " ?([0-1][1-2]|[1-9])[/]([0-2][0-9]|3[01]|[1-9])([/]([0-9]{4}|[0-9]{2}))?|[1-9]0s|[0-9]{4}s ?", replacement = " <date> ")
    # find and replace time data
    cleaned <- stri_replace_all(cleaned, regex ="\\b(([0-1]?[0-9]|2[1-4])([:][0-5]?[0-9]){1,2} ?([ap][. ]?[m][. ]?)? ?)|\\b([0-1]?[0-9]|2[1-4])(-\\d+)? ?[ap][. ]?[m][. ]?\\b", replacement =" <time> ")
    # find and replace phone data
    cleaned <- stri_replace_all(cleaned, regex ="\\b[ :-]1?[-(]?[\\d]{3}[-.)]?[\\d]{3}[-.]?[\\d]{4}\\b ?", replacement= " <phone> ")
    # remove extraneous punctuation
    cleaned <- stri_replace_all(cleaned, regex = "(?<=[!/,.:;?#])[!,.:;#?]+|'+", replacement = "")
    # break lines into sentences
    cleaned <- unlist(stri_split(cleaned, regex = " ?[!?;:.]+ +", omit_empty=T))
    #cleaned <- unlist(stri_split_boundaries(cleaned, type = "sentence"))
    cleaned <- stri_replace_all(cleaned, regex = "[^ a-zA-Z0-9<>#]+", replacement = " ")
    #cleaned <- stri_replace_all(cleaned, regex = "[-/]+", replacement = " ")
    # find and replace number data
    cleaned <- stri_replace_all(cleaned, regex ="\\b[\\d]+([-,.\\d]+)?(th|st|nd|rd)?", replacement= " <num> ")
    ##cleaned <- stri_replace_all(cleaned, regex =" ?[$]?([0-9,]+)?([0-9]+|[0-9]+[.-][0-9]+|[.][0-9]+)(%|th|st|nd)? ",replacement= " _NUM_ ")
    # remove leading/trailing/extraneou space nad apostrophe
    cleaned <- stri_replace_all(cleaned, regex = "^ +|(?<= ) +| +$", replacement = "")
    # break sentences into words
    ###cleaned <- stri_split(cleaned, regex = "[[:space:]]+", omit_empty = T)
    ###cleaned <- stri_split_boundaries(cleaned, type = "word", skip_word_none=TRUE)
    # drop empty elements
    cleaned <- cleaned[lapply(cleaned, nchar)>0]
    # add start and stop
    return(cleaned)
}

wrap <- function(list){
    return(paste("<s>", list, "</s>"))
}


# generate ngrams by offset

ngram.offset <- function(cleaned){
    # split words
    tokens <- unlist(stri_split(cleaned, regex = "[[:space:]]+", omit_empty = T))

    # generate unigram
    unigram <- as.data.table(table(tokens)) #[N>1]
    # setorder(unigram, -N)
    setnames(unigram,c("word", "N"))
    setkey(unigram, word)
    # unigram <<- unigram[word!="<s>"&word!="</s>"]
    unigram <<- unigram[word!="</s>"]


    # generate bigram
    table <- table(paste(c("NA", tokens), c(tokens, "NA"))[2:length(tokens)])
    bigram <<- data.table(t(sapply(stri_split(names(table), fixed = " "), c)), N = as.vector(table))
    # setorder(bigram, -N)
    setnames(bigram,c("word1", "word2", "N"))
    setkey(bigram, word1, word2)
    # bigram <<- bigram[word1!="<s>"&word1!="</s>"&word2!="<s>"&word2!="</s>"]
    bigram <<- bigram[word1!="</s>"&word2!="<s>"&word2!="</s>"]

    # generate trigram
    table <- table(paste(c("NA", "NA", tokens), c("NA", tokens, "NA"), c(tokens, "NA", "NA"))[3:length(tokens)])
    trigram <<- data.table(t(sapply(stri_split(names(table), fixed = " "), c)), N = as.vector(table))
    # setorder(trigram, -N)
    setnames(trigram,c("word1", "word2", "word3", "N"))
    setkey(trigram, word1, word2, word3)
    # trigram <<- trigram[word1!="<s>"&word1!="</s>"&word2!="<s>"&word2!="</s>"&word3!="<s>"&word3!="</s>"]
    trigram <<- trigram[word1!="</s>"&word2!="<s>"&word2!="</s>"&word3!="<s>"&word3!="</s>"]

    # generate fourgram
    table <- table(paste(c("NA", "NA", "NA", tokens), c("NA", "NA", tokens, "NA"), c("NA", tokens, "NA", "NA"), c(tokens, "NA", "NA", "NA"))[4:length(tokens)])
    fourgram <<- data.table(t(sapply(stri_split(names(table), fixed = " "), c)), N = as.vector(table))
    # setorder(fourgram, -N)
    setnames(fourgram,c("word1", "word2", "word3", "word4", "N"))
    setkey(fourgram, word1, word2, word3, word4)
    # fourgram <<- fourgram[word1!="<s>"&word1!="</s>"&word2!="<s>"&word2!="</s>"&word3!="<s>"&word3!="</s>"&word4!="<s>"&word4!="</s>"]
    fourgram <<- fourgram[word1!="</s>"&word2!="<s>"&word2!="</s>"&word3!="<s>"&word3!="</s>"&word4!="<s>"&word4!="</s>"]
}



divide.conquer <- function(cleaned, k = 10){
    unlink("ngrams", recursive = T)
    dir.create("ngrams")
    index <- split(1:length(cleaned), 1:length(cleaned)%%k)
    for(i in 1:k){
        print(paste(Sys.time(), "---- cycle", i))
        ngram.offset(cleaned[index[[i]]])
        saveRDS(unigram, paste0("ngrams/unigram",i,".Rds"))
        saveRDS(bigram, paste0("ngrams/bigram",i,".Rds"))
        saveRDS(trigram, paste0("ngrams/trigram",i,".Rds"))
        saveRDS(fourgram, paste0("ngrams/fourgram",i,".Rds"))
        #rm(unigram,bigram, trigram, fourgram)
    }
    return(TRUE)
}


combine <- function(ngram, k = 10){
    return(lapply(1:k, function(i) readRDS(paste0("ngrams/",ngram,i,".Rds"))))
}



# divide the data source and split into different files
k = 10
#unlink("ngrams", recursive=T)
if(divide.conquer(cleaned, k)){
    # rm(unigram, bigram, trigram, fourgram); gc()
    print("loading and merging unigrams...")
    unigram <- rbindlist(combine("unigram", k))
    unigram <- unigram[, sum(N), by = "word"]
    setnames(unigram, "V1", "N")
    # unigram[, p:=N/sum(N)]
    # setorder(unigram, -N)
    setkey(unigram, word)
    print("loading and merging bigrams...")
    bigram <- rbindlist(combine("bigram", k))
    bigram <- bigram[, sum(N), by = "word1,word2"]
    setnames(bigram, "V1", "N")
    # bigram[, p:=N/sum(N)]
    # setorder(bigram, -N)
    setkey(bigram, word1)
    print("loading and merging trigrams...")
    trigram <- rbindlist(combine("trigram", k))
    trigram <- trigram[, sum(N), by = "word1,word2,word3"]
    setnames(trigram, "V1", "N")
    # trigram[, p:=N/sum(N)]
    # setorder(trigram, -N)
    setkey(trigram, word1, word2)
    print("loading and merging fourgrams...")
    fourgram <- rbindlist(combine("fourgram", k))
    fourgram <- fourgram[, sum(N), by = "word1,word2,word3,word4"]
    setnames(fourgram, "V1", "N")
    # fourgram[, p:=N/sum(N)]
    # setorder(fourgram, -N)
    setkey(fourgram, word1, word2, word3)
    print("DONE")
}

unigram[N==1, word:="<unk>"]
unigram <- unigram[, sum(N), by = "word"]
# setnames(unigram, "V1", "N")
# unigram[, p:=N/sum(N)]
# setorder(unigram, -N)
setkey(unigram, word)
bigram[!(word1 %in% unigram$word), word1:="<unk>"]
bigram[!(word2 %in% unigram$word), word2:="<unk>"]
bigram <- bigram[, sum(N), by="word1,word2"]
setkey(bigram, word1, word2)

trigram[!(word1 %in% unigram$word), word1:="<unk>"]
trigram[!(word2 %in% unigram$word), word2:="<unk>"]
trigram[!(word3 %in% unigram$word), word3:="<unk>"]
trigram <- trigram[, sum(N), by = "word1,word2,word3"]
setkey(trigram, word1, word2, word3)

fourgram[!(word1 %in% unigram$word), word1:="<unk>"]
fourgram[!(word2 %in% unigram$word), word2:="<unk>"]
fourgram[!(word3 %in% unigram$word), word3:="<unk>"]
fourgram[!(word4 %in% unigram$word), word4:="<unk>"]
fourgram <- fourgram[, sum(N), by = "word1,word2,word3,word4"]
setkey(fourgram, word1, word2, word3, word4)

setnames(unigram, "V1", "N")
setnames(bigram, "V1", "N")
setnames(trigram, "V1", "N")
setnames(fourgram, "V1", "N")

bigram <- bigram[N!=1]
trigram <- trigram[N!=1]
fourgram <- fourgram[N!=1]


# unlink("/Volumes//Documents, Files, & Media/Coursera/finalngrams", recursive = T)
# dir.create("/Volumes//Documents, Files, & Media/Coursera/finalngrams")
# saveRDS(unigram, "/Volumes//Documents, Files, & Media/Coursera/finalngrams//unigram.Rds")
# saveRDS(bigram, "/Volumes//Documents, Files, & Media/Coursera/finalngrams//bigram.Rds")
# saveRDS(trigram, "/Volumes//Documents, Files, & Media/Coursera/finalngrams//trigram.Rds")
# saveRDS(fourgram, "/Volumes//Documents, Files, & Media/Coursera/finalngrams//fourgram.Rds")


# unigram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//unigram.Rds")
# bigram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//bigram.Rds")
# trigram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//trigram.Rds")
# fourgram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//fourgram.Rds")

## Parse cleaned data
parse <- function(string){
    return(unlist(stri_split(string, regex = "[[:space:]]+", omit_empty = T)))
    # return(unlist(strsplit(string, "[[:space:]]+")))
}


# unigram <- unigram[N!=1]
# bigram <- bigram[N!=1]
# trigram <- trigram[N!=1]
# fourgram <- fourgram[N!=1]

replaceUnknown <- function(parsedInput){
    unlist(lapply(parsedInput, function(i){
        if(!(i %in% unigram$word))
            "<unk>"
        else
            i
    }))
}


## Prediction
predict <- function(phrase, k=3){
    # phrase <<- parse(clean(input))
    phrase <- replaceUnknown(phrase)
    j <- length(phrase)
    # print(c(phrase))
    if(j == 1){
        result <- c("bigram" = bigram[phrase[j], nomatch=0][order(-N)]$word2[1:k],
                    "unigram" =unigram[order(-N)]$word[2:(k+1)])
    }else if (j == 2){
        result <- c("trigram" = trigram[J(phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word3[1:k],
                    "bigram" = bigram[phrase[j], nomatch=0][order(-N)]$word2[1:k],
                    "unigram" =unigram[order(-N)]$word[2:(k+1)])
    }else {
        result <- c("fourgram" =fourgram[J(phrase[j-2], phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word4[1:k],
                    "trigram" = trigram[J(phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word3[1:k],
                    "bigram" = bigram[phrase[j], nomatch=0][order(-N)]$word2[1:k],
                    "unigram" =unigram[order(-N)]$word[2:(k+1)])
    }
    return(result[!is.na(result)][1:k])
}

# predict.three <- function(phrase, k=3){
#     # phrase <<- parse(clean(input))
#     # phrase <- unlist(strsplit(input, " "))
#     j <- length(phrase)
#     # print(c(phrase))
#     setorder(unigram, -N)
#     if(j == 1){
#         result <- c("bigram" = bigram[phrase[j], nomatch=0]$word2[1:k], "unigram" =unigram$word[1:k])
#     }else {
#         result <- c("trigram" = trigram[J(phrase[j-1], phrase[j]), nomatch=0]$word3[1:k],
#                "bigram" = bigram[phrase[j], nomatch=0]$word2[1:k], "unigram" =unigram$word[1:k])
#     }
#     return(result[!is.na(result)][1:k])
# }


test <- function(raw){
    # results <- data.table()
    results <- data.table(actual = character(0), predicted1 = character(0),
        predicted2 = character(0), predicted3 = character(0), correct = logical(0))
    for(i in 1:length(raw)){
        processed <- parse(raw[i])
        for(j in 1:(length(processed)-1)){
            # print(processed[1:j])
            predicted <- predict(processed[1:j])
            correct <-{if(processed[j+1] %in% predicted) TRUE else FALSE}
            results <- rbindlist(list(results, as.list(c(processed[j+1], predicted,  correct))))

        }
    }
    return(results)

}
# test.three <- function(raw){
#     # results <- data.table()
#     results <- data.table(actual = character(0), predicted1 = character(0),
#         predicted2 = character(0), predicted3 = character(0), correct = logical(0))
#     for(i in 1:length(raw)){
#         processed <- parse(raw[i])
#         for(j in 1:(length(processed)-1)){
#             # print(processed[1:j])
#             predicted <- predict.three(processed[1:j])
#             correct <-{if(processed[j+1] %in% predicted) TRUE else FALSE}
#             results <- rbindlist(list(results, as.list(c(processed[j+1], predicted,  correct))))

#         }
#     }
#     return(results)

# }

test.one <- function(raw){
    # results <- data.table()
    results <- data.table(actual = character(0), predicted1 = character(0), correct = logical(0))
    for(i in 1:length(raw)){
        processed <- parse(raw[i])
        for(j in 1:(length(processed)-1)){
            # print(processed[1:j])
            predicted <- predict(processed[1:j], 1)
            correct <-{if(processed[j+1] %in% predicted) TRUE else FALSE}
            results <- rbindlist(list(results, as.list(c(processed[j+1], predicted, correct))))

        }
    }
    return(results)

}


interpolate <- function(phrase){
    j <- length(phrase)
    fourN <- fourgram[J(phrase[j-3], phrase[j-2], phrase[j-1], phrase[j])]$N
    triN <- trigram[J(phrase[j-2], phrase[j-1], phrase[j])]$N
    biN <- bigram[J(phrase[j-1], phrase[j])]$N
    uniN <- unigram[phrase[j]]$N

    # fourN/triN + triN/biN + biN/uniN + uniN/nrow(unigram)
    print(fourN)
    print(triN)
    print(biN)
    print(uniN)
    result <- c("fourgram" = fourN/triN,
               "trigram" = triN/biN,
               "bigram" = biN/uniN,
               "unigram" = uniN/sum(unigram$N))
    
    return(result)
}
# ## Prediction
# predict.word <- function(input, k=3){
#     phrase <<- c("<s>", unlist(stri_split(clean(input), regex = "[[:space:]]+", omit_empty = T)))
#     #phrase <- unlist(strsplit(input, " "))
#     j <- length(phrase)
#     print(c(phrase))

#     top.fourgram <- fourgram[J(phrase[j-2], phrase[j-1], phrase[j]), nomatch=0]
#     top.trigram <- trigram[J(phrase[j-1], phrase[j]), nomatch=0]
#     top.bigram <- bigram[phrase[j]]
#     unigram[, sum(.N), by=N]

#     if(j == 1){
#         result <- list(bigram[phrase[j]]$word2[1:k])
#     }else{
#         result <- list("2" = bigram[phrase[j], nomatch=0]$word2[1:k],
#                        "3" = trigram[J(phrase[j-1], phrase[j]), nomatch=0]$word3[1:k])
#                        "4" = fourgram[J(phrase[j-2], phrase[j-1], phrase[j]), nomatch=0]$word4[1:k])
#     }


#     return(result)
# }




