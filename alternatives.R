library(pbapply); library(tau)
# produce ngrams
ngram <- function(cleaned){
    # generate unigram
    count <- textcnt(cleaned, split = "[[:space:]]+", method = "string", n = 1L, decreasing = T, useBytes=T)
    unigram <<- data.table(names(count), N = as.vector(count))
    setorder(unigram, -N)
    setkey(unigram, V1)
    unigram <<- unigram[V1!="_START_"&V1!="_STOP_"]

    # generate bigram
    count <- textcnt(cleaned, split = "[[:space:]]+", method = "string", n = 2L, decreasing = T, useBytes=T)
        bigram <<- data.table(t(sapply(stri_split(names(count), fixed = " "), c)), N = as.vector(count))
    setorder(bigram, -N)
    setkey(bigram, V1)
    bigram <<- bigram[V1!="_START_"&V1!="_STOP_"&V2!="_START_"&V2!="_STOP_"]

    # generate trigram
    count <- textcnt(cleaned, split = "[[:space:]]+", method = "string", n = 3L, decreasing = T, useBytes=T)
    trigram <<- data.table(t(sapply(stri_split(names(count), fixed = " "), c)), N = as.vector(count))
    setorder(trigram, -N)
    setkey(trigram, V1, V2)
    trigram <<- trigram[V1!="_START_"&V1!="_STOP_"&V2!="_START_"&V2!="_STOP_"&V3!="_START_"&V3!="_STOP_"]
    # generate fourgram
    count <- textcnt(cleaned, split = "[[:space:]]+", method = "string", n = 4L, decreasing = T, useBytes=T)
    fourgram <<- data.table(t(sapply(stri_split(names(count), fixed = " "), c)), N = as.vector(count))
    setorder(fourgram, -N)
    setkey(fourgram, V1, V2, V3)
    fourgram <<- fourgram[V1!="_START_"&V1!="_STOP_"&V2!="_START_"&V2!="_STOP_"&V3!="_START_"&V3!="_STOP_"&V4!="_START_"&V4!="_STOP_"]
}


# build ngrams
system.time(ngram(cleaned))

#ngram <- function (list, n = 1){

uni <- function(phrase){
        # if(length(phrase)-2 < n){
        #   return("")
        # } else if (n == 1){
        # with start/stop for(i in 2:(length(phrase)-1)){
        for(i in 1:length(phrase)){
            lower <- tolower(phrase[i])
            # print(paste(phrase[i],"i=", i, "length=", length(phrase)))
            #unigram <<- rbindlist(list(unigram, list(lower,1)))
            if(dim(unigram[n1==lower])[1]==0){
                unigram <<- rbindlist(list(unigram, list(lower,1)))
                #setkey(unigram, n1)
            } else {
                unigram[n1==lower]$value <<- unigram[n1==lower]$value +1
            }
        }
        return("")
    }


bi <- function(list){
    lapply(list, function(phrase){
        for(i in 1:(length(phrase)-n+1)){
            lower <- tolower(phrase[i:(i+1)])
            print(paste(lower,"i=", i))
            if(dim(bigram[n1==lower[1] & n2 == lower[2]])[1]==0){
                bigram <<- rbindlist(list(bigram, list(lower[1], lower[2], 1)))
                #setkey(bigram, c(n1,n2))
            } else {
                bigram[n1 == lower[1] & n2 == lower[2]]$value <<- bigram[n1 == lower[1] & n2 == lower[2]]$value +1
            }
        }
    })
}

tri <- function(list){
    lapply(list, function(phrase){
        for(i in 1:(length(phrase)-n+1)){
            lower <- tolower(phrase[i:(i+2)])
            #print(paste(lower,"i=", i))
            if(dim(trigram[n1==lower[1] & n2 == lower[2] & n3 == lower[3]])[1]==0){
                trigram <<- rbindlist(list(trigram, list(lower[1], lower[2], lower[3], 1)))
                #setkey(trigram, c(n1,n2))
            } else {
                trigram[n1==lower[1] & n2 == lower[2] & n3 == lower[3]]$value <<- trigram[n1==lower[1] & n2 == lower[2] & n3 == lower[3]]$value +1
            }
        }
    })
    return("DONE")
}



# combine

unigram1 <- readRDS("ngrams/unigram1.Rds")
bigram1 <- readRDS("ngrams/bigram1.Rds")
trigram1 <- readRDS("ngrams/trigram1.Rds")
fourgram1 <- readRDS("ngrams/fourgram1.Rds")
for(i in 2:10){
    unigram1 <- merge(unigram1, readRDS(paste0("ngrams/unigram",i,".Rds")),suffixes=c(i-1,i), all = TRUE, allow.cartesian=TRUE)
    bigram1 <- merge(bigram1, readRDS(paste0("ngrams/bigram",i,".Rds")),suffixes=c(i-1,i), all = TRUE, allow.cartesian=TRUE)
    #trigram1 <- merge(trigram1, readRDS(paste0("ngrams/trigram",i,".Rds")),suffixes=c(i-1,i), all = TRUE, allow.cartesian=TRUE)
    #fourgram1 <- merge(fourgram1, readRDS(paste0("ngrams/fourgram",i,".Rds")),suffixes=c(i-1,i), all = TRUE, allow.cartesian=TRUE)
}



# combine <- function(dt){
#     dt[is.na(dt)]<-0
#     dt[, N:=N1+N2+N3+N4+N5+N6+N7+N8+N9+N10]
#     dt[, 2:11:=NULL]
# }

# unigram1 <- combine(unigram1)
# bigram1 <- combine(bigram1)
# trigram1 <- combine(trigram1)
# fourgram1 <- combine(fourgram1)