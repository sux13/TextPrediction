library(stringi); library(data.table)
setwd("/Volumes/Documents, Files, & Media/GitHub/TextPrediction")
source("milestone.R")

# # test data
twitter <- readLines("twitterTrain.txt", encoding = "UTF-8", skipNul = T)
blogs <- readLines("blogsTrain.txt", encoding = "UTF-8", skipNul = T)
news <- readLines("newsTrain.txt", encoding = "UTF-8", skipNul = T)
# twitter <- readLines("final/en_US//en_US.twitter.txt", encoding = "UTF-8", skipNul = T)
# blogs <- readLines("final/en_US//en_US.blogs.txt", encoding = "UTF-8", skipNul = T)
# news <- readLines("final/en_US//en_US.news.txt", encoding = "UTF-8", skipNul = T)

# clean data
cleaned <<- wrap(clean(c(twitter, news, blogs)))
#cleaned <<- wrap(clean(twitter[1:20000]))


index <- sample(1:length(cleaned), ceiling(.10*length(cleaned)))
train.set <- cleaned[-index]
test.set <- cleaned[index]

# cleaned <- wrap(clean(c(twitter.all[train], blogs.all[train], news.all[train])))
# ngram.offset(train.set)

k = 50
#unlink("ngrams", recursive=T)
if(divide.conquer(train.set, k)){
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

# unlink("finalngrams", recursive = T)
dir.create("finalngrams")
saveRDS(unigram, "finalngrams//unigram.Rds")
saveRDS(bigram, "finalngrams//bigram.Rds")
saveRDS(trigram, "finalngrams//trigram.Rds")
saveRDS(fourgram, "finalngrams//fourgram.Rds")

results <- test(test.set[1:200])

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


results2 <- test(test.set[1:200])
results3 <- test.interpolate(test.set[1:200])

bigram <- bigram[N!=1]
trigram <- trigram[N!=1]
fourgram <- fourgram[N!=1]



results <- test(test.set[1:200])
# results.three <- test.three(test.set[1:200])


unigram <- unigram[N!=1]
bigram <- bigram[N!=1]
trigram <- trigram[N!=1]
fourgram <- fourgram[N!=1]


results.minus1 <- test(test.set[1:200])
# results.three.minus1 <- test.three(test.set[1:200])


unigram <- unigram[N!=2]
bigram <- bigram[N!=2]
trigram <- trigram[N!=2]
fourgram <- fourgram[N!=2]


results.minus2 <- test(test.set[1:200])
# results.three.minus2 <- test.three(test.set[1:200])

unigram <- unigram[N!=3]
bigram <- bigram[N!=3]
trigram <- trigram[N!=3]
fourgram <- fourgram[N!=3]

results.minus3 <- test(test.set[1:200])
# results.three.minus3 <- test.three(test.set[1:200])


rbind(all = c(four=mean(as.logical(results$correct)),
			 three=mean(as.logical(results.three$correct))),
    minus1 = c(mean(as.logical(results.minus1$correct)),
    		   mean(as.logical(results.three.minus1$correct))),
    minus2 = c(mean(as.logical(results.minus2$correct)),
    		   mean(as.logical(results.three.minus2$correct))),
    minus3 = c(mean(as.logical(results.minus3$correct)),
    		   mean(as.logical(results.three.minus3$correct))))

