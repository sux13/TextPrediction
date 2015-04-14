setwd("~/Documents/Capstone")
source("milestone.R")


twitter.all <- readLines("final/en_US//en_US.twitter.txt", encoding = "UTF-8", skipNul = T)
blogs.all <- readLines("final/en_US//en_US.blogs.txt", encoding = "UTF-8", skipNul = T)
news.all <- readLines("final/en_US//en_US.news.txt", encoding = "UTF-8", skipNul = T)


index <- sample(1:length(cleaned), ceiling(.1*length(cleaned)))
train.set <- cleaned[-index]
test.set <- cleaned[index]

# cleaned <- wrap(clean(c(twitter.all[train], blogs.all[train], news.all[train])))
ngram.offset(train.set)

results <- test(cleaned[1:200])
results.three <- test.three(cleaned[1:200])


unigram <- unigram[N!=1]
bigram <- bigram[N!=1]
trigram <- trigram[N!=1]
fourgram <- fourgram[N!=1]


results.minus1 <- test(cleaned[1:200])
results.three.minus1 <- test.three(cleaned[1:200])


unigram <- unigram[N!=2]
bigram <- bigram[N!=2]
trigram <- trigram[N!=2]
fourgram <- fourgram[N!=2]


results.minus2 <- test(cleaned[1:200])
results.three.minus2 <- test.three(cleaned[1:200])

unigram <- unigram[N!=3]
bigram <- bigram[N!=3]
trigram <- trigram[N!=3]
fourgram <- fourgram[N!=3]

results.minus3 <- test(cleaned[1:200])
results.three.minus3 <- test.three(cleaned[1:200])


rbind(all = c(four=mean(as.logical(results$correct)), three=mean(as.logical(results.three$correct))),
    minus1 = c(mean(as.logical(results.minus1$correct)), mean(as.logical(results.three.minus1$correct))),
    minus2 = c(mean(as.logical(results.minus2$correct)), mean(as.logical(results.three.minus2$correct))),
    minus3 = c(mean(as.logical(results.minus3$correct)), mean(as.logical(results.three.minus3$correct))))

