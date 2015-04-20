parse <- function(string){
    return(unlist(stri_split(string, regex = "[[:space:]]+", omit_empty = T)))
    # return(unlist(strsplit(string, "[[:space:]]+")))
}


unigram <- unigram[N!=1]
bigram <- bigram[N!=1]
trigram <- trigram[N!=1]
fourgram <- fourgram[N!=1]


## Prediction
predict <- function(phrase, k=3){
    # phrase <<- parse(clean(input))
    # phrase <- unlist(strsplit(input, " "))
    j <- length(phrase)
    # print(c(phrase))
    if(j == 2){
        result <- c("bigram" = bigram()[phrase[j], nomatch=0][order(-N)]$word2[1:k], "unigram" =unigram()[order(-N)]$word[1:k])
    }else if (j == 3){
        result <- c("trigram" = trigram()[J(phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word3[1:k],
               "bigram" = bigram()[phrase[j], nomatch=0][order(-N)]$word2[1:k], "unigram" =unigram()[order(-N)]$word[1:k])
    }else {
        result <- c("fourgram" =fourgram()[J(phrase[j-2], phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word4[1:k],
               "trigram" = trigram()[J(phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word3[1:k],
               "bigram" = bigram()[phrase[j], nomatch=0][order(-N)]$word2[1:k], "unigram" =unigram()[order(-N)]$word[1:k])
    }
    return(result[!is.na(result)][1:k])
}

saveRDS(unigram, "unigram.Rds")
saveRDS(bigram, "bigram.Rds")
saveRDS(trigram, "trigram.Rds")
saveRDS(fourgram, "fourgram.Rds")

unigram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//unigram.Rds")
bigram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//bigram.Rds")
trigram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//trigram.Rds")
fourgram <- readRDS("/Volumes//Documents, Files, & Media/Coursera/finalngrams//fourgram.Rds")

tokens <- unlist(stri_split(wrap(test.set), regex = "[[:space:]]+", omit_empty = T))

table <- paste(c("NA", "NA", "NA", tokens), c("NA", "NA", tokens, "NA"), c("NA", tokens, "NA", "NA"), c(tokens, "NA", "NA", "NA"))[4:length(tokens)]
table <- data.table(t(sapply(stri_split(table, fixed = " "), c)))
table <- table[V1!="</s>"&V2!="<s>"&V2!="</s>"&V3!="<s>"&V3!="</s>"&V4!="<s>"&V4!="</s>"]


t(apply(data, 1, function(i) interpolate(unlist(i))))


l <- nrow(p)
nloptr( x0=c(0.25, 0.25, 0.25, 0.25),
        eval_f=function(lambda) -sum(log(apply(matrix(rep(lambda,), l, 4, byrow = T)*p[1:l, ], 1, sum))),
        eval_grad_f= function(lambda){
            c(-sum(p[1:l, 1]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)),
              -sum(p[1:l, 2]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)),
              -sum(p[1:l, 3]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)),
              -sum(p[1:l, 4]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)))},
        lb = c(0, 0, 0, 0),
        ub = c(1, 1, 1, 1),
        eval_g_eq = function(lambda) sum(lambda) - 1,
        eval_jac_g_eq = function(lambda) c(1, 1, 1, 1),
        opts = list("algorithm"="NLOPT_LD_AUGLAG",
                    "xtol_rel"=1.0e-8,
                    "local_opts"=local_opts))


nloptr( x0=c(0.25, 0.25, 0.25, 0.25),
        eval_f=function(lambda) -sum(log(apply(lambda*p[1], 1, sum))),
        eval_grad_f= function(lambda){
            c(-sum(p[1, 1]/apply(lambda*p[1], 1, sum)),
              -sum(p[1, 2]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)),
              -sum(p[1, 3]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)),
              -sum(p[1, 4]/apply(matrix(rep(lambda,l), l, 4, byrow = T)*p[1:l, ], 1, sum)))},
        lb = c(0, 0, 0, 0),
        ub = c(1, 1, 1, 1),
        eval_g_eq = function(lambda) sum(lambda) - 1,
        eval_jac_g_eq = function(lambda) c(1, 1, 1, 1),
        opts = list("algorithm"="NLOPT_LD_AUGLAG",
                    "xtol_rel"=1.0e-8,
                    "local_opts"=local_opts))

