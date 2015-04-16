
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny); library(data.table); library(stringi)
# source("milestone.R")
shinyServer(function(input, output, session) {

    unigram <- reactive({readRDS("unigram.Rds")})
    bigram <- reactive({readRDS("bigram.Rds")})
    trigram <- reactive({readRDS("trigram.Rds")})
    fourgram <- reactive({readRDS("fourgram.Rds")})




    parse <- function(string){
        return(unlist(stri_split(string, regex = "[[:space:]]+", omit_empty = T)))
        # return(unlist(strsplit(string, "[[:space:]]+")))
    }

    replaceUnknown <- function(parsedInput){
        unlist(lapply(parsedInput, function(i){
            if(!(i %in% unigram()$word))
                "<unk>"
            else
                i
        }))
    }

    ## Prediction
    predict <- function(phrase, k=3){
        # print(clean(phrase))
        phrase <- c("<s>", parse(clean(phrase)))
        phrase <- replaceUnknown(phrase)

        # phrase <- unlist(strsplit(input, " "))
        j <- length(phrase)
        # print(c("last", phrase[j]))
        if(j == 1){
            result <- c("bigram" = bigram()[phrase[j], nomatch=0][order(-N)]$word2[1:k],
                        "unigram" = unigram()[order(-N)]$word[2:(k+1)])
            result <- unlist(lapply(result, function(i) paste0(toupper(substring(i, 1, 1)), substring(i, 2))))
        }else if (j == 2){
            result <- c("trigram" = trigram()[J(phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word3[1:k],
                        "bigram" = bigram()[phrase[j], nomatch=0][order(-N)]$word2[1:k],
                        "unigram" = unigram()[order(-N)]$word[2:(k+1)])
        }else {
            result <- c("fourgram" =fourgram()[J(phrase[j-2], phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word4[1:k],
                        "trigram" = trigram()[J(phrase[j-1], phrase[j]), nomatch=0][order(-N)]$word3[1:k],
                        "bigram" = bigram()[phrase[j], nomatch=0][order(-N)]$word2[1:k],
                        "unigram" = unigram()[order(-N)]$word[2:(k+1)])
        }
        print(result[!is.na(result)][1:k])
        return(result[!is.na(result)][1:k])
        # return(rbind(names(result), result))
    }

    output$prediction <- renderText({
        prediction <- predict(input$text, input$numPred)
        return(prediction)
#         input$text
    })

    output$autocomplete <- renderText({
        # print(unlist(stri_split(input$text, regex = "\\b", omit_empty = T)))
        last <- tail(unlist(stri_split(input$text, regex = "\\b", omit_empty = T)), n = 1)
        # print(c("last=", last))
        # print(c("last=", last, "phrase=", unlist(stri_split(clean(input$text), regex = "\\b", omit_empty=T))))
        if(length(last) == 0)
            suggestion <- ""
        else if (last == "" | last == " ")
            suggestion <- ""
            # else if (last == " ") return("")
        else
            suggestion <- unigram()[order(-N)][, head(grep(paste0("^", last), word, value = T), n = input$numPred)]
#         if(last %in% suggestion) return("") else
        return(suggestion)
    })

    output$test <- renderText({
        input$test
        #         input$text
    })
})
