library(shiny); library(data.table); library(stringi)
shinyServer(function(input, output, session) {
    observe({

        unigram <- reactive({readRDS("unigram.Rds")})
        bigram <- reactive({readRDS("bigram.Rds")})
        trigram <- reactive({readRDS("trigram.Rds")})
        fourgram <- reactive({readRDS("fourgram.Rds")})

        # clean 
        clean <- function(corpus){
            # replace any character that is not alphanumeric or symbols
            cleaned <- stri_replace_all(tolower(corpus), regex = "[^ a-zA-Z0-9!',\\-./:;<?@()]+|\"", replacement = "")
            cleaned <- stri_replace_all(cleaned, regex = "'ve\\b", replacement = " have")
            cleaned <- stri_replace_all(cleaned, regex = "'re\\b", replacement = " are")
            cleaned <- stri_replace_all(cleaned, regex = "n't\\b", replacement = " not")
            cleaned <- stri_replace_all(cleaned, regex = "'ll\\b", replacement = " will")
            cleaned <- stri_replace_all(cleaned, regex = "'d\\b", replacement = " would")
            cleaned <- stri_replace_all(cleaned, regex = "\\b(i'm|im)\\b", replacement = "i am")
            cleaned <- stri_replace_all(cleaned, regex = "\\b(he|she|it|how|that|there|what|when|who|why|where)'?s\\b", replacement = "$1 is")
            cleaned <- stri_replace_all(cleaned, regex = "\\bu\\b", replacement = "you")
            cleaned <- stri_replace_all(cleaned, regex = "\\bur\\b", replacement = "your")
            cleaned <- stri_replace_all(cleaned, regex = "\\b(\\w*f+u+c+k+\\w*|\\w*b+i+t+c+h+\\w*|\\w*s+h+i+t+\\w*|\\w*c+u+n+t+\\w*|\\w*f+a+g+g+o+t\\w*|t+w+a+t+|\\w*a+s+s+h+o+l+e+\\w*|\\w*n+i+g+g+e+r+\\w*)\\b", replacement = " <profanity> ")
            # find and replace email data
            cleaned <- stri_replace_all(cleaned, regex = " ?([\\w_.-]+)@([\\w.-]+)\\.([\\w.]{2,6}) ?", replacement = " <email> ")
            # find and replace url
            cleaned <- stri_replace_all(cleaned, regex =" ?(https?:/+)?([\\w]+[\\.]){1,4}[a-z]{2,4}[/\\w-!=?@_\\d]* ?", replacement = " <url> ", simplify = T, omit_no_match = T)
            # find and replace emoji data
            cleaned <- stri_replace_all(cleaned, regex = "[<>0O%]?[:;=8]([-o*']+)?[()dbp/ocs]+ | [()dbp/}{#|c]+([-o*']+)?[:;=8][<>]?|<+3+|<+/+3+|[-o0><^][_.]+[-o0><^]|([<>0O%]?[:;=8]([-o*']+)?[()dbp/ocs]+|[()dbp/}{#|c]+([-o*']+)?[:;=8][<>]?|<+3+|<+/+3+|[-o0><^][_.]+[-o0><^])$", replacement = " <emoticon> ")
            # find and replace date data
            cleaned <- stri_replace_all(cleaned, regex = " ([0-1][1-2]|[1-9])[/]([0-2][0-9]|3[01]|[1-9])([/]([0-9]{4}|[0-9]{2}))?|[1-9]0s|[0-9]{4}s ", replacement = " <date> ")
            # find and replace time data
            cleaned <- stri_replace_all(cleaned, regex ="\\b(([0-1]?[0-9]|2[1-4])([:][0-5]?[0-9]){1,2} ?([ap][. ]?[m][. ]?)? ?)|\\b([0-1]?[0-9]|2[1-4])(-\\d+)? ?[ap][. ]?[m][. ]?\\b", replacement =" <time> ")
            # find and replace phone data
            cleaned <- stri_replace_all(cleaned, regex ="\\b[ :-]?1?[-(.]*[\\d]{3}[-.)]* ?[\\d]{3}[-.]? ?[\\d]{4}\\b ?", replacement= " <phone> ")
            # remove extraneous punctuation
            cleaned <- stri_replace_all(cleaned, regex = "(?<=[!/,.:;?#])[!,.:;#?]+|'+|<+[^a-z0-9]", replacement = "")
            # break lines into sentences
            cleaned <- unlist(stri_split(cleaned, regex = " ?[!?;:.]+ +", omit_empty=T))
            # replace all extraneous 
            cleaned <- stri_replace_all(cleaned, regex = "[^ a-zA-Z0-9<>#]+", replacement = " ")
            # find and replace number data
            cleaned <- stri_replace_all(cleaned, regex ="\\b[\\d]+([-,.\\d]+)?(th|st|nd|rd)?", replacement= " <num> ")
            # remove leading/trailing/extraneou space nad apostrophe
            cleaned <- stri_replace_all(cleaned, regex = "^ +|(?<= ) +| +$", replacement = "")
            # drop empty elements
            cleaned <- cleaned[lapply(cleaned, nchar)>0]
            # add start and stop
            return(cleaned)
        }

        # split sentences into words
        parse <- function(string){
            return(unlist(stri_split(string, regex = "[[:space:]]+", omit_empty = T)))
        }

        # replace unseen vocabulary with <unk>
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
            phrase <- c("<s>", parse(clean(phrase)))
            phrase <- replaceUnknown(phrase)
            j <- length(phrase)
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
            return(unique(result[!is.na(result)])[1:k])
        }

        output$prediction <- renderText({
            prediction <- predict(input$text, input$numPred)
            return(prediction)
        })

        output$autocomplete <- renderText({
            last <- tail(unlist(stri_split(input$text, regex = "\\b", omit_empty = T)), n = 1)
            if(length(last) == 0)
                suggestion <- ""
            else if (last == "" | last == " ")
                suggestion <- ""
            else
                suggestion <- unigram()[order(-N)][, head(grep(paste0("^", last), word, value = T), n = 1)]
            session$sendCustomMessage(type = "myCallbackHandler", suggestion)
            return(suggestion)
        })

        output$parsed <- renderText({
            text <- c("<s>", parse(clean(input$text)))
            text <- replaceUnknown(text)
            text
        })
    })
})
