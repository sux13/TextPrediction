DSS Capstone Project: Text Prediction
========================================================
author: Xing Su
date: April 26, 2015
css: presentation.css

Data
========================================================

- For this project, we used the *English* files from the **HC Corpora** dataset, which contains ~4M lines of scraped text from news articles, Twitter, and blogs
- ***Regular Expression*** was used to clean and tokenize the raw text in the following manner
    - isolated and replaced items such as `<emoticon>`, `<phone>`, `<url>`, `<email>`, `<date>`, `<time>`, `<num>`, as well as `<profanity>`
    - expanded apostrophes like `don't`, `we'll`, `there's` to `don not`, `we will`, `there is` respectively
    - splitting text into multiple sentences
    - removing any extraneous punctuation and spaces


Exploratory Analysis & Data Structure
========================================================

- explored with `tau` and `tm` packages to tokenize data and build ngrams, however, ultimately decided to ***build from scratch*** to improve performance
- chose `data.table` (fast retrieval) to store the 4-gram Markov model
    + built Ngram tables by dividing the tokenized data into groups of 1/2/3/4 words and tabulating their frequency using the `table` function
    + `<s>` and `</s>` tags were added to each sentence to mark beginning/end
    + the end result is stored as `data.tables` with each column corresponding to a word and `N` column as the frequency of the word/group of words
- randomly sampled ~10% of all three of the blogs, news, and twitter dataset to use to build the algorithm
- experimented with removing stopwords ("a", "the", "or"), but lost quite a bit of accuracy and context and ***decided to keep them***
- vocabulary is limited to around ***80,000 words*** and the rest are replaced with the token `<unk>` and the model is trained as such


Prediction Algorithm & Adjustment
========================================================
- the model cleans/tokenizes the input first, and are fed through two algorithms
    + **Autocomplete**: take the last group of letters from the input, look up words in the `unigram` table that start with the letters, and return the result with the highest frequency
    + **Prediction**: look up the last 3 words in `fourgram` table, last 2 words in the `trigram`, and last word in `bigram` table, respectively in that order, for the most likely prediction; if nothing is found, the top word in `unigram` table is returned
+ tried **interpolation** of all four N-gram tables (see model below) using non-linear optimization (`nloptr` package) but resulted in ~30% increase in computation time and 8% drop in accuracy

<span style = "font-size: medium;">$$P_{interpolated} = 0.1155\frac{Count(w_{i−3},w_{i−2},w_{i−1},w_i)}{Count(w_{i−2},w_{i−1},w_i)} + 0.2364\frac{Count(w_{i−2},w_{i−1},w_i)}{Count(w_{i−1},w_i)} + 0.3757\frac{Count(w_{i−1},w_i)}{Count(w_i)} + 0.2724\frac{Count(w_i)}{Total~Words}$$</span>

- tried **Kneser-Ney Smoothing** but resulted in ~10% increase in computation time and approximately the same accuracy


Shinyapp & Instructions
========================================================
- the final product can be found [here](https://sxing.shinyapps.io/shinyapp/)
    1. type/paste a phrase onto the line provided (best if less than line width)
    2. autocomplete hints will appear in <strong style = "color: #d16527;">orange</strong> $\rightarrow$  works best with typing
    3. predicted word will appear in <strong style = "color: #176de3;">blue</strong> $\rightarrow$ "I" will appear initially as it is the most common way to begin a sentence
        - if <em style = "color: #AAA;">grey</em> text appears after your cursor, you can press the `tab` or <strong style = "color: #d16527;">$\rightarrow$</strong> to autocomplete the word
    4. there's an option to show the <em style = "color: #000;">Details</em> panel
        - displays tokenized input phrase
        - includes option to predict multiple words
+ <strong style = "color: #666;">Note:</strong> alternative strategies, exploratory code, and experimental analysis can all be found in this [repository](https://github.com/sux13/TextPrediction)

