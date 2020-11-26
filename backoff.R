# https://rstudio-pubs-static.s3.amazonaws.com/271652_1525c0598da74774bfa4047803cee0d5.html
# https://rpubs.com/pferriere/dscapreport


library(tidyverse)
library(tidytext)

text <- c("SOS buy the book EOS", "SOS buy the book EOS",
                          "SOS buy the book EOS", "SOS buy the book EOS",   
                          "SOS sell the book EOS", "SOS buy the house EOS",  
                          "SOS buy the house EOS", "SOS paint the house EOS")


text_df <- tibble(line = 1:8, text = text)

text_df %>% 
  unnest_sentences(sentance, text, strip_punct = TRUE)

# 1.1 Create the unigram, bigram, trigram counts
unigram <- text_df %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) #%>% 
  #mutate(prop = n/sum(n),
  #       n_star = n - discount) 
  
bigram <- text_df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

trigram <- text_df %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3)%>% 
  count(trigram, sort = TRUE)%>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigram_fun <- function(pre_word_df, discount1, discount2){
  
  # 3 calculate probabilities of words completing observed trigrams
  
  # count of bigram word
  count_word <- bigram %>% 
    filter(word1 == pre_word_df$word1 & word2 == pre_word_df$word2)
  bigram_count <- count_word$n
  
  # create the observed trigram table
  obs_tri <- trigram %>% 
    filter(word1 == pre_word_df$word1 & word2 == pre_word_df$word2)  %>% 
    mutate(prob = (n - discount1) / bigram_count)
  
  # 4 calculate probabilities of words completing unobserved trigrams
  
  # 4.1 Find the words that complete the unobserved trigrams
  obs_tri_tails <- obs_tri$word3 # grabs the last word
  unobs_tri_tails <- unigram[!(unigram$word %in% obs_tri_tails),]$word
  
  
  # 4.2 Calculate discounted probability mass at the bigram level, alpha1
  
  pre_word2 <- pre_word_df$word2 # second word of the bigram prefix
  pre_word2_df <- unigram[unigram$word == pre_word2,] # single row of word and its count
  
  pre_word_bigram <- bigram[bigram$word1 == pre_word2,] # get all bigrams that start with the pre-word (unigram)
  
  # calcualte the alpha value
  alpha_bi <- 0
  if (nrow(pre_word_bigram) > 0){
    alpha_bi <- 1 - (sum(pre_word_bigram$n - discount1) / pre_word2_df$n)
  }
  
  # 4.3 Calculate the back off probability for bigrams
  
  pre_word2 # second word of the bigram prefix
  unobs_tri_tails # the words that complete the unobserved trigrams
  
  bo_bigrams <- paste(pre_word2, unobs_tri_tails, sep = " ") # get back off bigrams
  
  # unite the bigrams so there is one bigram column
  bigrams_united <- bigram %>% 
    unite(bigram, word1, word2, sep = " ")
  # get the observed back off bigrams
  obs_bo_bigrams <- bigram[bigrams_united$bigram %in% bo_bigrams, ]
  
  # unite the obs_bo_bigrams so there is one bigram column
  obs_bo_bigrams_united <- obs_bo_bigrams %>% 
    unite(bigram, word1, word2, sep = " ")
  
  # get the unobserved back off bigrams
  unobs_bo_bigrams <- bo_bigrams[!(bo_bigrams %in% obs_bo_bigrams_united$bigram)]
  
  # calculate the observed bigram probability
  first_word <- obs_bo_bigrams$word1
  first_word_n <- unigram[unigram$word %in% first_word, ]
  # calcualte the observed bigram probability
  prob_obs_bigram <- (obs_bo_bigrams$n - discount2) / first_word_n$n
  # create a table with bigram and prop
  prob_obs_bigram <- data.frame(bigram = obs_bo_bigrams_united$bigram, prob = prob_obs_bigram)
  
  # distribute discounted bigram probability mass to unobserved bigrams in proportion to unigram
  
  # get the unobserved bigram tails
  prob_unobs_bigram <- str_split_fixed(unobs_bo_bigrams, " ", 2)[ ,2]
  # make a table of unobserved tails
  prob_unobs_bigram <- unigram[unigram$word %in% prob_unobs_bigram, ]
  # convert counts to probabilities
  prob_unobs_bigram <- data.frame(bigram = unobs_bo_bigrams,
                                  prob = alpha_bi * prob_unobs_bigram$n / sum(prob_unobs_bigram$n))
  # add the prop observed bigram to data frame
  prob_bigram <- rbind(prob_obs_bigram, prob_unobs_bigram)
  
  
  #check 
  unobs <- prob_bigram[-1,] # take away the observed bigram
  sum(unobs$prob)
  
  # 4.4 Calculate discout probability mass at the trigram level
  
  ## Calculate the total probability mass discounted from all observed trigrams.
  ## This is the amount of probability mass which is
  ## redistributed to UNOBSERVED trigrams. If no trigrams starting with the bigram
  ## exist, alpha_tri = 1 
  
  big <- bigram %>% 
    filter(bigram$word1 == pre_word_df$word1 & bigram$word2 == pre_word_df$word2)
  
  
  alpha_tri <- 1
  if (nrow(obs_tri) > 0){
    alpha_tri <- 1 - (sum(obs_tri$n - discount2) / big$n[1])
  }
  
  # 4.5 Calculate unobserved trigram probabilities
  
  prob_bigram <- prob_bigram %>% 
    arrange(desc(prob_bigram$prob)) # sort the table
  
  sum_prob_bo_bigrams <- sum(prob_bigram$prob)
  first_pre_word <- pre_word_df$word1
  unobs_tri <- paste(first_pre_word, prob_bigram$bigram, sep = " ")
  prob_unobs_tri <- alpha_tri * prob_bigram$prob / sum_prob_bo_bigrams
  unobs_tri_df <- data.frame(trigram = unobs_tri, prob = prob_unobs_tri)
  
  # 5 Select word with the highest probability
  # unite this and rbind with prob_unobs_tri
  obs_tri 
  obs_tri_df <- obs_tri %>% 
    unite(trigram, word1, word2, word3, sep = " ") %>% 
    select(trigram, prob)
  
  prob_trigram <- rbind(obs_tri_df, unobs_tri_df)
  prob_trigram
}

# 1.2 Select bigram and trigram discounts
discount1 <- 0.5
discount2 <- 0.5
# 2 Select bigram prefix of word to be predicted

phrase_input <- "the book" 
phrase_input <- str_trim(phrase_input, side = "both") #trim white spaces on both sides

if (str_count(phrase_input, "\\w+") <= 1){
  phrase_input <- paste0("notAword", " ", phrase_input)
}

pre_words <- word(phrase_input, -2, -1) # grab trhe last two word in the input phrase



#create tibble for word input
pre_word_df <- tibble(line = 1, text = pre_words)%>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")



top_amount <- 10
# is the last word in the unigram model?
if (sum(unigram$word %>% str_detect(pre_word_df$word2)) > 0){ 
result <- trigram_fun(pre_word_df, discount1, discount2) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  arrange(desc(prob)) %>% 
  select(word3, prob) %>% 
  rename(word = word3) %>% 
  filter(word != "eos", word != "sos") %>% 
  head(top_amount)
}else{
  result <- unigram %>% 
    filter(word != "eos", word != "sos") %>% 
    mutate(prob = n / sum(n)) %>% 
    select(word, prob) %>% 
    arrange(desc(prob)) %>% 
    head(top_amount)
}
result

